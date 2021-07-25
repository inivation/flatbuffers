// Features we do support:
// - scoped_enums (only supported enums)
// -X include_dependence_headers
// - mutable_buffer
// - generate_name_strings
// - generate_object_based_api
// - gen_compare
// - cpp_object_api_pointer_type
// - cpp_object_api_string_type
// - cpp_object_api_string_flexible_constructor
// - cpp_object_api_vector_type
// - cpp_object_api_field_case_style
// - cpp_direct_copy
// - gen_nullable
// -X object_prefix
// -X object_suffix
// -X flatbuffer_prefix
// -X flatbuffer_suffix
// -X include_prefix
// -X keep_include_path
// -X cpp_includes
// -X cpp_std
// - cpp_static_reflection
// - set_empty_strings_to_null
// - set_empty_vectors_to_null
// - attribute 'bit_flags'
// - attribute 'cpp_ptr_type'
// - attribute 'cpp_ptr_type_get'
// - attribute 'cpp_str_type'
// - attribute 'cpp_str_flex_ctor'
// - attribute 'cpp_vec_type'
// - attribute 'required'
// - attribute 'deprecated'
// - attribute 'key'
// - attribute 'native_inline'
// - attribute 'native_default'
// - attribute 'native_custom_alloc'
// - attribute 'native_type'
// - attribute 'native_type_pack_name'
// -X attribute 'original_order' (in parser)
//
// Currently not supported features:
// - prefixed_enums (only scoped enums)
// - Flexbuffers
// - Mini-Reflection
// - hash-based references, attributes 'hash' and 'cpp_type'
// - attribute 'id'
// - attribute 'nested_flatbuffer'
// - attribute 'force_align'
// - attribute 'shared'

// FBS file parsing:
// An enum is made up of constant values, doesn't reference anything else.
// A union can reference any struct or table, defined before or after the union.
// A struct can only use scalars, fixed-size arrays, enums and other structs.
// The enums and structs must have been defined before it.
// A table can use scalars, fixed-size arrays, enums, unions, structs and other
// tables. The enums and unions must have been defined before it, while the
// structs and tables can be defined at any point. The resulting optimal order
// for generating seems to be:
// - namespaces in order of declaration
// - per namespace: enum, struct, union, table
// - structs and tables must be forward declared for union use

#define FMT_HEADER_ONLY 1

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/flatc.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"

#include <array>
#include <fmt/format.h>
#include <iostream>
#include <type_traits>

#define FMT_INDENT(STR) "{:{}}" STR "\n", "", indent

static constexpr const char *NAMESPACE_SEP{"::"};

static constexpr std::array<const char *, 18> TYPE_NAMES = {{
	"NONE",
	"UTYPE",
	"BOOL",
	"CHAR",
	"UCHAR",
	"SHORT",
	"USHORT",
	"INT",
	"UINT",
	"LONG",
	"ULONG",
	"FLOAT",
	"DOUBLE",
	"STRING",
	"VECTOR",
	"STRUCT",
	"UNION",
	"ARRAY",
}};

namespace flatbuffers {
namespace cppng {

// We only support C++11 and newer in this generator.
enum class CppStandard {
	CPP_11 = 11,
	CPP_17 = 17,
	CPP_20 = 20,
};

// Define a style of 'struct' constructor if it has 'Array' fields.
enum class GenArrayArgMode {
	None,       // don't generate initialization args
	SpanStatic, // generate flatbuffers::span<T,N>
};

template<typename Enumeration, typename std::enable_if<std::is_enum<Enumeration>::value, bool>::type = true>
constexpr typename std::underlying_type<Enumeration>::type EnumAsInteger(const Enumeration value) noexcept {
	return static_cast<typename std::underlying_type<Enumeration>::type>(value);
}

// Extension of IDLOptions for cpp-generator.
struct IDLOptionsCppNG : public IDLOptions {
	// All fields start with 'g_' prefix to distinguish from the base IDLOptions.
	CppStandard mCppStandard; // Base version of C++ standard in parsed enum format.

	IDLOptionsCppNG(const IDLOptions &opts) : IDLOptions(opts) {
		auto cppStandard = opts.cpp_std;

		// Discard C++ part, regardless of case or spelling (cpp, ...).
		// If not present, we just assume the whole string is just the
		// standard expressed as a number.
		if (cppStandard.size() > 3) {
			cppStandard.erase(0, 3);
		}

		// Convert to number.
		auto cppStandardNumeric = 11;

		try {
			cppStandardNumeric = std::stoi(cppStandard);
		}
		catch (const std::invalid_argument &) {
			if (!cppStandard.empty()) {
				LogCompilerWarn("Could not parse given C++ standard, defaulting to C++ 11.");
			}
		}

		if ((cppStandardNumeric == 11) || (cppStandardNumeric == 14)) {
			mCppStandard = CppStandard::CPP_11;
		}
		else if (cppStandardNumeric == 17) {
			mCppStandard = CppStandard::CPP_17;
		}
		else if (cppStandardNumeric == 20) {
			mCppStandard = CppStandard::CPP_20;
		}
		else {
			throw std::invalid_argument("Unknown value of the '--cpp-std' switch: " + opts.cpp_std);
		}

		if (cpp_static_reflection && (EnumAsInteger(mCppStandard) < EnumAsInteger(CppStandard::CPP_17))) {
			throw std::out_of_range("--cpp-static-reflection requires using --cpp-std 17 or higher.");
		}

		// Features support: only scoped enums.
		if (prefixed_enums || !scoped_enums) {
			throw std::invalid_argument("Generator supports only C++ scoped enums, must pass "
										"--scoped-enums.");
		}
	}
};

class CppGeneratorNG : public BaseGenerator {
public:
	CppGeneratorNG(
		const Parser &parser, const std::string &path, const std::string &fileName, const IDLOptionsCppNG opts) :
		BaseGenerator(parser, path, fileName, "", NAMESPACE_SEP, "hpp"), mOptions(opts) {
		// Build list of namespaces.
		// Empty namespace will be followed by all others in same order as declared.
		for (const auto *ns : parser_.namespaces_) {
			CppNamespace cppns{ns};

			// Each namespace can have associated structure definitions (structs/tables)
			// and enum definitions (enum/union).
			for (const auto *structDef : parser_.structs_.vec) {
				// Skip structs from other flatbuffer files.
				if (structDef->generated) {
					continue;
				}

				// Skip structs with a different namespace.
				if (getNamespaceName(structDef->defined_namespace) != cppns.mFullName) {
					continue;
				}

				if (structDef->fixed) {
					// A struct of fixed size.
					cppns.mStructs.emplace_back(structDef->name, structDef);
				}
				else {
					// A flexible table.
					cppns.mTables.emplace_back(structDef->name, structDef);
				}
			}

			for (const auto *enumDef : parser_.enums_.vec) {
				// Skip enums from other flatbuffer files.
				if (enumDef->generated) {
					continue;
				}

				// Skip enums with a different namespace.
				if (getNamespaceName(enumDef->defined_namespace) != cppns.mFullName) {
					continue;
				}

				if (enumDef->is_union) {
					// Union type.
					cppns.mUnions.emplace_back(enumDef->name, enumDef);
				}
				else {
					// Enum type.
					cppns.mEnums.emplace_back(enumDef->name, enumDef);
				}
			}

			// If namespace has no content, we can discard it. It probably
			// came from an included file, which we don't need to repeat here.
			if (!cppns.mStructs.empty() || !cppns.mTables.empty() || !cppns.mUnions.empty() || !cppns.mEnums.empty()) {
				mNamespaces.push_back(std::move(cppns));
			}
		}
	}

	bool generate() override {
		// Debug print everything.
		traverse();

		// Start code generation.
		mCode.clear();
		mCode += fmt::format("// {}\n\n", FlatBuffersGeneratedWarning());

		// First thing: the include guard for this generated header file.
		const auto filePath = GeneratedFileName(path_, file_name_, mOptions);
		mCode += includeGuardOpenClose(filePath, true);

		// Mark file content as system_header to avoid warnings.
		// mCode += "#pragma GCC system_header\n";
		// mCode += "#pragma clang system_header\n";

		// Add includes.
		generateIncludes();

		// Generate forward declarations for tables and structs first.
		// These are needed for referencing in unions, other structs and tables.
		generateForwardDeclarations();

		// Last thing: close the include guard for this generated header file.
		mCode += includeGuardOpenClose(filePath, false);

		// Save the file and optionally generate the binary schema code.
		return SaveFile(filePath.c_str(), mCode, false);
	}

private:
	// DEBUG PRINT START
	static std::string getNamespaceForPrint(const Namespace *ns) {
		if (ns == nullptr) {
			return "(undefined)";
		}

		const auto namespaceName
			= fmt::format("{}", fmt::join(ns->components.cbegin(), ns->components.cend(), NAMESPACE_SEP));

		if (namespaceName.empty()) {
			return "(empty)";
		}

		return namespaceName;
	}

	static void printValue(const Value *val, const size_t indent) {
		fmt::print(FMT_INDENT("==VALUE=="));
		if (val == nullptr) {
			fmt::print(FMT_INDENT("none\n"));
			return;
		}

		fmt::print(FMT_INDENT("Value addr: {}"), static_cast<const void *>(val));
		fmt::print(FMT_INDENT("Constant: {}"), val->constant);
		fmt::print(FMT_INDENT("Offset: {}"), val->offset);
		printType(&val->type, indent);
		fmt::print("\n");
	}

	static void printDefinition(const Definition *def, size_t indent) {
		fmt::print(FMT_INDENT("Name: {}"), def->name);
		fmt::print(FMT_INDENT("File: {}"), def->file);
		fmt::print(
			FMT_INDENT("Doc comment: {}"), fmt::join(def->doc_comment.cbegin(), def->doc_comment.cend(), " -NL- "));
		fmt::print(FMT_INDENT("Code generated: {}"), def->generated);
		fmt::print(FMT_INDENT("Namespace: {}"), getNamespaceForPrint(def->defined_namespace));

		fmt::print(FMT_INDENT("Attributes:"));
		indent += 4;
		for (const auto &attr : def->attributes.dict) {
			fmt::print(FMT_INDENT("Name: {}"), attr.first);
			printValue(attr.second, indent);
		}
	}

	static void printFieldDef(const FieldDef *fieldDef, const size_t indent) {
		fmt::print(FMT_INDENT("==FIELD DEFINITION=="));
		if (fieldDef == nullptr) {
			fmt::print(FMT_INDENT("none\n"));
			return;
		}

		printDefinition(fieldDef, indent);

		fmt::print(FMT_INDENT("Deprecated: {}"), fieldDef->deprecated);
		fmt::print(FMT_INDENT("Is search key: {}"), fieldDef->key);
		fmt::print(FMT_INDENT("Use shared strings: {}"), fieldDef->shared);
		fmt::print(FMT_INDENT("Native inline: {}"), fieldDef->native_inline);
		fmt::print(FMT_INDENT("Presence: {}"),
			(fieldDef->presence == FieldDef::Presence::kRequired)
				? ("required")
				: ((fieldDef->presence == FieldDef::Presence::kOptional) ? ("optional") : ("default")));
		fmt::print(FMT_INDENT("Flexbuffer: {}"), fieldDef->flexbuffer);
		fmt::print(FMT_INDENT("Padding: {}"), fieldDef->padding);
		printValue(&fieldDef->value, indent);
		fmt::print("\n");
	}

	static void printStructDef(const StructDef *structDef, const size_t indent) {
		fmt::print(FMT_INDENT("==STRUCT DEFINITION=="));
		if (structDef == nullptr) {
			fmt::print(FMT_INDENT("none\n"));
			return;
		}

		printDefinition(structDef, indent);

		fmt::print(FMT_INDENT("Struct (fixed size): {} (size = {})"), structDef->fixed, structDef->bytesize);
		fmt::print(FMT_INDENT("Pre-declared: {}"), structDef->predecl);
		fmt::print(FMT_INDENT("Fields sorted by size: {}"), structDef->sortbysize);
		fmt::print(FMT_INDENT("Has key field: {}"), structDef->has_key);
		fmt::print(FMT_INDENT("Minimum alignment: {}"), structDef->minalign);

		fmt::print(FMT_INDENT("Fields:"));
		for (const auto *field : structDef->fields.vec) {
			printFieldDef(field, indent + 4);
		}
		fmt::print("\n");
	}

	static void printEnumVal(const EnumVal *enumVal, const size_t indent) {
		fmt::print(FMT_INDENT("==ENUM VALUE=="));
		if (enumVal == nullptr) {
			fmt::print(FMT_INDENT("none\n"));
			return;
		}

		fmt::print(FMT_INDENT("EnumVal addr: {}"), static_cast<const void *>(enumVal));
		fmt::print(FMT_INDENT("Name: {}"), enumVal->name);
		fmt::print(FMT_INDENT("Value: {}"), enumVal->GetAsInt64());
		fmt::print(FMT_INDENT("Doc comment: {}"),
			fmt::join(enumVal->doc_comment.cbegin(), enumVal->doc_comment.cend(), " -NL- "));
		fmt::print(FMT_INDENT("Union type:"));
		printType(&enumVal->union_type, indent + 4);
		fmt::print("\n");
	}

	static void printEnumDef(const EnumDef *enumDef, const size_t indent) {
		fmt::print(FMT_INDENT("==ENUM DEFINITION=="));
		if (enumDef == nullptr) {
			fmt::print(FMT_INDENT("none\n"));
			return;
		}

		printDefinition(enumDef, indent);

		fmt::print(FMT_INDENT("Size: {}"), enumDef->size());
		fmt::print(FMT_INDENT("Values:"));
		for (const auto *val : enumDef->Vals()) {
			printEnumVal(val, indent + 4);
		}
		fmt::print(FMT_INDENT("Minimum value:"));
		printEnumVal(enumDef->MinValue(), indent + 4);
		fmt::print(FMT_INDENT("Maximum value:"));
		printEnumVal(enumDef->MaxValue(), indent + 4);
		fmt::print(FMT_INDENT("Union: {}"), enumDef->is_union);
		fmt::print(FMT_INDENT("Union with type aliases: {}"), enumDef->uses_multiple_type_instances);
		fmt::print(FMT_INDENT("Underlying type:"));
		printType(&enumDef->underlying_type, indent + 4, false);
		fmt::print("\n");
	}

	static void printType(const Type *type, const size_t indent, const bool recurse = true) {
		fmt::print(FMT_INDENT("==TYPE=="));
		if (type == nullptr) {
			fmt::print(FMT_INDENT("none\n"));
			return;
		}

		fmt::print(FMT_INDENT("Type addr: {}"), static_cast<const void *>(type));
		fmt::print(FMT_INDENT("Type name: {}"), TYPE_NAMES[type->base_type]);
		fmt::print(FMT_INDENT("Element type name: {}"), TYPE_NAMES[type->element]);
		fmt::print(FMT_INDENT("Fixed array length: {}"), type->fixed_length);

		if (!recurse) {
			fmt::print("\n");
			return;
		}

		fmt::print(FMT_INDENT("Struct definition:"));
		printStructDef(type->struct_def, indent + 4);
		fmt::print(FMT_INDENT("Enum definition:"));
		printEnumDef(type->enum_def, indent + 4);
		fmt::print("\n");
	}

	void traverse() {
		const size_t indent = 4;

		fmt::print("Types:\n");
		for (const auto &t : parser_.types_.vec) {
			printType(t, indent);
			fmt::print("\n");
		}

		fmt::print("Structs:\n");
		for (const auto &s : parser_.structs_.vec) {
			printStructDef(s, indent);
			fmt::print("\n");
		}

		fmt::print("Enums:\n");
		for (const auto &e : parser_.enums_.vec) {
			printEnumDef(e, indent);
			fmt::print("\n");
		}

		fmt::print("Root struct definition:\n");
		const auto rootStruct = parser_.root_struct_def_;
		printStructDef(rootStruct, indent);
		fmt::print("\n");

		fmt::print("Namespaces:\n");
		for (const auto &ns : parser_.namespaces_) {
			fmt::print(FMT_INDENT("{}"), getNamespaceForPrint(ns));
		}
		fmt::print("\n");

		fmt::print("File identifier: {}\n", parser_.file_identifier_);
		fmt::print("File extension: {}\n", parser_.file_extension_);
	}

	// DEBUG PRINT END

	struct CppNamespace {
		std::string mFullName;
		std::string mName;
		std::vector<std::string> mNameComponents;
		std::vector<std::pair<std::string, const StructDef *>> mStructs;
		std::vector<std::pair<std::string, const StructDef *>> mTables;
		std::vector<std::pair<std::string, const EnumDef *>> mEnums;
		std::vector<std::pair<std::string, const EnumDef *>> mUnions;

		CppNamespace(const Namespace *ns) {
			assert(ns != nullptr);

			mFullName       = getNamespaceName(ns);
			mNameComponents = ns->components;

			if (!mNameComponents.empty()) {
				mName = mNameComponents.back();
			}
		}

		bool operator==(const CppNamespace &rhs) const noexcept {
			return (mFullName == rhs.mFullName);
		}

		bool operator!=(const CppNamespace &rhs) const noexcept {
			return (!operator==(rhs));
		}

		bool operator==(const std::string &rhs) const noexcept {
			return (mFullName == rhs);
		}

		bool operator!=(const std::string &rhs) const noexcept {
			return (!operator==(rhs));
		}
	};

	static std::string getNamespaceName(const Namespace *ns) {
		assert(ns != nullptr);
		return fmt::format("{}", fmt::join(ns->components.cbegin(), ns->components.cend(), NAMESPACE_SEP));
	}

	const IDLOptionsCppNG mOptions;
	std::vector<CppNamespace> mNamespaces;
	std::string mCode;

	void generateIncludes() {
		mCode += "// Header includes\n";
		mCode += "#include \"flatbuffers/flatbuffers.h\"\n";

		if (mOptions.include_dependence_headers) {
			mCode += includeDependencies();
		}

		mCode += includeExtra();
		mCode += '\n';
	}

	void generateForwardDeclarations() {
		mCode += "// Forward declarations\n";

		// Go through namespaces in order (see top comments about traversal order).
		for (const auto &ns : mNamespaces) {
			if (ns.mStructs.empty() && ns.mTables.empty()) {
				// No structs or tables present, skip this namespace.
				continue;
			}

			mCode += namespaceOpenClose(ns, true);

			for (const auto &st : ns.mStructs) {
				mCode += fmt::format("struct {};\n", className(st.second));
			}
			for (const auto &st : ns.mTables) {
				mCode += fmt::format("struct {};\n", className(st.second));
				mCode += fmt::format("struct {}Builder;\n", className(st.second));

				if (mOptions.generate_object_based_api) {
					mCode += fmt::format("struct {};\n", className(st.second, true));
				}
			}

			mCode += namespaceOpenClose(ns, false);
			mCode += '\n';
		}
	}

	std::string className(const StructDef *structDef, const bool objectAPI = false) {
		assert(structDef != nullptr);

		// Structs have the same definition and thus name for both
		// the normal flatbuffers API and the object API.
		if (structDef->fixed) {
			return structDef->name;
		}

		// Tables have different names, plus the option of adding
		// prefixes and suffixes to them.
		if (objectAPI) {
			return mOptions.object_prefix + structDef->name + mOptions.object_suffix;
		}
		else {
			return mOptions.flatbuffer_prefix + structDef->name + mOptions.flatbuffer_suffix;
		}
	}

	std::string fullyQualifiedClassName(const StructDef *structDef, const bool objectAPI = false) {
		assert(structDef != nullptr);
		return getNamespaceName(structDef->defined_namespace) + qualifying_separator_ + className(structDef, objectAPI);
	}

	std::string includeGuardOpenClose(std::string guard, const bool open) {
		// Posix file path components to _.
		std::transform(guard.begin(), guard.end(), guard.begin(), [](const char c) {
			return ((c == '.') || (c == '/')) ? ('_') : (c);
		});

		// Remove any non-alpha-numeric characters that may appear in a filename.
		guard.erase(std::remove_if(guard.begin(), guard.end(),
						[](const char c) {
							return (!is_alnum(c) && (c != '_'));
						}),
			guard.end());

		// By convention #define are all upper-case.
		std::transform(guard.begin(), guard.end(), guard.begin(), CharToUpper);

		return fmt::format((open) ? ("#ifndef FLATBUFFERS_GENERATED_{0}_\n"
									 "#define FLATBUFFERS_GENERATED_{0}_\n\n")
								  : ("#endif // FLATBUFFERS_GENERATED_{0}_\n"),
			guard);
	}

	std::string includeDependencies() {
		std::string includes;

		for (const auto &inc : parser_.included_files_) {
			if (inc.second.empty()) {
				continue;
			}

			const auto noext       = flatbuffers::StripExtension(inc.second);
			const auto basename    = flatbuffers::StripPath(noext);
			const auto includeName = GeneratedFileName(
				mOptions.include_prefix, (mOptions.keep_include_path) ? (noext) : (basename), mOptions);

			includes += fmt::format("#include \"{}\"\n", includeName);
		}

		if (mOptions.generate_object_based_api) {
			// Add native_includes if object API is enabled, as they're
			// meant for the native types used therein.
			for (const auto &inc : parser_.native_included_files_) {
				includes += fmt::format("#include \"{}\"\n", inc);
			}
		}

		return includes;
	}

	std::string includeExtra() {
		std::string includes;

		for (const auto &inc : mOptions.cpp_includes) {
			includes += fmt::format("#include \"{}\"\n", inc);
		}

		return includes;
	}

	std::string namespaceOpenClose(const CppNamespace &ns, const bool open) {
		// Global (empty) namespace is just nothing, else we'd create an
		// unnamed namespace and we don't want that.
		if (ns.mFullName.empty()) {
			return "\n";
		}

		if (mOptions.mCppStandard == CppStandard::CPP_11) {
			std::string nsString;
			for (const auto &component : ns.mNameComponents) {
				nsString += fmt::format((open) ? ("namespace {} {{\n") : ("}} // namespace {}\n"), component);
			}
			return nsString;
		}
		else {
			// Newer C++ can do nested namespaces directly.
			return fmt::format((open) ? ("namespace {} {{\n") : ("}} // namespace {}\n"), ns.mFullName);
		}
	}

	std::string comment(const std::vector<std::string> &docComment) {
		std::string comment;
		GenComment(docComment, &comment, nullptr, "");
		return comment;
	}
};

} // namespace cppng

bool GenerateCPPNG(const Parser &parser, const std::string &path, const std::string &fileName) {
	try {
		cppng::IDLOptionsCppNG opts(parser.opts);

		cppng::CppGeneratorNG generator(parser, path, fileName, opts);

		return generator.generate();
	}
	catch (const std::exception &ex) {
		LogCompilerError(ex.what());
		return false;
	}
}

} // namespace flatbuffers
