// Features we do support:
// -X scoped_enums (only supported enums)
// -X include_dependence_headers
// -? mutable_buffer
// -? generate_name_strings
// - generate_object_based_api
// - gen_compare
// - cpp_object_api_pointer_type
// - cpp_object_api_string_type
// - cpp_object_api_string_flexible_constructor
// - cpp_object_api_vector_type
// - cpp_object_api_field_case_style
// -? cpp_direct_copy
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
// -X attribute 'bit_flags'
// - attribute 'cpp_ptr_type'
// - attribute 'cpp_ptr_type_get'
// - attribute 'cpp_str_type'
// - attribute 'cpp_str_flex_ctor'
// - attribute 'cpp_vec_type'
// - attribute 'required'
// - attribute 'deprecated'
// - attribute 'native_inline'
// - attribute 'native_default'
// - attribute 'native_custom_alloc'
// - attribute 'native_type'
// - attribute 'native_type_pack_name'
// -X attribute 'original_order' (in parser)
//
// Currently not supported features:
// - prefixed_enums (only scoped enums)
// - gen_nullable
// - Flexbuffers
// - Mini-Reflection
// - hash-based references, attributes 'hash' and 'cpp_type'
// - attribute 'id'
// - attribute 'key'
// - attribute 'shared'
// - attribute 'nested_flatbuffer'
// - attribute 'force_align'

// FBS file parsing:
// An enum is made up of constant values, doesn't reference anything else.
// A union can reference any struct or table, defined before or after the union.
// A struct can only use scalars, enums, other structs and arrays thereof.
// The enums and structs must have been defined beforehand.
// A table can use scalars, enums, unions, strings, structs and other
// tables as well as vectors thereof.
// The enums and unions must have been defined before it, while the
// structs and tables can be defined at any point
// The resulting optimal order for generating seems to be:
// - namespaces in order of declaration
// - per namespace: enum (including union types), struct, table
// - structs and tables must be forward declared for later use

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

using namespace fmt::literals;

#define FMT_INDENT(STR) "{:{}}" STR "\n", "", indent

static constexpr const char *NAMESPACE_SEP{"::"};

template<typename Enumeration, typename std::enable_if<std::is_enum<Enumeration>::value, bool>::type = true>
constexpr typename std::underlying_type<Enumeration>::type EnumAsInteger(const Enumeration value) noexcept {
	return static_cast<typename std::underlying_type<Enumeration>::type>(value);
}

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

static constexpr std::array<const char *, 18> FLATBUFFERS_CPP_TYPES = {{
#define FLATBUFFERS_TD(ENUM, IDLTYPE, CTYPE, ...) #CTYPE,
	FLATBUFFERS_GEN_TYPES(FLATBUFFERS_TD)
#undef FLATBUFFERS_TD
}};

namespace flatbuffers {
namespace cppng {

// We only support C++11 and newer in this generator.
enum class CppStandard {
	CPP_11 = 11,
	CPP_17 = 17,
	CPP_20 = 20,
};

// Comparison operators.
inline bool operator==(const CppStandard lhs, const CppStandard rhs) {
	return (lhs == rhs);
}

inline bool operator<(const CppStandard lhs, const CppStandard rhs) {
	// Look at it as its underlying integer here.
	return (EnumAsInteger(lhs) < EnumAsInteger(rhs));
}

inline bool operator!=(const CppStandard lhs, const CppStandard rhs) {
	return (!operator==(lhs, rhs));
}

inline bool operator<=(const CppStandard lhs, const CppStandard rhs) {
	return (operator<(lhs, rhs) || operator==(lhs, rhs));
}

inline bool operator>(const CppStandard lhs, const CppStandard rhs) {
	return ((!operator<(lhs, rhs)) && (!operator==(lhs, rhs)));
}

inline bool operator>=(const CppStandard lhs, const CppStandard rhs) {
	return (!operator<(lhs, rhs));
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

		if (cpp_static_reflection && (mCppStandard < CppStandard::CPP_17)) {
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
					cppns.mStructs.push_back(structDef);
				}
				else {
					// A flexible table.
					cppns.mTables.push_back(structDef);
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
					cppns.mUnions.push_back(enumDef);
				}
				else {
					// Enum type.
					cppns.mEnums.push_back(enumDef);
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

		// Generate enums first of the main types, as they act like constants
		// that are re-used by the others.
		generateScopedEnums();

		// Generate structs next, these are fixed size, unchanging and all their
		// components must already be declared (see 'FBS file parsing' at top).
		generateFixedStructs();

		// And then the more complex tables, which can refer to tables fully
		// defined after them (not structs as we generate them all above).
		generateTables();

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
		// const auto rootStruct = parser_.root_struct_def_;
		// printStructDef(rootStruct, indent);

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
		std::vector<const StructDef *> mStructs;
		std::vector<const StructDef *> mTables;
		std::vector<const EnumDef *> mEnums;
		std::vector<const EnumDef *> mUnions;

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

			for (const auto *st : ns.mStructs) {
				mCode += fmt::format("struct {};\n", className(st));
			}
			for (const auto *tb : ns.mTables) {
				mCode += fmt::format("struct {};\n", className(tb));
				mCode += fmt::format("struct {}Builder;\n", className(tb));

				if (mOptions.generate_object_based_api) {
					mCode += fmt::format("struct {};\n", className(tb, true));
				}
			}

			mCode += namespaceOpenClose(ns, false);
			mCode += '\n';
		}
	}

	void generateScopedEnums() {
		mCode += "// Scoped enumerations (including union types)\n";

		// Go through namespaces in order (see top comments about traversal order).
		for (const auto &ns : mNamespaces) {
			if (ns.mEnums.empty() && ns.mUnions.empty()) {
				// No enumerations present, skip this namespace.
				continue;
			}

			mCode += namespaceOpenClose(ns, true);

			for (const auto *en : ns.mEnums) {
				mCode += scopedEnumeration(en);
			}
			for (const auto *un : ns.mUnions) {
				mCode += scopedEnumeration(un);
			}

			mCode += namespaceOpenClose(ns, false);
			mCode += '\n';
		}
	}

	void generateFixedStructs() {
		mCode += "// Fixed size structs\n";

		// Go through namespaces in order (see top comments about traversal order).
		for (const auto &ns : mNamespaces) {
			if (ns.mStructs.empty()) {
				// No fixed-size structs present, skip this namespace.
				continue;
			}

			mCode += namespaceOpenClose(ns, true);

			for (const auto *st : ns.mStructs) {
				mCode += fixedSizeStruct(st);
			}

			mCode += namespaceOpenClose(ns, false);
			mCode += '\n';
		}
	}

	void generateTables() {
		// TODO: implement.
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
			return "";
		}

		if (mOptions.mCppStandard < CppStandard::CPP_17) {
			std::string nsString;
			for (const auto &component : ns.mNameComponents) {
				nsString += fmt::format((open) ? ("namespace {} {{\n") : ("}} // namespace {}\n"), component);
			}
			return nsString;
		}
		else {
			// C++17 and newer can do nested namespaces directly.
			return fmt::format((open) ? ("namespace {} {{\n") : ("}} // namespace {}\n"), ns.mFullName);
		}
	}

	static std::string comment(const std::vector<std::string> &docComment) {
		std::string comment;
		GenComment(docComment, &comment, nullptr, "");
		return comment;
	}

	std::string stringType(const Definition *definition) {
		return attributeValue(definition, "cpp_str_type", BASE_TYPE_STRING, mOptions.cpp_object_api_string_type);
	}

	std::string vectorType(const Definition *definition) {
		return attributeValue(definition, "cpp_vec_type", BASE_TYPE_STRING, mOptions.cpp_object_api_vector_type);
	}

	std::string pointerType(const Definition *definition) {
		return attributeValue(definition, "cpp_ptr_type", BASE_TYPE_STRING, mOptions.cpp_object_api_pointer_type);
	}

	std::string pointerTypeGetter(const Definition *definition) {
		return attributeValue(definition, "cpp_ptr_type_get", BASE_TYPE_STRING, ".get()");
	}

	static bool attributeExists(
		const Definition *definition, const std::string &name, const BaseType type = BASE_TYPE_NONE) {
		if (definition == nullptr) {
			return false;
		}

		const auto result = definition->attributes.Lookup(name);
		if ((result == nullptr) || ((type != BASE_TYPE_NONE) && (result->type.base_type != type))) {
			return false;
		}

		return true;
	}

	static std::string attributeValue(const Definition *definition, const std::string &name, const BaseType type,
		const std::string &defaultOnFailure) {
		if (attributeExists(definition, name, type)) {
			return definition->attributes.Lookup(name)->constant;
		}

		return defaultOnFailure;
	}

	static std::string enumName(const EnumDef *enumDef) {
		assert(enumDef != nullptr);
		return enumDef->name;
	}

	static std::string fullyQualifiedEnumName(const EnumDef *enumDef) {
		assert(enumDef != nullptr);
		return getNamespaceName(enumDef->defined_namespace) + NAMESPACE_SEP + enumName(enumDef);
	}

	static std::string numericLiteral(const std::string &literal, const BaseType type) {
		switch (type) {
			case BASE_TYPE_UINT:
				return literal + "U";

			case BASE_TYPE_LONG:
				return literal + "LL";

			case BASE_TYPE_ULONG:
				return literal + "LLU";

			case BASE_TYPE_FLOAT:
				return literal + "F";

			default:
				return literal;
		}
	}

	std::string scopedEnumeration(const EnumDef *enumDef) {
		assert(enumDef != nullptr);

		// bit_flags attribute: each enum value is 2^N and can thus be used
		// directly as a flag for bitwise operations. MIN/MAX make no sense,
		// but we can declare additional operators with a Flatbuffer macro.
		const auto bitFlags = attributeExists(enumDef, "bit_flags");

		std::string enumeration = comment(enumDef->doc_comment);

		enumeration
			+= fmt::format("enum class {} : {} {{\n", enumName(enumDef), scalarTypeToString(enumDef->underlying_type));

		for (const auto *enumVal : enumDef->Vals()) {
			enumeration += fmt::format("{}{} = {},\n", comment(enumVal->doc_comment), enumVal->name,
				numericLiteral(enumDef->ToString(*enumVal), enumDef->underlying_type.base_type));
		}

		if (bitFlags) {
			// NONE/ANY enumerations for convenience (all bits off -> zero, all bits on -> sum).
			enumeration += "NONE = 0,\n";
			enumeration += fmt::format("ANY = {},\n", enumDef->AllFlags());
		}
		else {
			// MIN/MAX enumerations for convenience.
			enumeration += fmt::format("MIN = {},\n", enumDef->MinValue()->name);
			enumeration += fmt::format("MAX = {},\n", enumDef->MaxValue()->name);
		}

		enumeration += fmt::format("}}; // enum {}\n", enumName(enumDef));
		if (bitFlags) {
			enumeration += fmt::format("FLATBUFFERS_DEFINE_BITMASK_OPERATORS({}, {})\n",
				fullyQualifiedEnumName(enumDef), scalarTypeToString(enumDef->underlying_type));
		}
		enumeration += '\n';

		// Functions to introspect the enumeration: get values, names and name(E).
		std::string enumValues;
		for (const auto *enumVal : enumDef->Vals()) {
			enumValues += fmt::format("{}::{},\n", fullyQualifiedEnumName(enumDef), enumVal->name);
		}

		enumeration += fmt::format("constexpr std::array<{enumType}, {enumNumElements}> EnumValues{enumName}() {{\n"
								   "  constexpr std::array<{enumType}, {enumNumElements}> values = {{{{\n"
								   "    {enumValues}"
								   "  }}}};\n"
								   "  return values;\n"
								   "}}\n",
			"enumType"_a = fullyQualifiedEnumName(enumDef), "enumName"_a = enumName(enumDef),
			"enumNumElements"_a = enumDef->size(), "enumValues"_a = enumValues);
		enumeration += '\n';

		std::string enumNames;
		for (const auto *enumVal : enumDef->Vals()) {
			enumNames += fmt::format("\"{}\",\n", enumVal->name);
		}

		enumeration += fmt::format("constexpr std::array<{stringType}, {enumNumElements}> EnumNames{enumName}() {{\n"
								   "  constexpr std::array<{stringType}, {enumNumElements}> names = {{{{\n"
								   "    {enumNames}"
								   "  }}}};\n"
								   "  return names;\n"
								   "}}\n",
			"stringType"_a = constexprStringType(), "enumName"_a = enumName(enumDef),
			"enumNumElements"_a = enumDef->size(), "enumNames"_a = enumNames);
		enumeration += '\n';

		std::string nameLookup;
		size_t counter = 0;
		for (const auto *enumVal : enumDef->Vals()) {
			nameLookup += fmt::format("case {}::{}:\n"
									  "  return EnumNames{}()[{}];\n",
				fullyQualifiedEnumName(enumDef), enumVal->name, enumName(enumDef), counter++);
		}

		enumeration += fmt::format("constexpr {stringType} EnumName{enumName}(const {enumType} e) {{\n"
								   "  switch (e) {{\n"
								   "    {enumLookupCases}"
								   "    default:\n"
								   "      return \"\";\n"
								   "  }}\n"
								   "}}\n",
			"stringType"_a = constexprStringType(), "enumName"_a = enumName(enumDef),
			"enumType"_a = fullyQualifiedEnumName(enumDef), "enumLookupCases"_a = nameLookup);

		return enumeration;
	}

	std::string constexprStringType() {
		// C++17 introduces constexpr std::string_view.
		return (mOptions.mCppStandard < CppStandard::CPP_17) ? ("const char *") : ("std::string_view");
	}

	std::string fixedSizeStruct(const StructDef *structDef) {
		assert(structDef != nullptr);

		std::string structure = comment(structDef->doc_comment);

		structure += fmt::format(
			"FLATBUFFERS_MANUALLY_ALIGNED_STRUCT({}) {} final {{\n", structDef->minalign, className(structDef));
		structure += "private:\n";
		for (const auto *field : structDef->fields.vec) {
			structure += structMember(field);
		}
		structure += '\n';
		structure += "public:\n";
		structure += "};\n";
		structure += fmt::format("FLATBUFFERS_STRUCT_END({}, {});\n", className(structDef), structDef->bytesize);
		structure += '\n';

		return structure;
	}

	std::string structMember(const FieldDef *fieldDef) {
		assert(fieldDef != nullptr);

		std::string field = comment(fieldDef->doc_comment);

		// Fields in structs can be: scalars, other structs, enums and fixed-length arrays of each of those.
		// Padding must be added if needed to get the struct size correct.
		// No defaults for values are supported, nor any attributes really.
		const auto type = fieldDef->value.type;

		field += fmt::format("{} {};\n", structFieldTypeToString(type), fieldDef->name);

		// Apply padding requirement.
		if (fieldDef->padding != 0) {
			size_t padding = fieldDef->padding;
			size_t counter = 0;

			if (padding & 0x01) {
				field += fmt::format("int8_t _padding_{}_{};\n", fieldDef->name, counter++);
				padding -= 1;
			}
			if (padding & 0x02) {
				field += fmt::format("int16_t _padding_{}_{};\n", fieldDef->name, counter++);
				padding -= 2;
			}
			if (padding & 0x04) {
				field += fmt::format("int32_t _padding_{}_{};\n", fieldDef->name, counter++);
				padding -= 4;
			}
			while (padding != 0) {
				field += fmt::format("int64_t _padding_{}_{};\n", fieldDef->name, counter++);
				padding -= 8;
			}
		}

		return field;
	}

	static bool typeIsString(const Type &type) {
		return (type.base_type == BASE_TYPE_STRING) || (type.element == BASE_TYPE_STRING);
	}

	static bool typeIsBool(const Type &type) {
		return IsBool(type.base_type) || IsBool(type.element);
	}

	static bool typeIsInteger(const Type &type) {
		return IsInteger(type.base_type) || IsInteger(type.element);
	}

	static bool typeIsLongInteger(const Type &type) {
		return IsLong(type.base_type) || IsLong(type.element);
	}

	static bool typeIsUnsignedInteger(const Type &type) {
		return IsUnsigned(type.base_type) || IsUnsigned(type.element);
	}

	static bool typeIsScalar(const Type &type) {
		return IsScalar(type.base_type) || IsScalar(type.element);
	}

	static bool typeIsFloat(const Type &type) {
		return IsFloat(type.base_type) || IsFloat(type.element);
	}

	static bool typeIsStruct(const Type &type) {
		return ((type.base_type == BASE_TYPE_STRUCT) || (type.element == BASE_TYPE_STRUCT))
			   && (type.struct_def != nullptr) && (type.struct_def->fixed == true);
	}

	static bool typeIsTable(const Type &type) {
		return ((type.base_type == BASE_TYPE_STRUCT) || (type.element == BASE_TYPE_STRUCT))
			   && (type.struct_def != nullptr) && (type.struct_def->fixed == false);
	}

	static bool typeIsEnum(const Type &type) {
		return typeIsInteger(type) && (type.enum_def != nullptr) && (type.enum_def->is_union == false);
	}

	static bool typeIsUnionType(const Type &type) {
		return ((type.base_type == BASE_TYPE_UTYPE) || (type.element == BASE_TYPE_UTYPE)) && (type.enum_def != nullptr)
			   && (type.enum_def->is_union == true);
	}

	static bool typeIsUnion(const Type &type) {
		return ((type.base_type == BASE_TYPE_UNION) || (type.element == BASE_TYPE_UNION)) && (type.enum_def != nullptr)
			   && (type.enum_def->is_union == true);
	}

	/**
	 * Must be an enum, union or union type.
	 */
	static std::string scalarTypeToString(const Type &type) {
		assert(typeIsScalar(type));

		if (IsScalar(type.base_type)) {
			return FLATBUFFERS_CPP_TYPES[type.base_type];
		}
		else {
			return FLATBUFFERS_CPP_TYPES[type.element];
		}
	}

	std::string structFieldTypeToString(const Type &type, const bool objectAPI = false) {
		// See flatbuffers type table with APIs.
		// First we generate the types for data elements, then append
		// the needed parts for vectors/arrays.
		std::string typeString;

		if (typeIsStruct(type)) {
			typeString = fullyQualifiedClassName(type.struct_def, objectAPI);
		}
		else if (typeIsEnum(type)) {
			typeString = fullyQualifiedEnumName(type.enum_def);
		}
		else if (typeIsBool(type)) {
			if (objectAPI && !IsArray(type)) {
				// vectors/arrays of bool are problematic from a memory-layout standpoint.
				typeString = "bool";
			}
			else {
				// Flatbuffers API.
				typeString = FLATBUFFERS_CPP_TYPES[BASE_TYPE_BOOL];
			}
		}
		else if (typeIsScalar(type)) {
			// Same for fb and object APIs.
			typeString = scalarTypeToString(type);
		}
		else {
			throw std::out_of_range("structFieldTypeToString(): invalid type passed.");
		}

		if (IsArray(type)) {
			if (objectAPI) {
				if (mOptions.mCppStandard < CppStandard::CPP_20) {
					typeString = fmt::format("flatbuffers::span<const {}, {}>", typeString, type.fixed_length);
				}
				else {
					typeString = fmt::format("std::span<const {}, {}>", typeString, type.fixed_length);
				}
			}
			else {
				// Flatbuffers API.
				typeString = fmt::format("std::array<{}, {}>", typeString, type.fixed_length);
			}
		}

		return typeString;
	}

	/*std::string tableFieldTypeToString(const Type &type, const FieldDef *fieldDef, const bool objectAPI = false) {
		// See flatbuffers type table with APIs.
		// First we generate the types for data elements, then append
		// the needed parts for vector/array sequences.
		std::string typeString;

		return typeString;
	}*/
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
