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

static constexpr std::array<const char *, 95> CPP_KEYWORDS = {{
	"alignas",
	"alignof",
	"and",
	"and_eq",
	"asm",
	"atomic_cancel",
	"atomic_commit",
	"atomic_noexcept",
	"auto",
	"bitand",
	"bitor",
	"bool",
	"break",
	"case",
	"catch",
	"char",
	"char16_t",
	"char32_t",
	"class",
	"compl",
	"concept",
	"const",
	"constexpr",
	"const_cast",
	"continue",
	"co_await",
	"co_return",
	"co_yield",
	"decltype",
	"default",
	"delete",
	"do",
	"double",
	"dynamic_cast",
	"else",
	"enum",
	"explicit",
	"export",
	"extern",
	"false",
	"float",
	"for",
	"friend",
	"goto",
	"if",
	"import",
	"inline",
	"int",
	"long",
	"module",
	"mutable",
	"namespace",
	"new",
	"noexcept",
	"not",
	"not_eq",
	"nullptr",
	"operator",
	"or",
	"or_eq",
	"private",
	"protected",
	"public",
	"register",
	"reinterpret_cast",
	"requires",
	"return",
	"short",
	"signed",
	"sizeof",
	"static",
	"static_assert",
	"static_cast",
	"struct",
	"switch",
	"synchronized",
	"template",
	"this",
	"thread_local",
	"throw",
	"true",
	"try",
	"typedef",
	"typeid",
	"typename",
	"union",
	"unsigned",
	"using",
	"virtual",
	"void",
	"volatile",
	"wchar_t",
	"while",
	"xor",
	"xor_eq",
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
		mCode += "#include <array>\n";

		if (mOptions.mCppStandard >= CppStandard::CPP_20) {
			mCode += "#include <span>\n";
		}

		if (mOptions.mCppStandard >= CppStandard::CPP_17) {
			mCode += "#include <string_view>\n";
		}

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
				if (mOptions.generate_object_based_api) {
					mCode += fmt::format("struct {};\n", className(tb, true));
				}

				mCode += fmt::format("struct {};\n", className(tb));
				mCode += fmt::format("struct {}Builder;\n", className(tb));
			}
			mCode += '\n';

			mCode += namespaceOpenClose(ns, false);
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
				mCode += unionVerification(un);
			}

			mCode += namespaceOpenClose(ns, false);
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
		}
	}

	void generateTables() {
		if (mOptions.generate_object_based_api) {
			mCode += "// Flatbuffer tables and Object API native tables\n";
		}
		else {
			mCode += "// Flatbuffer tables (Object API disabled)\n";
		}

		// Go through namespaces in order (see top comments about traversal order).
		for (const auto &ns : mNamespaces) {
			if (ns.mTables.empty()) {
				// No tables present, skip this namespace.
				continue;
			}

			mCode += namespaceOpenClose(ns, true);

			for (const auto *tb : ns.mTables) {
				if (mOptions.generate_object_based_api) {
					mCode += nativeTable(tb);
				}

				mCode += table(tb);
			}

			mCode += namespaceOpenClose(ns, false);
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
			return "";
		}

		if (mOptions.mCppStandard < CppStandard::CPP_17) {
			std::string nsString;
			for (const auto &component : ns.mNameComponents) {
				nsString += fmt::format((open) ? ("namespace {} {{\n") : ("}} // namespace {}\n"), component);
			}
			nsString += '\n';
			return nsString;
		}
		else {
			// C++17 and newer can do nested namespaces directly.
			return fmt::format((open) ? ("namespace {} {{\n\n") : ("}} // namespace {}\n\n"), ns.mFullName);
		}
	}

	static std::string comment(const std::vector<std::string> &docComment) {
		std::string comment;
		GenComment(docComment, &comment, nullptr, "");
		return comment;
	}

	std::string constexprStringType() {
		// C++17 introduces constexpr std::string_view.
		return (mOptions.mCppStandard < CppStandard::CPP_17) ? ("const char *") : ("std::string_view");
	}

	std::string constexprSpanType() {
		return (mOptions.mCppStandard < CppStandard::CPP_20) ? ("flatbuffers::span") : ("std::span");
	}

	std::string stringType(const FieldDef *definition) {
		return attributeValue(definition, "cpp_str_type", BASE_TYPE_STRING, mOptions.cpp_object_api_string_type);
	}

	std::string vectorType(const FieldDef *definition) {
		return attributeValue(definition, "cpp_vec_type", BASE_TYPE_STRING, mOptions.cpp_object_api_vector_type);
	}

	std::string pointerType(const FieldDef *definition) {
		return attributeValue(definition, "cpp_ptr_type", BASE_TYPE_STRING, mOptions.cpp_object_api_pointer_type);
	}

	std::string pointerTypeGetter(const FieldDef *definition) {
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

	static std::string numericConstant(
		const std::string &constant, const BaseType type, const EnumDef *enumDef = nullptr) {
		if (enumDef != nullptr) {
			// This is an enum numeric constant, so we try to convert it to its symbolic name.
			const auto enumVal = enumDef->FindByValue(constant);
			if (enumVal != nullptr) {
				return fmt::format("{}::{}", fullyQualifiedEnumName(enumDef), enumVal->name);
			}

			// Maybe it's a bitflag enum and we also have to check NONE/ANY.
			if (attributeExists(enumDef, "bit_flags")) {
				if (constant == enumDef->AllFlags()) {
					return fmt::format("{}::ANY", fullyQualifiedEnumName(enumDef));
				}
				else if (constant == "0") {
					return fmt::format("{}::NONE", fullyQualifiedEnumName(enumDef));
				}
			}
		}

		switch (type) {
			case BASE_TYPE_BOOL: {
				const auto intToBool = std::stoll(constant);

				if (intToBool == 0) {
					return "false";
				}
				else {
					return "true";
				}
			}

			case BASE_TYPE_UINT:
				return constant + "U";

			case BASE_TYPE_LONG:
				return constant + "LL";

			case BASE_TYPE_ULONG:
				return constant + "LLU";

			case BASE_TYPE_FLOAT: {
				const auto flt = std::stof(constant);

				if (std::isnan(flt)) {
					return "std::numeric_limits<float>::quiet_NaN()";
				}
				else if (std::isinf(flt)) {
					return "std::numeric_limits<float>::infinity()";
				}
				else {
					return constant + "F";
				}
			}

			case BASE_TYPE_DOUBLE: {
				const auto dbl = std::stod(constant);

				if (std::isnan(dbl)) {
					return "std::numeric_limits<double>::quiet_NaN()";
				}
				else if (std::isinf(dbl)) {
					return "std::numeric_limits<double>::infinity()";
				}
				else {
					return constant;
				}
			}

			default:
				return constant;
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
				numericConstant(enumDef->ToString(*enumVal), enumDef->underlying_type.base_type));
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
								   "}}\n\n",
			"enumType"_a = fullyQualifiedEnumName(enumDef), "enumName"_a = enumName(enumDef),
			"enumNumElements"_a = enumDef->size(), "enumValues"_a = enumValues);

		std::string enumNames;
		for (const auto *enumVal : enumDef->Vals()) {
			enumNames += fmt::format("\"{}\",\n", enumVal->name);
		}

		enumeration += fmt::format("constexpr std::array<{stringType}, {enumNumElements}> EnumNames{enumName}() {{\n"
								   "  constexpr std::array<{stringType}, {enumNumElements}> names = {{{{\n"
								   "    {enumNames}"
								   "  }}}};\n"
								   "  return names;\n"
								   "}}\n\n",
			"stringType"_a = constexprStringType(), "enumName"_a = enumName(enumDef),
			"enumNumElements"_a = enumDef->size(), "enumNames"_a = enumNames);

		std::string nameLookup;
		size_t counter = 0;
		for (const auto *enumVal : enumDef->Vals()) {
			nameLookup += fmt::format("case {}::{}:\n"
									  "  return EnumNames{}()[{}];\n\n",
				fullyQualifiedEnumName(enumDef), enumVal->name, enumName(enumDef), counter++);
		}

		enumeration += fmt::format("constexpr {stringType} EnumName{enumName}(const {enumType} e) {{\n"
								   "  switch (e) {{\n"
								   "    {enumLookupCases}"
								   "    default:\n"
								   "      return \"\";\n"
								   "  }}\n"
								   "}}\n\n",
			"stringType"_a = constexprStringType(), "enumName"_a = enumName(enumDef),
			"enumType"_a = fullyQualifiedEnumName(enumDef), "enumLookupCases"_a = nameLookup);

		return enumeration;
	}

	std::string unionVerification(const EnumDef *enumDef) {
		assert(enumDef != nullptr);

		std::string verifiers;

		verifiers += fmt::format("inline bool Verify{}(flatbuffers::Verifier &verifier, const void *obj, {} type) {{\n",
			enumName(enumDef), fullyQualifiedEnumName(enumDef));
		verifiers += "switch (type) {\n";
		for (const auto *enumVal : enumDef->Vals()) {
			verifiers += fmt::format("case {}::{}:\n", fullyQualifiedEnumName(enumDef), enumVal->name);

			if (typeIsStruct(enumVal->union_type)) {
				verifiers += fmt::format("return verifier.Verify<{}>(static_cast<const uint8_t *>(obj), 0);\n\n",
					fullyQualifiedClassName(enumVal->union_type.struct_def));
			}
			else if (typeIsTable(enumVal->union_type)) {
				verifiers += fmt::format("return verifier.VerifyTable(reinterpret_cast<const {} *>(obj));\n\n",
					fullyQualifiedClassName(enumVal->union_type.struct_def));
			}
			else {
				verifiers += "return true;\n\n";
			}
		}
		verifiers += "default:\nreturn true;\n\n";
		verifiers += "}\n}\n\n";

		verifiers += fmt::format("inline bool Verify{}Vector(flatbuffers::Verifier &verifier, "
								 "const flatbuffers::Vector<flatbuffers::Offset<void>> *values, "
								 "const flatbuffers::Vector<{}> *types) {{\n",
			enumName(enumDef), scalarTypeToString(enumDef->underlying_type));
		verifiers += "if ((!values) || (!types)) { return ((!values) && (!types)); }\n\n";
		verifiers += "if (values->size() != types->size()) { return false; }\n\n";
		verifiers += "for (flatbuffers::uoffset_t i = 0; i < values->size(); i++) {\n";
		verifiers
			+= fmt::format("if (!Verify{}(verifier, values->Get(i), types->GetEnum<{}>(i))) {{ return false; }}\n",
				enumName(enumDef), fullyQualifiedEnumName(enumDef));
		verifiers += "}\n\nreturn true;\n}\n\n";

		return verifiers;
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

		if (mOptions.generate_object_based_api) {
			structure += fmt::format("using NativeTableType = {};\n", className(structDef));
		}
		structure += fmt::format("using TableType = {};\n\n", className(structDef));

		structure += structConstructors(structDef);
		for (const auto *field : structDef->fields.vec) {
			structure += structAccessors(field);
		}
		structure += "};\n";
		structure += fmt::format("FLATBUFFERS_STRUCT_END({}, {});\n", className(structDef), structDef->bytesize);
		structure += '\n';

		// Check assumptions via static_assert.
		structure += fmt::format("static_assert(std::is_standard_layout_v<{0}>, \"{0} is not standard layout\");\n",
			fullyQualifiedClassName(structDef));
		structure
			+= fmt::format("static_assert(std::is_trivially_copyable_v<{0}>, \"{0} is not trivially copyable\");\n",
				fullyQualifiedClassName(structDef));
		structure += fmt::format(
			"static_assert(std::is_trivially_destructible_v<{0}>, \"{0} is not trivially destructible\");\n",
			fullyQualifiedClassName(structDef));
		structure += '\n';

		return structure;
	}

	std::string structMember(const FieldDef *fieldDef) {
		assert(fieldDef != nullptr);

		std::string field = comment(fieldDef->doc_comment);

		// Fields in structs can be: scalars, other structs, enums and fixed-length arrays of each of those.
		// Padding must be added if needed to get the struct size correct.
		// No defaults for values are supported, nor any attributes really.
		// Use C++11 {} uniform initialization which will value-initialize all types
		// that a struct supports to zero/empty (scalars, arrays, enums, other structs).
		// Struct fields have an underscore (_) appended to their name to not conflict with the
		// name of their getter, which is the same.
		field += fmt::format("{} {}_{{}};\n", structFieldTypeToString(fieldDef->value.type), fieldDef->name);

		// Apply padding requirement. Zero initialize padding.
		if (fieldDef->padding != 0) {
			size_t padding = fieldDef->padding;
			size_t counter = 0;

			if (padding & 0x01) {
				field += fmt::format("int8_t padding_{}_{}{{}};\n", fieldDef->name, counter++);
				padding -= 1;
			}
			if (padding & 0x02) {
				field += fmt::format("int16_t padding_{}_{}{{}};\n", fieldDef->name, counter++);
				padding -= 2;
			}
			if (padding & 0x04) {
				field += fmt::format("int32_t padding_{}_{}{{}};\n", fieldDef->name, counter++);
				padding -= 4;
			}
			while (padding != 0) {
				field += fmt::format("int64_t padding_{}_{}{{}};\n", fieldDef->name, counter++);
				padding -= 8;
			}
		}

		return field;
	}

	std::string structConstructors(const StructDef *structDef) {
		assert(structDef != nullptr);

		std::string constructors;

		// Default constructor. Do nothing as everything is already value-initialized.
		constructors += fmt::format("{}() = default;\n\n", className(structDef));

		// Constructor with all members. First declare all parameters.
		// Use objectAPI interface here as that's the 'user-visibile-C++-interface' code.
		// And then use the constructor initializer list to initialize all scalars,
		// enums and copy structs. Arrays are done separately in the constructor body.
		std::string constructorParameters;
		std::string constructorInitializerList;
		std::string constructorBody;

		for (const auto *field : structDef->fields.vec) {
			const auto type = field->value.type;

			constructorParameters += fmt::format("const {} {},", structFieldTypeToString(type, true), field->name);

			if (IsArray(type)) {
				// Handle arrays: use std:::uninitialized_copy_n() for byte-sized elements for
				// maximum performance, for scalars loop over the span and assign to array,
				// doing endian conversion, by using std::transform(). For other structs, we can
				// again just use std:::uninitialized_copy_n(), as they are trivially copyable.
				if (IsOneByte(type.element) || typeIsStruct(type)
					|| (typeIsEnum(type) && IsOneByte(type.enum_def->underlying_type.base_type))) {
					constructorBody += fmt::format(
						"std::uninitialized_copy_n({0}.cbegin(), {0}_.size(), {0}_.begin());\n", field->name);
				}
				else {
					constructorBody += fmt::format(
						"std::transform({0}.cbegin(), {0}.cend(), {0}_.begin(), flatbuffers::EndianScalar<{1}>);\n",
						field->name, structFieldTypeToString(type, false, true));
				}
			}
			else if (typeIsScalar(type)) {
				// For scalars we always do EndianConversion with underlying fixed-size type.
				// For bool we use uint8_t; for one-byte scalars this isn't strictly needed but does no
				// harm (gets compiled away), so we keep handling for all scalars the same to simplify.
				constructorInitializerList += fmt::format(
					"{0}_(flatbuffers::EndianScalar<{1}>({0})),", field->name, structFieldTypeToString(type));
			}
			else {
				constructorInitializerList += fmt::format("{0}_({0}),", field->name);
			}
		}

		// Remove last comma.
		constructorParameters.pop_back();
		constructorInitializerList.pop_back();

		// Full constructor.
		constructors += fmt::format("{}({}) : {}\n{{ {} }}\n\n", className(structDef), constructorParameters,
			constructorInitializerList, constructorBody);

		return constructors;
	}

	std::string structAccessors(const FieldDef *fieldDef) {
		assert(fieldDef != nullptr);

		const auto type              = fieldDef->value.type;
		const auto fieldNameOriginal = fieldName(fieldDef->name, IDLOptions::CaseStyle_Unchanged);
		const auto fieldNameUpper    = fieldName(fieldDef->name, IDLOptions::CaseStyle_Upper);

		std::string getter = comment(fieldDef->doc_comment);

		// Getter return type.
		std::string getterReturnType = structFieldTypeToString(type, true, false, true);

		if (IsArray(type)) {
			// Arrays are special: pointers to const flatbuffers::Array.
			getterReturnType = fmt::format("const {} *", getterReturnType);
		}
		else if (IsStruct(type)) {
			// Structs are best returned as const-ref, so we special-case that here.
			// Object-API return type for struct is already ref, so add const only.
			getterReturnType = fmt::format("const {}", getterReturnType);
		}

		// Getter function body.
		std::string getterBody;

		if (IsArray(type)) {
			// Arrays access underlying std::array memory directly via .data().
			getterBody = fmt::format("return reinterpret_cast<{}>({}_.data());", getterReturnType, fieldNameOriginal);
		}
		else if (IsScalar(type.base_type)) {
			getterBody
				= fmt::format("flatbuffers::EndianScalar<{}>({}_)", structFieldTypeToString(type), fieldNameOriginal);

			if (IsBool(type.base_type)) {
				// Integer to bool by checking against zero.
				getterBody = fmt::format("({} != 0)", getterBody);
			}

			getterBody = fmt::format("return {};", getterBody);
		}
		else {
			getterBody = fmt::format("return {}_;", fieldNameOriginal);
		}

		// Add getVAR() getter.
		getter += fmt::format("{} get{}() const {{ {} }}\n", getterReturnType, fieldNameUpper, getterBody);

		// Add compatibility with main flatbuffers getter.
		getter += comment(fieldDef->doc_comment);
		getter += fmt::format(
			"{} {}() const {{ return get{}(); }}\n", getterReturnType, fieldNameOriginal, fieldNameUpper);

		// Return early if mutability is disabled.
		if (!mOptions.mutable_buffer) {
			return getter;
		}

		// Setter.
		std::string setter = comment(fieldDef->doc_comment);

		std::string setterParameterType = structFieldTypeToString(type, true);

		// Setter function body.
		std::string setterBody;

		if (IsArray(type)) {
			// Handle arrays: use std:::uninitialized_copy_n() for byte-sized elements for
			// maximum performance, for scalars loop over the span and assign to array,
			// doing endian conversion, by using std::transform(). For other structs, we can
			// again just use std:::uninitialized_copy_n(), as they are trivially copyable.
			if (IsOneByte(type.element) || typeIsStruct(type)
				|| (typeIsEnum(type) && IsOneByte(type.enum_def->underlying_type.base_type))) {
				setterBody += fmt::format(
					"std::uninitialized_copy_n({0}.cbegin(), {0}_.size(), {0}_.begin());\n", fieldNameOriginal);
			}
			else {
				setterBody += fmt::format(
					"std::transform({0}.cbegin(), {0}.cend(), {0}_.begin(), flatbuffers::EndianScalar<{1}>);\n",
					fieldNameOriginal, structFieldTypeToString(type, false, true));
			}
		}
		else if (typeIsScalar(type)) {
			// For scalars we always do EndianConversion with underlying fixed-size type.
			// For bool we use uint8_t; for one-byte scalars this isn't strictly needed but does no
			// harm (gets compiled away), so we keep handling for all scalars the same to simplify.
			setterBody += fmt::format(
				"{0}_ = flatbuffers::EndianScalar<{1}>({0});", fieldNameOriginal, structFieldTypeToString(type));
		}
		else {
			setterBody += fmt::format("{0}_ = {0};", fieldNameOriginal);
		}

		// Add setVAR(value) setter.
		setter += fmt::format(
			"void set{}(const {} {}) {{ {} }}\n", fieldNameUpper, setterParameterType, fieldNameOriginal, setterBody);

		// Add compatibility setters/mutators.
		if (IsArray(type)) {
			// Mutable arrays are a special case.
			setter += fmt::format("{0} *mutable_{1}() {{ return reinterpret_cast<{0} *>({1}_.data()); }}\n",
				structFieldTypeToString(type, true, false, true), fieldNameOriginal);
		}
		else {
			setter += fmt::format("void mutate_{0}(const {1} {0}) {{ set{2}({0}); }}\n", fieldNameOriginal,
				setterParameterType, fieldNameUpper);
		}

		return getter + setter;
	}

	static std::string fieldName(const std::string &name, const IDLOptions::CaseStyle style) {
		std::string fieldName = name;

		if (style == IDLOptions::CaseStyle_Upper) {
			fieldName = MakeCamel(fieldName, true); /* upper */
		}
		else if (style == IDLOptions::CaseStyle_Lower) {
			fieldName = MakeCamel(fieldName, false); /* lower */
		}

		return fieldName;
	}

	static std::string escapeKeyword(const std::string &name) {
		const auto iter = std::find(CPP_KEYWORDS.cbegin(), CPP_KEYWORDS.cend(), name);
		return (iter == CPP_KEYWORDS.cend()) ? (name) : (name + "_");
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

	static bool typeIsScalar(const Type &type) {
		return IsScalar(type.base_type) || IsScalar(type.element);
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

	std::string structFieldTypeToString(const Type &type, const bool objectAPI = false,
		const bool arrayInnerElementTypeOnly = false, const bool arrayAccessor = false) {
		// See flatbuffers type table with APIs.
		// First we generate the types for data elements, then append
		// the needed parts for vectors/arrays.
		std::string typeString;

		if (typeIsStruct(type)) {
			typeString = fullyQualifiedClassName(type.struct_def, objectAPI);

			// Pass structs by reference in object API for performance. Usually const-ref.
			if (objectAPI && !IsArray(type)) {
				typeString += " &";
			}
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

		if (IsArray(type) && (!arrayInnerElementTypeOnly)) {
			if (objectAPI) {
				const auto arrayType        = (arrayAccessor) ? ("flatbuffers::Array") : (constexprSpanType());
				const auto elementConstness = (arrayAccessor) ? ("") : ("const");

				typeString = fmt::format("{}<{} {}, {}>", arrayType, elementConstness, typeString, type.fixed_length);
			}
			else {
				// Flatbuffers API.
				typeString = fmt::format("std::array<{}, {}>", typeString, type.fixed_length);
			}
		}

		return typeString;
	}

	std::string tableFieldTypeToString(const Type &type, const FieldDef *fieldDef, const bool objectAPI = false,
		const bool vectorInnerElementTypeOnly = false) {
		// See flatbuffers type table with APIs.
		// First we generate the types for data elements, then append
		// the needed parts for vector/array sequences.
		std::string typeString;

		if (typeIsStruct(type) || typeIsTable(type)) {
			if (objectAPI) {
				if (fieldDef->native_inline) {
					typeString = fullyQualifiedClassName(type.struct_def, true);

					// Pass structs by reference in object API for performance. Usually const-ref.
					if (!IsVector(type)) {
						typeString += " &";
					}
				}
				else {
					typeString
						= fmt::format("{}<{}>", pointerType(fieldDef), fullyQualifiedClassName(type.struct_def, true));
				}
			}
			else {
				typeString = fullyQualifiedClassName(type.struct_def, false);
			}
		}
		else if (typeIsEnum(type)) {
			typeString = fullyQualifiedEnumName(type.enum_def);
		}
		else if (typeIsUnionType(type)) {
			typeString = fullyQualifiedEnumName(type.enum_def);
		}
		else if (typeIsUnion(type)) {
			if (objectAPI) {
				typeString = fullyQualifiedEnumName(type.enum_def) + "Union";
			}
			else {
				typeString = "void";
			}
		}
		else if (typeIsString(type)) {
			if (objectAPI) {
				typeString = stringType(fieldDef);
			}
			else {
				typeString = "flatbuffers::String";
			}
		}
		else if (typeIsBool(type)) {
			if (objectAPI && !IsVector(type)) {
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
			throw std::out_of_range("tableFieldTypeToString(): invalid type passed.");
		}

		if (IsVector(type) && (!vectorInnerElementTypeOnly)) {
			if (objectAPI) {
				typeString = fmt::format("{}<{}>", vectorType(fieldDef), typeString);
			}
			else {
				// Flatbuffers API. This is a bit more complex to account for fb::Offset mainly.
				// And structs are again special because of internal performance.
				if (typeIsStruct(type)) {
					typeString = fmt::format("const {} *", typeString);
				}
				else if (typeIsTable(type) || typeIsUnion(type) || typeIsString(type)) {
					typeString = fmt::format("flatbuffers::Offset<{}>", typeString);
				}

				typeString = fmt::format("flatbuffers::Vector<{}>", typeString);
			}
		}

		return typeString;
	}

	std::string nativeTable(const StructDef *tableDef) {
		assert(tableDef != nullptr);

		const auto flatName   = className(tableDef, false);
		const auto nativeName = className(tableDef, true);

		std::string nativeTable = comment(tableDef->doc_comment);

		nativeTable += fmt::format("struct {} : public flatbuffers::NativeTable {{\n", nativeName);
		nativeTable += fmt::format("using TableType = {};\n\n", flatName);
		for (const auto *field : tableDef->fields.vec) {
			if (field->deprecated) {
				// Skip deprecated fields in native table.
				continue;
			}

			const std::string initValue
				= (IsScalar(field->value.type.base_type)) ? (
					  numericConstant(field->value.constant, field->value.type.base_type, field->value.type.enum_def))
														  : ("");

			nativeTable += fmt::format("{}{} {}{{ {} }};\n", comment(field->doc_comment),
				tableFieldTypeToString(field->value.type, field, true),
				fieldName(field->name, mOptions.cpp_object_api_field_case_style), initValue);
		}
		nativeTable += "};\n\n";

		return nativeTable;
	}

	std::string fieldOffsetName(const std::string &name) {
		auto upperName = fieldName(name, IDLOptions::CaseStyle_Unchanged);
		std::transform(upperName.begin(), upperName.end(), upperName.begin(), CharToUpper);
		return "VT_" + upperName;
	}

	std::string table(const StructDef *tableDef) {
		assert(tableDef != nullptr);

		const auto flatName    = className(tableDef, false);
		const auto builderName = flatName + "Builder";
		const auto nativeName  = className(tableDef, true);

		std::string table = comment(tableDef->doc_comment);

		table += fmt::format("struct {} final : private flatbuffers::Table {{\n", flatName);
		if (mOptions.generate_object_based_api) {
			table += fmt::format("using NativeTableType = {};\n", nativeName);
		}
		table += fmt::format("using Builder = {};\n", builderName);
		table += '\n';

		// Offsets for fields.
		table += "enum FlatBuffersVTableOffset FLATBUFFERS_VTABLE_UNDERLYING_TYPE {\n";
		for (const auto *field : tableDef->fields.vec) {
			if (field->deprecated) {
				// Skip deprecated fields.
				continue;
			}

			table += fmt::format("{} = {},\n", fieldOffsetName(field->name), field->value.offset);
		}
		table += "};\n\n";

		// Flatbuffer field getters.
		for (const auto *field : tableDef->fields.vec) {
			if (field->deprecated) {
				// Skip deprecated fields.
				continue;
			}

			table += flatbufferAccessors(field);
		}

		table += "};\n\n";

		return table;
	}

	std::string flatbufferAccessors(const FieldDef *fieldDef) {
		assert(fieldDef != nullptr);

		const auto type = fieldDef->value.type;

		std::string getter = comment(fieldDef->doc_comment);

		// Getter return type.
		std::string getterReturnType = tableFieldTypeToString(type, fieldDef, false);

		if (!IsScalar(type.base_type)) {
			getterReturnType = fmt::format("const {} *", getterReturnType);
		}

		// Getter function body.
		std::string getterBody;

		if (!IsVector(type) && typeIsScalar(type)) {
			getterBody = fmt::format("return GetField<{}>({}, {});", getterReturnType, fieldOffsetName(fieldDef->name),
				numericConstant(fieldDef->value.constant, type.base_type, type.enum_def));
		}
		else {
			getterBody = fmt::format("return GetPointer<{}>({});", getterReturnType, fieldOffsetName(fieldDef->name));
		}

		// Flatbuffers getter.
		getter += fmt::format("{} {}() const {{ {} }}\n", getterReturnType,
			fieldName(fieldDef->name, IDLOptions::CaseStyle_Unchanged), getterBody);

		// Return early if mutability is disabled.
		if (!mOptions.mutable_buffer) {
			return getter;
		}

		// Setter.
		std::string setter = comment(fieldDef->doc_comment);

		return getter + setter;
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
