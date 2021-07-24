// Features we need to support from idl.h:
// - prefixed_enums
// - scoped_enums
// - include_dependence_headers
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
// - object_prefix
// - object_suffix
// - include_prefix
// - keep_include_path
// - cpp_includes
// - cpp_std
// - cpp_static_reflection
// - mini_reflect
// - set_empty_strings_to_null
// - set_empty_vectors_to_null

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/flatc.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"

#include <iostream>
#include <string_view>
#include <type_traits>

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
	CppStandard g_cpp_std; // Base version of C++ standard.

	IDLOptionsCppNG(const IDLOptions &opts) : IDLOptions(opts) {
		auto cppStandard = opts.cpp_std;

		std::cout << "cppstd: " << cppStandard << std::endl;

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
			g_cpp_std = CppStandard::CPP_11;
		}
		else if (cppStandardNumeric == 17) {
			g_cpp_std = CppStandard::CPP_17;
		}
		else if (cppStandardNumeric == 20) {
			g_cpp_std = CppStandard::CPP_20;
		}
		else {
			throw std::invalid_argument("Unknown value of the '--cpp-std' switch: " + opts.cpp_std);
		}

		if (cpp_static_reflection && (EnumAsInteger(g_cpp_std) < EnumAsInteger(CppStandard::CPP_17))) {
			throw std::out_of_range("--cpp-static-reflection requires using --cpp-std 17 or higher.");
		}
	}
};

class CppGeneratorNG : public BaseGenerator {
private:
	// This tracks the current namespace so we can insert namespace declarations.
	const Namespace *mCurrentNamespace;

	const IDLOptionsCppNG mOptions;
	const TypedFloatConstantGenerator mFloatConstantGenerator;

public:
	CppGeneratorNG(const Parser &parser, const std::string &path, const std::string &fileName, const IDLOptionsCppNG opts) :
	BaseGenerator(parser, path, fileName, "", "::", "h"),
	mCurrentNamespace(nullptr),
	mOptions(opts),
	mFloatConstantGenerator("std::numeric_limits<double>::", "std::numeric_limits<float>::", "quiet_NaN()", "infinity()") {
		std::cout << "path: " << path << std::endl;
		std::cout << "fileName: " << fileName << std::endl;
	}

	bool generate() override {
		const auto filePath = GeneratedFileName(path_, file_name_, mOptions);
		const auto finalCode = "";

		// Save the file and optionally generate the binary schema code.
		return SaveFile(filePath.c_str(), finalCode, false);
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
