#include "util.h"
#include <sstream>

std::string not_implemented_error(const std::source_location location) {
  std::ostringstream oss{};
  oss << "Not yet implemented: " << location.file_name() << '('
      << location.line() << ':' << location.column() << ") \""
      << location.function_name() << '"';
  return oss.str();
}

NotImplemented::NotImplemented(const std::source_location location)
    : std::logic_error(not_implemented_error(location)) {}
