#include "errors.h"
#include <sstream>

namespace lisp {

UnboundVariable::UnboundVariable(const std::string &name) {
  std::ostringstream oss{};
  oss << "unbound-variable " << name;
  message = oss.str();
}

const char *UnboundVariable::what() const { return message.c_str(); }

UndefinedFunction::UndefinedFunction(const std::string &name) {
  std::ostringstream oss{};
  oss << "undefined-function " << name;
  message = oss.str();
}

const char *UndefinedFunction::what() const { return message.c_str(); }

} // namespace lisp