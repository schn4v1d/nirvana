#include "BuiltinFunction.h"
#include "Lambda.h"
#include "call_function.h"
#include <cassert>

namespace lisp {

Value call_function(Value function, Value args) {
  if (is_builtin_function(function)) {
    return get_builtin_function(function)->call(args);
  } else if (is_lambda(function)) {
    return get_lambda(function)->call(args);
  }

  throw std::exception("not a function");
}

} // namespace lisp