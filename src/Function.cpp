#include "Function.h"
#include "BuiltinFunction.h"
#include <cassert>

namespace lisp {

Value call_function(Value function, Value args) {
  if (is_builtin_function(function)) {
    return get_builtin_function(function)->call(args);
  }

  assert(false);
  return NIL;
}

} // namespace lisp