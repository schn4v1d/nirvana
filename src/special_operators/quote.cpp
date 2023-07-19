#include "cl_fun.h"
#include "special_operators.h"
#include <cassert>

namespace lisp::special_operators {

Value op_quote(Value args, Environment *env) {
  assert(is_nil(cl::cdr(args)));
  return cl::car(args);
}

} // namespace lisp::special_operators