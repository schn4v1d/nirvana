#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_progn(Value body, Environment *env) {
  Value result{NIL};
  while (!is_nil(body)) {
    result = eval(cl::car(body), env, true);
    body = cl::cdr(body);
  }
  return result;
}

} // namespace lisp::special_operators