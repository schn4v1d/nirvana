#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"
#include <cassert>

namespace lisp::special_operators {

Value op_setq(Value args, Environment *env) {
  Value value{};

  while (!is_nil(args)) {
    Value name = cl::car(args);
    args = cl::cdr(args);

    if (is_nil(args)) {
      assert(false);
    }

    value = eval(cl::car(args), env);
    args = cl::cdr(args);

    env->assign_variable(name, value);
  }

  return value;
}

} // namespace lisp::special_operators