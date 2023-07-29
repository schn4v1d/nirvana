#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_if(Value args, Environment *env) {
  Value condition_form = cl::first(args);
  Value then_form = cl::second(args);
  Value else_form = cl::third(args);

  Value condition = eval(condition_form, env);

  if (condition) {
    return eval(then_form, env, true);
  } else {
    return eval(else_form, env, true);
  }
}

} // namespace lisp::special_operators