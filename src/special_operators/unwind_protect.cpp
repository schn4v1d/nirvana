#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_unwind_protect(Value args, Environment *env) {
  Value protected_form = cl::car(args);
  Value cleanup_forms = cl::cdr(args);

  Frame *frame = env->establish_unwind_protect(cleanup_forms);

  Value result = eval(protected_form, env, true);

  env->unwind(frame);

  return result;
}

} // namespace lisp::special_operators