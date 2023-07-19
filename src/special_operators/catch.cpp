#include "Frame.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_catch(Value args, Environment *env) {
  Value result{NIL};

  Value tag = eval(cl::car(args), env);

  Frame *frame = env->establish_catch(tag);

  Value body = cl::cdr(args);

  if (setjmp(frame->get_catch().jmp_buf) == 0) {
    while (!is_nil(body)) {
      result = eval(cl::car(body), env);
      body = cl::cdr(body);
    }
  } else {
    result = frame->get_catch().return_value;
  }

  env->unwind(frame);

  return result;
}

} // namespace lisp::special_operators