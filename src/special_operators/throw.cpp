#include "Frame.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"
#include <sstream>

namespace lisp::special_operators {

Value op_throw(Value args, Environment *env) {
  Value tag = eval(cl::first(args), env);

  Frame *frame = env->lookup_catch(tag);

  if (!frame) {
    std::ostringstream oss{};
    oss << "attempt to throw to a tag that does not exit: " << tag;
    throw std::runtime_error{oss.str()};
  }

  Value return_value = eval(cl::second(args), env);

  frame->get_catch().return_value = return_value;

  env->unwind(frame, false);

  longjmp(frame->get_catch().jmp_buf, 1);
}

} // namespace lisp::special_operators