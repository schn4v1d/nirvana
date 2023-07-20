#include "Environment.h"
#include "Frame.h"
#include "cl_fun.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_go(Value args, Environment *env) {
  Value tag = cl::car(args);

  Frame *frame = env->lookup_tagbody(tag);

  env->unwind(frame, false);

  TagbodyFrame &tb = frame->get_tagbody();

  longjmp(tb.jmp_buf, tb.tags[tag]);
}

} // namespace lisp::special_operators