#include "LexicalBlock.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_block(Value args, Environment *parent) {
  Value result{NIL};

  Environment *env = make_environment(parent);

  Value name = cl::car(args);

  LexicalBlock *block = env->establish_block(name);

  Value body = cl::cdr(args);

  if (setjmp(*block->get_jmp_buf()) == 0) {
    while (!is_nil(body)) {
      result = eval(cl::car(body), env);
      body = cl::cdr(body);
    }
  } else {
    result = block->get_return_value();
  }

  env->unwind(block->get_frame());

  return result;
}

} // namespace lisp::special_operators