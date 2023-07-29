#include "LexicalBlock.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"
#include <sstream>

namespace lisp::special_operators {

Value op_return_from(Value args, Environment *env) {
  Value block_name = cl::first(args);

  Value return_value = eval(cl::second(args), env, true);

  Value blockv = env->lookup_block(block_name);
  if (is_nil(blockv)) {
    std::ostringstream oss{};
    oss << "no block named " << block_name << " in current environment";
    throw std::runtime_error{oss.str()};
  }

  LexicalBlock *block = get_block(blockv);

  block->set_return_value(return_value);

  env->unwind(block->get_frame(), false);

  longjmp(*block->get_jmp_buf(), 1);
}

} // namespace lisp::special_operators