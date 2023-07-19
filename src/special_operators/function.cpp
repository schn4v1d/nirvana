#include "Environment.h"
#include "cl_fun.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_function(Value args, Environment *env) {
  Value designator = cl::car(args);

  return env->get_function(designator);
}

} // namespace lisp::special_operators