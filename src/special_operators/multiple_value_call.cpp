#include "Cons.h"
#include "Values.h"
#include "call_function.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_multiple_value_call(Value args, Environment *env) {
  Value function = eval(cl::car(args), env);

  Value forms = cl::cdr(args);

  std::vector<Value> call_args{};

  while (is_cons(forms)) {
    Value values = eval(cl::car(forms), env, true);

    if (is_values(values)) {
      for (Value v : get_values(values)->get_values()) {
        call_args.push_back(v);
      }
    } else {
      call_args.push_back(values);
    }

    forms = cl::cdr(forms);
  }

  return call_function(function, list_from_cppvector(std::move(call_args)));
}

} // namespace lisp::special_operators