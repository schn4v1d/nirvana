#include "Cons.h"
#include "Environment.h"
#include "Symbol.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"
#include <cassert>

namespace lisp::special_operators {

Value op_let(Value args, Environment *parent) {
  Value var_list = cl::car(args);
  Value body = cl::cdr(args);
  // TODO SPECIALS

  Environment *env = make_environment(parent);

  while (!is_nil(var_list)) {
    Value var_spec = cl::car(var_list);
    if (is_cons(var_spec)) {
      Value name = cl::car(var_spec);
      assert(is_symbol(name));
      Value value = cl::cadr(var_spec);
      assert(is_nil(cl::cddr(var_spec)));
      value = eval(value, parent);
      env->bind_lexical_variable(name, value, false);
    } else {
      assert(is_symbol(var_spec));
      env->bind_lexical_variable(var_spec, NIL, false);
    }
    var_list = cl::cdr(var_list);
  }

  Value result{};
  while (!is_nil(body)) {
    result = eval(cl::car(body), env, true);
    body = cl::cdr(body);
  }

  return result;
}

Value op_let_star(Value args, Environment *parent) {
  Value var_list = cl::car(args);
  Value body = cl::cdr(args);
  // TODO SPECIALS

  Environment *env = make_environment(parent);

  while (!is_nil(var_list)) {
    Value var_spec = cl::car(var_list);
    if (is_cons(var_spec)) {
      Value name = cl::car(var_spec);
      assert(is_symbol(name));
      Value value = cl::cadr(var_spec);
      assert(is_nil(cl::cddr(var_spec)));
      value = eval(value, env);
      env->bind_lexical_variable(name, value, false);
    } else {
      assert(is_symbol(var_spec));
      env->bind_lexical_variable(var_spec, NIL, false);
    }
    var_list = cl::cdr(var_list);
  }

  Value result{};
  while (!is_nil(body)) {
    result = eval(cl::car(body), env, true);
    body = cl::cdr(body);
  }

  return result;
}

} // namespace lisp::special_operators