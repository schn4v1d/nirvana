#include "eval.h"
#include "Cons.h"
#include "Symbol.h"
#include "cl_fun.h"
#include "util.h"
#include <cassert>
#include <unordered_map>

namespace lisp {

std::unordered_map<Symbol *, std::function<Value(Value args, Environment *env)>>
    special_forms{};

void init_eval() {
  special_forms.insert(
      std::make_pair(get_symbol(SYM_LET), [](Value args, Environment *parent) {
        Value var_list = cl::car(args);
        Value body = cl::cdr(args);

        Environment *env = make_environment(parent);

        while (!is_nil(var_list)) {
          Value var_spec = cl::car(var_list);
          if (is_cons(var_spec)) {
            Value name = cl::car(var_spec);
            assert(is_symbol(name));
            Value value = cl::cadr(var_spec);
            assert(is_nil(cl::cddr(var_spec)));
            value = eval(value, parent);
            env->bind(name, value, false);
          } else {
            assert(is_symbol(var_spec));
            env->bind(var_spec, NIL, false);
          }
          var_list = cl::cdr(var_list);
        }

        Value result{};
        while (!is_nil(body)) {
          result = eval(cl::car(body), env);
          body = cl::cdr(body);
        }

        return result;
      }));
}

Value eval(Value value, Environment *env) {
  if (is_cons(value)) {
    Cons *cons = get_cons(value);

    Value op = cons->get_car();

    if (is_symbol(op)) {
      Symbol *name = get_symbol(op);

      auto special_form = special_forms.find(name);
      if (special_form != special_forms.end()) {
        return (*special_form).second(cons->get_cdr(), env);
      }

      throw NotImplemented{};
    } else {
      throw NotImplemented{};
    }
  } else if (is_symbol(value)) {
    env->lookup_variable(value);
  } else {
    return value;
  }
}

} // namespace lisp