#include "eval.h"
#include "Cons.h"
#include "Function.h"
#include "Package.h"
#include "Symbol.h"
#include "cl_fun.h"
#include "util.h"
#include <cassert>
#include <unordered_map>

namespace lisp {

std::unordered_map<Symbol *, std::function<Value(Value args, Environment *env)>>
    special_forms{};

void init_eval() {
  Symbol *current_package = get_symbol(SYM_STAR_PACKAGE_STAR);
  current_package->declare_special();
  current_package->set_value(PKG_CL_USER->make_value());

  special_forms.insert(
      std::make_pair(get_symbol(SYM_DEFUN), [](Value args, Environment *env) {
        Value name = cl::first(args);

        assert(false);

        return name;
      }));

  special_forms.insert(
      std::make_pair(get_symbol(SYM_IF), [](Value args, Environment *env) {
        Value condition_form = cl::first(args);
        Value then_form = cl::second(args);
        Value else_form = cl::third(args);

        Value condition = eval(condition_form, env);

        if (condition) {
          return eval(then_form, env);
        } else {
          return eval(else_form, env);
        }
      }));

  special_forms.insert(
      std::make_pair(get_symbol(SYM_LET), [](Value args, Environment *parent) {
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
          result = eval(cl::car(body), env);
          body = cl::cdr(body);
        }

        return result;
      }));

  special_forms.insert(std::make_pair(
      get_symbol(SYM_LET_STAR), [](Value args, Environment *parent) {
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
          result = eval(cl::car(body), env);
          body = cl::cdr(body);
        }

        return result;
      }));

  special_forms.insert(
      std::make_pair(get_symbol(SYM_QUOTE), [](Value args, Environment *env) {
        return cl::car(args);
      }));

  special_forms.insert(
      std::make_pair(get_symbol(SYM_SETQ), [](Value args, Environment *env) {
        Value value{};

        while (!is_nil(args)) {
          Value name = cl::car(args);
          args = cl::cdr(args);

          if (is_nil(args)) {
            assert(false);
          }

          value = eval(cl::car(args), env);
          args = cl::cdr(args);

          env->assign_variable(name, value);
        }

        return value;
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

      // TODO lexical

      if (name->is_fbound()) {
        Value function = name->get_function();

        return call_function(
            function, map_list([&env](Value arg) { return eval(arg, env); },
                               cons->get_cdr()));
      }

      assert(false);
    } else {
      throw NotImplemented{};
    }
  } else if (is_symbol(value)) {
    Symbol *symbol = get_symbol(value);

    if (symbol->is_constant()) {
      return symbol->get_value();
    }

    Value result{UNBOUND};
    if (symbol->is_special() || env->is_lexical_special(value)) {
      result = env->lookup_special(value);
    } else {
      result = env->lookup_variable(value);
      if (is_unbound(result)) {
        result = env->lookup_special(value);
      }
    }

    if (is_unbound(result)) {
      result = symbol->get_value();
      if (is_unbound(result)) {
        assert(false);
      }
    }

    // symbol-macro
    // throw NotImplemented();

    return result;
  } else {
    return value;
  }
}

} // namespace lisp