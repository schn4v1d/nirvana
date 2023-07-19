#include "eval.h"
#include "Cons.h"
#include "Function.h"
#include "Lambda.h"
#include "MacroFunction.h"
#include "OrdinaryLambdaList.h"
#include "Package.h"
#include "Symbol.h"
#include "cl_fun.h"
#include "errors.h"
#include "util.h"
#include <cassert>
#include <sstream>
#include <unordered_map>

namespace lisp {

Value get_function(Value op, Environment *env) {
  if (is_symbol(op)) {

    Value function = env->lookup_function(op);

    if (is_unbound(function)) {
      Symbol *name = get_symbol(op);

      if (name->is_fbound()) {
        function = name->get_function();
      } else {
        throw UndefinedFunction{name->get_name()};
      }
    }

    return function;
  } else if (is_cons(op) && cl::car(op) == SYM_LAMBDA) {
    OrdinaryLambdaList lambda_list{cl::second(op)};
    Value body = cl::cddr(op);

    return make_lambda_v(std::move(lambda_list), env, body);
  } else {
    throw std::exception{"invalid function"};
  }
}

std::unordered_map<Symbol *, std::function<Value(Value args, Environment *env)>>
    special_forms{};

void init_eval() {
  Symbol *current_package = get_symbol(SYM_STAR_PACKAGE_STAR);
  current_package->declare_special();
  current_package->set_value(PKG_CL_USER->make_value());

  special_forms.insert(std::make_pair(
      get_symbol(SYM_BACKQUOTE), [](Value args, Environment *env) {
        std::function<std::vector<Value>(Value)> process_form =
            [&](Value form) -> std::vector<Value> {
          if (!is_cons(form)) {
            return {form};
          }

          Cons *cons = get_cons(form);

          if (SYM_UNQUOTE == cons->get_car()) {
            return {eval(get_cons(cons->get_cdr())->get_car(), env)};
          }

          if (SYM_UNQUOTE_SPLICING == cons->get_car()) {
            Cons *splice =
                get_cons(eval(get_cons(cons->get_cdr())->get_car(), env));
            std::vector<Value> result{};

            for (const auto &subform : *splice) {
              result.push_back(subform);
            }

            return result;
          }

          std::vector<Value> forms{};

          map_list(
              [&](Value form) {
                auto result = process_form(form);
                forms.insert(forms.end(), result.begin(), result.end());
                return NIL;
              },
              form);

          return {list_from_cppvector(std::move(forms))};
        };

        return process_form(cl::car(args))[0];
      }));

  special_forms.insert(std::make_pair(
      get_symbol(SYM_BLOCK), [](Value args, Environment *parent) {
        Value result{NIL};

        Environment *env = make_environment(parent);

        Value name = cl::car(args);

        Block *block = env->establish_block(name);

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

  special_forms.insert(std::make_pair(get_symbol(SYM_FUNCTION),
                                      [](Value args, Environment *env) {
                                        Value designator = cl::car(args);

                                        return get_function(designator, env);
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

  special_forms.insert(std::make_pair(
      get_symbol(SYM_RETURN_FROM), [](Value args, Environment *env) {
        Value block_name = cl::first(args);

        Value return_value = eval(cl::second(args), env);

        Value blockv = env->lookup_block(block_name);
        if (is_nil(blockv)) {
          std::ostringstream oss{};
          oss << "no block named " << block_name << " in current environment";
          throw std::runtime_error{oss.str()};
        }

        Block *block = get_block(blockv);

        block->set_return_value(return_value);

        env->unwind(block->get_frame(), false);

        longjmp(*block->get_jmp_buf(), 1);

        return NIL;
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

  special_forms.insert(std::make_pair(
      get_symbol(SYM_UNWIND_PROTECT), [](Value args, Environment *env) {
        Value protected_form = cl::car(args);
        Value cleanup_forms = cl::cdr(args);

        Frame *frame = env->establish_unwind_protect(cleanup_forms);

        Value result = eval(protected_form, env);

        env->unwind(frame);

        return result;
      }));
}

Value eval(Value value, Environment *env) {
  if (is_cons(value)) {
    Cons *cons = get_cons(value);

    Value op = cons->get_car();

    if (is_symbol(op)) {
      auto special_form = special_forms.find(get_symbol(op));
      if (special_form != special_forms.end()) {
        return (*special_form).second(cons->get_cdr(), env);
      }
    }

    Value function = get_function(op, env);

    if (is_macro_function(function)) {
      return eval(get_macro_function(function)->expand(cons->get_cdr(), env),
                  env);
    }

    return call_function(function,
                         map_list([&env](Value arg) { return eval(arg, env); },
                                  cons->get_cdr()));
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
        throw UnboundVariable{symbol->get_name()};
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