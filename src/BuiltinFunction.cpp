#include "BuiltinFunction.h"

#include "Cons.h"
#include "GarbageCollector.h"
#include "Lambda.h"
#include "LispString.h"
#include "MacroFunction.h"
#include "OrdinaryLambdaList.h"
#include "Package.h"
#include "Symbol.h"
#include "Values.h"
#include "Vector.h"
#include "call_function.h"
#include "cl_fun.h"
#include "eval.h"
#include "reader.h"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <utility>

namespace lisp {

BuiltinFunction::BuiltinFunction(std::function<Value(Value)> function)
    : Object(OBJ_BUILTIN_FUNCTION), function{std::move(function)} {}

void BuiltinFunction::trace(bool marking) { mark(marking); }

Value BuiltinFunction::call(Value args) { return function(args); }

bool is_builtin_function(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_BUILTIN_FUNCTION;
  }

  return false;
}

BuiltinFunction *get_builtin_function(Value value) {
  return reinterpret_cast<BuiltinFunction *>(get_object(value));
}

BuiltinFunction *make_builtin_function(std::function<Value(Value)> function) {
  return make_object<BuiltinFunction>(std::move(function));
}

Value make_builtin_function_v(std::function<Value(Value)> function) {
  return make_builtin_function(std::move(function))->make_value();
}

static std::filesystem::path current_path{std::filesystem::current_path()};

void init_builtin_functions() {
  get_symbol(PKG_NIRVANA_BUILTINS->intern("%CONS", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        return make_cons_v(cl::first(args), cl::second(args));
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%CONSP", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        if (is_cons(cl::first(args))) {
          return T;
        } else {
          return NIL;
        }
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%NULL", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        if (is_nil(cl::first(args))) {
          return T;
        } else {
          return NIL;
        }
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%CAR", true))
      ->set_function(make_builtin_function_v(
          [](Value args) -> Value { return cl::car(cl::first(args)); }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%CDR", true))
      ->set_function(make_builtin_function_v(
          [](Value args) -> Value { return cl::cdr(cl::first(args)); }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%RPLACA", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Cons *cons = get_cons(cl::car(args));
        cons->set_car(cl::second(args));
        return cons->make_value();
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%RPLACD", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Cons *cons = get_cons(cl::car(args));
        cons->set_cdr(cl::second(args));
        return cons->make_value();
      }));

  get_symbol(PKG_CL->intern("INTEGERP", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        if (is_integer(cl::first(args))) {
          return T;
        } else {
          return NIL;
        }
      }));

  get_symbol(PKG_CL->intern("EQ", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        if (cl::first(args) == cl::second(args)) {
          return T;
        } else {
          return NIL;
        }
      }));

  get_symbol(PKG_CL->intern("ERROR", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        throw std::runtime_error(print_to_string(cl::car(args)));
      }));

  get_symbol(PKG_CL->intern("+", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        return make_value(get_integer(cl::first(args)) +
                          get_integer(cl::second(args)));
      }));

  get_symbol(PKG_CL->intern("-", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        return make_value(get_integer(cl::first(args)) -
                          get_integer(cl::second(args)));
      }));

  get_symbol(PKG_CL->intern("VECTOR", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Vector *vec = make_vector();

        if (is_cons(args)) {
          for (const auto &el : *get_cons(args)) {
            vec->push(el);
          }
        }

        return vec->make_value();
      }));

  get_symbol(PKG_CL->intern("AREF", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Vector *vec = get_vector(cl::first(args));
        std::int32_t i = get_integer(cl::second(args));

        return vec->get(i);
      }));

  get_symbol(PKG_CL->intern("VECTOR-PUSH-EXTEND", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value v = cl::first(args);
        Vector *vec = get_vector(cl::second(args));

        vec->push(v);

        return make_value((int)vec->size() - 1);
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->add_external_symbol("%FIND-PACKAGE"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value name = cl::first(args);

        return find_package(name);
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->add_external_symbol("%EXPORT"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value symbols = cl::first(args);
        if (is_nil(symbols))
          return T;
        if (!is_cons(symbols))
          symbols = make_cons_v(symbols, NIL);

        Package *package = coerce_to_package(cl::second(args));

        map_list(
            [&](Value sym) {
              package->export_symbol(get_symbol(sym));
              return NIL;
            },
            symbols);

        return T;
      }));

  get_symbol(PKG_CL->add_external_symbol("SYMBOLP"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        if (is_symbol(cl::car(args))) {
          return T;
        } else {
          return NIL;
        }
      }));

  get_symbol(PKG_CL->add_external_symbol("SYMBOL-FUNCTION"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        return get_symbol(cl::car(args))->get_function();
      }));

  get_symbol(PKG_CL->add_external_symbol("SYMBOL-NAME"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        return make_string_v(get_symbol(cl::car(args))->get_name());
      }));

  get_symbol(PKG_CL->add_external_symbol("SYMBOL-PACKAGE"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        return get_symbol(cl::car(args))->get_package();
      }));

  get_symbol(PKG_CL->add_external_symbol("SYMBOL-VALUE"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Symbol *sym = get_symbol(cl::car(args));
        Value v = sym->get_value();

        if (is_unbound(v)) {
          std::ostringstream oss{};
          oss << "unbound variable " << sym;
          std::string str{oss.str()};
          throw std::exception{str.c_str()};
        }

        return v;
      }));

  get_symbol(PKG_CL->add_external_symbol("BOUNDP"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Symbol *sym = get_symbol(cl::car(args));
        Value v = sym->get_value();

        if (is_unbound(v)) {
          return NIL;
        }

        return T;
      }));

  get_symbol(PKG_CL->add_external_symbol("MAKUNBOUND"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Symbol *sym = get_symbol(cl::car(args));

        sym->set_value(UNBOUND);

        return sym->make_value();
      }));

  get_symbol(PKG_CL->add_external_symbol("STRING"))
      ->set_function(make_builtin_function_v(
          [](Value args) -> Value { return cl::string(cl::car(args)); }));

  get_symbol(PKG_CL->add_external_symbol("STRINGP"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        if (is_string(cl::car(args))) {
          return T;
        } else {
          return NIL;
        }
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%DEFUN", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value name = cl::first(args);
        Value lambda = cl::second(args);

        get_symbol(name)->set_function(lambda);

        return name;
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->intern("%DEFMACRO", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value name = cl::first(args);
        Lambda *lambda = get_lambda(cl::second(args));

        get_symbol(name)->set_function(make_macro_function_v(lambda));

        return name;
      }));

  get_symbol(PKG_CL->add_external_symbol("PRINT"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value obj = cl::car(args);

        std::cout << obj << std::endl;

        return NIL;
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->add_external_symbol("%LOAD"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        std::filesystem::path old = current_path;
        Value package_before = get_symbol(SYM_STAR_PACKAGE_STAR)->get_value();

        if (std::filesystem::is_directory(current_path)) {
          current_path =
              current_path / get_string(cl::car(args))->get_content();
        } else {
          current_path = current_path.parent_path() /
                         get_string(cl::car(args))->get_content();
        }

        std::ifstream t{current_path};
        if (!t.is_open()) {
          throw std::exception("invalid load path");
        }
        std::stringstream buffer;
        buffer << t.rdbuf();
        std::istringstream input_stream{buffer.str()};

        Environment *env = make_environment();

        try {
          while (true) {
            Value v = read(input_stream, env);
            eval(v, env);
          }
        } catch (ReadEndOfFile &e) {
        }

        current_path = old;
        get_symbol(SYM_STAR_PACKAGE_STAR)->set_value(package_before);

        return T;
      }));

  get_symbol(PKG_CL->add_external_symbol("VALUES"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        std::vector<Value> values{};

        map_list(
            [&](Value v) {
              values.push_back(v);
              return NIL;
            },
            args);

        return make_values_v(std::move(values));
      }));

  get_symbol(PKG_NIRVANA_BUILTINS->add_external_symbol("%FUNCALL"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value function = cl::car(args);
        Value call_args = cl::cadr(args);

        return call_function(function, call_args);
      }));
}

} // namespace lisp