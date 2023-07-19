#include "eval.h"
#include "Cons.h"
#include "MacroFunction.h"
#include "Package.h"
#include "Symbol.h"
#include "call_function.h"
#include "errors.h"
#include "special_operators.h"
#include <sstream>

namespace lisp {

void init_eval() {
  Symbol *current_package = get_symbol(SYM_STAR_PACKAGE_STAR);
  current_package->declare_special();
  current_package->set_value(PKG_CL_USER->make_value());

  init_special_operators();
}

Value eval(Value value, Environment *env) {
  if (is_cons(value)) {
    Cons *cons = get_cons(value);

    Value op = cons->get_car();

    if (is_special_operator(op)) {
      return get_special_operator(op)(cons->get_cdr(), env);
    }

    Value function = env->get_function(op);

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