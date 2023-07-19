#include "Cons.h"
#include "Environment.h"
#include "Symbol.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

std::vector<Value> process_form(Value form, Environment *env) {
  if (!is_cons(form)) {
    return {form};
  }

  Cons *cons = get_cons(form);

  if (SYM_UNQUOTE == cons->get_car()) {
    return {eval(get_cons(cons->get_cdr())->get_car(), env)};
  }

  if (SYM_UNQUOTE_SPLICING == cons->get_car()) {
    Cons *splice = get_cons(eval(get_cons(cons->get_cdr())->get_car(), env));
    std::vector<Value> result{};

    for (const auto &subform : *splice) {
      result.push_back(subform);
    }

    return result;
  }

  std::vector<Value> forms{};

  map_list(
      [&](Value form) {
        auto result = process_form(form, env);
        forms.insert(forms.end(), result.begin(), result.end());
        return NIL;
      },
      form);

  return {list_from_cppvector(std::move(forms))};
}

Value op_backquote(Value args, Environment *env) {
  return process_form(cl::car(args), env)[0];
}

} // namespace lisp::special_operators