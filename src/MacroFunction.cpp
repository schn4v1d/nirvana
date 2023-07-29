#include "MacroFunction.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "eval.h"
#include <iostream>

namespace lisp {

MacroFunction::MacroFunction(Lambda *lambda)
    : Object{OBJ_MACRO_FUNCTION}, lambda{lambda} {}

void MacroFunction::trace(bool marking) {
  mark(marking);
  trace_value(lambda->make_value(), marking);
}

Value MacroFunction::expand(Value form, Environment *env) {
  Value expansion = lambda->call(list_from_cppvector({form, env->make_value()}));
//  std::cout << "Expanding: " << form << std::endl;
//  std::cout << "Expanded to: " << expansion << std::endl;
  return expansion;
}

bool is_macro_function(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_MACRO_FUNCTION;
  }

  return false;
}

MacroFunction *get_macro_function(Value value) {
  return reinterpret_cast<MacroFunction *>(get_object(value));
}

MacroFunction *make_macro_function(Lambda *lambda) {
  return make_object<MacroFunction>(lambda);
}

Value make_macro_function_v(Lambda *lambda) {
  return make_macro_function(lambda)->make_value();
}

} // namespace lisp