#include "BuiltinFunction.h"

#include "GarbageCollector.h"
#include "Package.h"
#include "Symbol.h"
#include "cl_fun.h"
#include <iostream>
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
  return make_object<BuiltinFunction>(function);
}

Value make_builtin_function_v(std::function<Value(Value)> function) {
  return make_builtin_function(std::move(function))->make_value();
}

void init_builtin_functions() {
  get_symbol(PKG_CL->intern("NULL", true))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        if (is_nil(cl::first(args))) {
          return T;
        } else {
          return NIL;
        }
      }));

  get_symbol(PKG_CL->intern("CAR", true))
      ->set_function(make_builtin_function_v(
          [](Value args) -> Value { return cl::car(cl::first(args)); }));

  get_symbol(PKG_CL->intern("CDR", true))
      ->set_function(make_builtin_function_v(
          [](Value args) -> Value { return cl::cdr(cl::first(args)); }));

  get_symbol(PKG_CL->add_external_symbol("FIND-PACKAGE"))
      ->set_function(make_builtin_function_v([](Value args) -> Value {
        Value name = cl::first(args);

        return find_package(name);
      }));
}

} // namespace lisp