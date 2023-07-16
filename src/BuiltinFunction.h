#pragma once

#include "Object.h"
#include <functional>

namespace lisp {

class BuiltinFunction: public Object {
  std::function<Value(Value args)> function;

public:
  explicit BuiltinFunction(std::function<Value(Value args)> function);

  Value call(Value args);

  void trace(bool marking) override;
};

bool is_builtin_function(Value value);
BuiltinFunction *get_builtin_function(Value value);

BuiltinFunction *make_builtin_function(std::function<Value(Value args)> function);
Value make_builtin_function_v(std::function<Value(Value args)> function);

void init_builtin_functions();

} // namespace lisp
