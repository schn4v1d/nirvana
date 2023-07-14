#pragma once

#include "Object.h"

namespace lisp {

class DynamicBindings: public Object {
  Value bindings{NIL};

public:
  DynamicBindings();

  void trace(bool marking) override;

  Value checkpoint();
  Value lookup(Value name);
  void push_binding(Value name, Value value);
  void restore_checkpoint(Value checkpoint);
};

bool is_dynamic_bindings(Value value);
DynamicBindings *get_dynamic_bindings(Value value);

DynamicBindings *make_dynamic_bindings();
Value make_dynamic_bindings_v();

} // namespace lisp
