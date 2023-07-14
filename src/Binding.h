#pragma once

#include "Object.h"

namespace lisp {

class Binding : public Object {
  Value name;
  Value value;
  bool special;

public:
  Binding(Value name, Value value, bool special);

  [[nodiscard]] Value get_name() const;
  [[nodiscard]] Value get_value() const;
  [[nodiscard]] bool is_special() const;

  void trace(bool marking) override;
};

bool is_binding(Value value);
Binding *get_binding(Value value);

Binding *make_binding(Value name, Value value, bool special);
Value make_binding_v(Value name, Value value, bool special);

} // namespace lisp
