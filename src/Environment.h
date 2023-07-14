#pragma once

#include "Object.h"

namespace lisp {

class Environment : public Object {
  Value variables;
  Value functions;
  Value specials;

public:
  explicit Environment();
  explicit Environment(Environment *parent);

  void trace(bool marking) override;

  Value lookup_variable(Value name);
  Value lookup_function(Value name);
  Value lookup_special(Value name);
  void bind(Value name, Value value, bool special = false);
};

bool is_environment(Value value);
Environment *get_environment(Value value);

Environment *make_environment();
Environment *make_environment(Environment *parent);
Value make_environment_v();
Value make_environment_v(Environment *parent);

} // namespace lisp
