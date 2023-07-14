#pragma once

#include "DynamicBindings.h"
#include "Object.h"

namespace lisp {

class Environment : public Object {
  Value lexical_variables{NIL};

public:
  explicit Environment();
  explicit Environment(Environment *parent);

  void trace(bool marking) override;

  Value lookup_variable(Value name);
  Value lookup_special(Value name);
  void bind_lexical_variable(Value name, Value value, bool special = false);
  bool is_lexical_special(Value name);
};

bool is_environment(Value value);
Environment *get_environment(Value value);

Environment *make_environment();
Environment *make_environment(Environment *parent);
Value make_environment_v();
Value make_environment_v(Environment *parent);

} // namespace lisp
