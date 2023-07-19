#pragma once

#include "Block.h"
#include "DynamicBindings.h"
#include "Object.h"

namespace lisp {

class Environment : public Object {
  Value lexical_variables{NIL};
  Value lexical_functions{NIL};
  Value blocks{NIL};
  DynamicBindings *dynamic_bindings;

public:
  explicit Environment();
  explicit Environment(Environment *parent);

  void trace(bool marking) override;

  Value lookup_variable(Value name);
  Value lookup_function(Value name);
  Value lookup_special(Value name);
  Value lookup_block(Value name);
  Frame *lookup_catch(Value tag);
  void bind_lexical_variable(Value name, Value value, bool special = false);
  void bind_lexical_function(Value name, Value value, bool special = false);
  bool is_lexical_special(Value name);
  void assign_variable(Value name, Value value);
  Block *establish_block(Value name);
  Frame *establish_unwind_protect(Value cleanup_forms);
  Frame *establish_catch(Value tag);
  void unwind(Frame *frame, bool inclusive = true);
};

bool is_environment(Value value);
Environment *get_environment(Value value);

Environment *make_environment();
Environment *make_environment(Environment *parent);
Value make_environment_v();
Value make_environment_v(Environment *parent);

} // namespace lisp
