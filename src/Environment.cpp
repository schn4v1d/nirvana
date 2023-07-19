#include "Environment.h"
#include "Binding.h"
#include "Block.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "Symbol.h"

namespace lisp {

DynamicBindings *dynamic_bindings;

Environment::Environment() : Environment{nullptr} {
  lisp::dynamic_bindings = make_dynamic_bindings();
  dynamic_bindings = lisp::dynamic_bindings;
}

Environment::Environment(Environment *parent) : Object{OBJ_ENVIRONMENT} {
  if (parent) {
    lexical_variables = parent->lexical_variables;
    lexical_functions = parent->lexical_functions;
    dynamic_bindings = parent->dynamic_bindings;
    blocks = parent->blocks;
  }
}

void Environment::trace(bool marking) {
  mark(marking);
  trace_value(lexical_variables, marking);
  trace_value(lexical_functions, marking);
  trace_value(dynamic_bindings->make_value(), marking);
}

Value Environment::lookup_variable(Value name) {
  return lookup_value(name, lexical_variables);
}

Value Environment::lookup_function(Value name) {
  return lookup_value(name, lexical_functions);
}

Value Environment::lookup_special(Value name) {
  return dynamic_bindings->lookup_value(name);
}

Value Environment::lookup_block(Value name) {
  Value result = iter_list(
      [name](Value blockv) -> std::optional<Value> {
        Block *block = get_block(blockv);
        if (block->get_name() == name) {
          return blockv;
        } else {
          return std::nullopt;
        }
      },
      blocks);

  if (is_unbound(result)) {
    return NIL;
  } else {
    return result;
  }
}

void Environment::bind_lexical_variable(Value name, Value value, bool special) {
  lexical_variables =
      make_cons_v(make_binding_v(name, value, special), lexical_variables);
}

void Environment::bind_lexical_function(Value name, Value value, bool special) {
  lexical_functions =
      make_cons_v(make_binding_v(name, value, special), lexical_functions);
}

bool Environment::is_lexical_special(Value name) {
  Binding *binding = lookup_binding(name, lexical_variables);

  if (binding) {
    return binding->is_special();
  } else {
    return false;
  }
}

void Environment::assign_variable(Value name, Value value) {
  Symbol *symbol = get_symbol(name);

  if (symbol->is_special()) {
    symbol->set_value(value);
  } else if (is_lexical_special(name)) {
    dynamic_bindings->lookup_binding(name)->set_value(value);
  } else {
    Binding *binding = lookup_binding(name, lexical_variables);
    if (binding) {
      binding->set_value(value);
    } else {
      symbol->set_value(value);
    }
  }
}

Block *Environment::establish_block(Value name) {
  Block *block =
      make_block(name, dynamic_bindings->push_frame(BlockFrame{name}));
  blocks = make_cons_v(block->make_value(), blocks);
  return block;
}

Frame *Environment::establish_unwind_protect(Value cleanup_forms) {
  return dynamic_bindings->push_frame(UnwindProtectFrame{cleanup_forms, this});
}

void Environment::unwind(Frame *frame, bool inclusive) {
  dynamic_bindings->unwind(frame, inclusive);
}

bool is_environment(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_ENVIRONMENT;
  }

  return false;
}

Environment *get_environment(Value value) {
  return reinterpret_cast<Environment *>(get_object(value));
}

Environment *make_environment() { return make_object<Environment>(); }

Environment *make_environment(Environment *parent) {
  return make_object<Environment>(parent);
}

Value make_environment_v() { return make_environment()->make_value(); }

Value make_environment_v(Environment *parent) {
  return make_environment(parent)->make_value();
}

} // namespace lisp