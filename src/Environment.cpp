#include "Environment.h"
#include "Binding.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "Package.h"
#include "Symbol.h"

namespace lisp {

DynamicBindings *dynamic_bindings;

Environment::Environment() : Environment{nullptr} {
  dynamic_bindings = make_dynamic_bindings();
}

Environment::Environment(Environment *parent) : Object{OBJ_ENVIRONMENT} {
  if (parent) {
    lexical_variables = parent->lexical_variables;
  }
}

void Environment::trace(bool marking) {
  mark(marking);
  trace_value(lexical_variables, marking);
  trace_value(dynamic_bindings->make_value(), marking);
}

Value Environment::lookup_variable(Value name) {
  return lookup_value(name, lexical_variables);
}

Value Environment::lookup_special(Value name) {
  return dynamic_bindings->lookup(name);
}

void Environment::bind_lexical_variable(Value name, Value value, bool special) {
  lexical_variables =
      make_cons_v(make_binding_v(name, value, special), lexical_variables);
}

bool Environment::is_lexical_special(Value name) {
  return lookup_binding(name, lexical_variables)->is_special();
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