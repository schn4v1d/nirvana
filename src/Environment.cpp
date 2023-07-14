#include "Environment.h"
#include "Binding.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "Package.h"
#include "Symbol.h"

namespace lisp {

Environment::Environment() : Environment{nullptr} {}

Environment::Environment(Environment *parent) : Object{OBJ_ENVIRONMENT} {
  if (parent) {
    variables = parent->variables;
    functions = parent->functions;
  }
}

void Environment::trace(bool marking) {
  mark(marking);
  trace_value(variables, marking);
  trace_value(functions, marking);
}

Value lookup_value(Value name, Value bindings) {
  return iter_list(
      [name](Value bindingv) -> std::optional<Value> {
        Binding *binding = get_binding(bindingv);
        if (binding->get_name() == name) {
          return std::optional{binding->get_value()};
        } else {
          return std::nullopt;
        }
      },
      bindings);
}

Value Environment::lookup_variable(Value name) {
  return lookup_value(name, variables);
}

Value Environment::lookup_function(Value name) {
  return lookup_value(name, functions);
}

Value Environment::lookup_special(Value name) {
  return lookup_value(name, specials);
}

void Environment::bind(Value name, Value value, bool special) {
  variables = make_cons_v(make_binding_v(name, value, special), variables);
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