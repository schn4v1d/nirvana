#include "Binding.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include <optional>

namespace lisp {

Binding::Binding(Value name, Value value, bool special)
    : Object{OBJ_BINDING}, name{name}, value{value}, special{special} {}

Value Binding::get_name() const { return name; }

Value Binding::get_value() const { return value; }

void Binding::set_value(Value new_value) { value = new_value; }

bool Binding::is_special() const { return special; }

void Binding::trace(bool marking) {
  mark(marking);
  trace_value(name, marking);
  trace_value(value, marking);
}

bool is_binding(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_BINDING;
  }

  return false;
}

Binding *get_binding(Value value) {
  return reinterpret_cast<Binding *>(get_object(value));
}

Binding *make_binding(Value name, Value value, bool special) {
  return make_object<Binding>(name, value, special);
}

Value make_binding_v(Value name, Value value, bool special) {
  return make_binding(name, value, special)->make_value();
}

Binding *lookup_binding(Value name, Value bindings) {
  Value result = iter_list(
      [name](Value bindingv) -> std::optional<Value> {
        Binding *binding = get_binding(bindingv);
        if (binding->get_name() == name) {
          return std::optional{bindingv};
        } else {
          return std::nullopt;
        }
      },
      bindings);

  if (is_unbound(result)) {
    return nullptr;
  } else {
    return get_binding(result);
  }
}

Value lookup_value(Value name, Value bindings) {
  Binding *binding = lookup_binding(name, bindings);
  if (binding) {
    return binding->get_value();
  } else {
    return UNBOUND;
  }
}

} // namespace lisp
