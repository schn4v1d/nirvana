#include "Binding.h"
#include "GarbageCollector.h"

namespace lisp {

Binding::Binding(Value name, Value value, bool special)
    : Object{OBJ_BINDING}, name{name}, value{value}, special{special} {}

Value Binding::get_name() const { return name; }

Value Binding::get_value() const { return value; }

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

} // namespace lisp
