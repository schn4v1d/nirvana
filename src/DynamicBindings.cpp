#include "DynamicBindings.h"
#include "Binding.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "cl_fun.h"

namespace lisp {

DynamicBindings::DynamicBindings() : Object{OBJ_DYNAMIC_BINDINGS} {}

void DynamicBindings::trace(bool marking) {
  mark(marking);
  trace_value(bindings, marking);
}

Value DynamicBindings::checkpoint() { return bindings; }

void DynamicBindings::push_binding(Value name, Value value) {
  bindings = make_cons_v(make_binding_v(name, value, false), bindings);
}

void DynamicBindings::restore_checkpoint(Value checkpoint) {
  while (!is_nil(bindings) && bindings != checkpoint) {
    bindings = cl::cdr(bindings);
  }
}

Value DynamicBindings::lookup(Value name) {
  return lookup_value(name, bindings);
}

bool is_dynamic_bindings(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_DYNAMIC_BINDINGS;
  }

  return false;
}

DynamicBindings *get_dynamic_bindings(Value value) {
  return reinterpret_cast<DynamicBindings *>(get_object(value));
}

DynamicBindings *make_dynamic_bindings() {
  return make_object<DynamicBindings>();
}

Value make_dynamic_bindings_v() {
  return make_dynamic_bindings()->make_value();
}

} // namespace lisp
