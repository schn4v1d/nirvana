#include "Values.h"
#include "GarbageCollector.h"

namespace lisp {

void Values::trace(bool marking) {
  mark(marking);
  for (Value &v : values) {
    trace_value(v, marking);
  }
}

Values::Values(std::vector<Value> &&values)
    : Object{OBJ_VALUES}, values{std::move(values)} {}

const Value &Values::get_value(size_t i) const {
  if (values.empty()) {
    return NIL;
  } else {
    return values[i];
  }
}

const std::vector<Value> &Values::get_values() const { return values; }

bool is_values(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_VALUES;
  }

  return false;
}

Values *get_values(Value value) {
  return reinterpret_cast<Values *>(get_object(value));
}

Values *make_values(std::vector<Value> &&values) {
  return make_object<Values>(std::move(values));
}

Value make_values_v(std::vector<Value> &&values) {
  return make_values(std::move(values))->make_value();
}

} // namespace lisp
