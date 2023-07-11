#include "Value.h"
#include "Object.h"

namespace lisp {

Value make_value(std::int32_t integer) {
  Value v{};
  v.integer[1] = integer;
  v.tag |= TAG_INTEGER;
  return v;
}

Value make_value(struct Object *object) { return object->make_value(); }

bool is_object(Value value) { return (value.tag & TAG_MASK) == TAG_OBJECT; }

Object *get_object(Value value) {
  return reinterpret_cast<Object *>(
      reinterpret_cast<std::uintptr_t>(value.object) & PTR_MASK);
}

void trace_value(Value value, bool marking) {
  if (is_object(value)) {
    Object *obj = get_object(value);
    obj->trace(marking);
  }
}

bool is_nil(Value value) { return (value.tag & TAG_MASK) == TAG_NIL; }

bool is_integer(Value value) { return (value.tag & TAG_MASK) == TAG_INTEGER; }

std::int32_t get_integer(Value value) { return value.integer[1]; }

std::ostream &operator<<(std::ostream &os, const Value &value) {
  if (is_nil(value)) {
    return os << "nil";
  } else if (is_integer(value)) {
    return os << get_integer(value);
  } else if (is_object(value)) {
    return get_object(value)->print(os);
  } else {
    return os;
  }
}

Value T;

} // namespace lisp