#include "Value.h"
#include "Object.h"

namespace lisp {

Value::operator bool() const { return !is_nil(*this); }

bool Value::operator<(const Value &other) const { return tag < other.tag; }

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
    if (obj->get_marked() == marking)
      return;
    obj->trace(marking);
  }
}

bool is_nil(Value value) { return (value.tag & TAG_MASK) == TAG_NIL; }

bool is_unbound(Value value) { return (value.tag & TAG_MASK) == TAG_UNBOUND; }

bool is_integer(Value value) { return (value.tag & TAG_MASK) == TAG_INTEGER; }

std::int32_t get_integer(Value value) { return value.integer[1]; }

std::ostream &operator<<(std::ostream &os, const Value &value) {
  if (is_nil(value)) {
    return os << "NIL";
  } else if (is_integer(value)) {
    return os << get_integer(value);
  } else if (is_object(value)) {
    return get_object(value)->print(os);
  } else {
    return os;
  }
}

bool operator==(Value lhs, Value rhs) {
  uint64_t lhs_tag = lhs.tag & TAG_MASK;
  uint64_t rhs_tag = rhs.tag & TAG_MASK;
  switch (lhs_tag) {
  case TAG_NIL:
    return rhs_tag == TAG_NIL;
  case TAG_INTEGER:
    return rhs_tag == TAG_INTEGER && get_integer(lhs) == get_integer(rhs);
  case TAG_OBJECT:
    return rhs_tag == TAG_OBJECT && get_object(lhs) == get_object(rhs);
  default:
    return false;
  }
}

Value T;

} // namespace lisp