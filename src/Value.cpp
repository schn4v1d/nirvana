#include "Value.h"
#include "Object.h"

namespace lisp {

Value make_value(std::int64_t integer) { return {.integer = integer}; }

Value make_value(struct Object *object) { return object->make_value(); }

bool is_object(Value value) { return value.tag & TAG_OBJECT; }

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

bool is_nil(Value value) { return value.tag & TAG_NIL; }

bool is_cons(Value value) { return value.tag & TAG_CONS; }

Cons *get_cons(Value value) {
  return reinterpret_cast<Cons *>(reinterpret_cast<std::uintptr_t>(value.cons) &
                                  PTR_MASK);
}

} // namespace lisp