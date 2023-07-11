#pragma once

#include <cinttypes>

namespace lisp {

union Value {
  std::uintptr_t tag;
  std::int64_t integer;
  class Cons *cons;
  class Object *object;
};

const std::uintptr_t TAG_MASK = 0b11;
const std::uintptr_t TAG_NIL = 0b00;
const std::uintptr_t TAG_INTEGER = 0b01;
const std::uintptr_t TAG_CONS = 0b10;
const std::uintptr_t TAG_OBJECT = 0b11;
const std::uintptr_t PTR_MASK = ~TAG_MASK;

const Value NIL{0};

Value make_value(std::int64_t integer);
Value make_value(class Object *object);

bool is_cons(Value value);
Cons *get_cons(Value value);

bool is_object(Value value);
Object *get_object(Value value);

bool is_nil(Value value);

void trace_value(Value value, bool marking);

} // namespace lisp