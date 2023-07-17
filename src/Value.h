#pragma once

#include <cinttypes>
#include <ostream>

namespace lisp {

union Value {
  std::uint64_t tag;
  std::int32_t integer[2];
  class Object *object;

  explicit operator bool() const;

  bool operator<(const Value& other) const;
};

const std::uint64_t TAG_MASK = 0b11;
const std::uint64_t TAG_NIL = 0b00;
const std::uint64_t TAG_INTEGER = 0b01;
const std::uint64_t TAG_UNBOUND = 0b10;
const std::uint64_t TAG_OBJECT = 0b11;
const std::uint64_t PTR_MASK = ~TAG_MASK;

const Value NIL{TAG_NIL};
const Value UNBOUND{TAG_UNBOUND};
extern Value T;

Value make_value(std::int32_t integer);
Value make_value(class Object *object);

bool is_object(Value value);
Object *get_object(Value value);

bool is_nil(Value value);
bool is_unbound(Value value);

bool is_integer(Value value);
std::int32_t get_integer(Value value);

void trace_value(Value value, bool marking);

std::ostream &operator<<(std::ostream &os, const Value &value);

bool operator==(Value lhs, Value rhs);

} // namespace lisp