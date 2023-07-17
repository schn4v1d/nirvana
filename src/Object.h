#pragma once

#include "Value.h"
#include <ostream>

namespace lisp {

using obj_tag = std::uint8_t;

const obj_tag OBJ_CONS = 0;
const obj_tag OBJ_SYMBOL = 1;
const obj_tag OBJ_PACKAGE = 2;
const obj_tag OBJ_ENVIRONMENT = 3;
const obj_tag OBJ_BINDING = 4;
const obj_tag OBJ_DYNAMIC_BINDINGS = 5;
const obj_tag OBJ_BUILTIN_FUNCTION = 6;

class Object {
  friend class GarbageCollector;
  friend void trace_value(Value value, bool marking);

  bool marked{false};
  obj_tag tag;

protected:
  void mark(bool marking);

public:
  explicit Object(obj_tag tag);
  virtual ~Object() = default;

  [[nodiscard]] bool get_marked() const;
  [[nodiscard]] obj_tag get_tag() const;

  virtual void trace(bool marking) = 0;

  virtual std::ostream& print(std::ostream& os);

  [[nodiscard]] inline Value make_value() const {
    return Value{.object = reinterpret_cast<Object *>(
                     reinterpret_cast<std::uintptr_t>(this) | TAG_OBJECT)};
  }
};

} // namespace lisp