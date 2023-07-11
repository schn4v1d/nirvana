#pragma once

#include "Value.h"

namespace lisp {

using obj_tag = std::uint8_t;

const obj_tag OBJ_CONS = 0;
const obj_tag OBJ_SYMBOL = 1;
const obj_tag OBJ_PACKAGE = 2;

class Object {
  friend class GarbageCollector;
  friend void trace_value(Value value, bool marking);

  bool marked{false};
  obj_tag tag;

protected:
  void mark(bool marking);
  virtual void trace(bool marking) = 0;

public:
  explicit Object(obj_tag tag);

  [[nodiscard]] bool get_marked() const;
  [[nodiscard]] obj_tag get_tag() const;

  inline Value make_value() {
    return Value{.object = reinterpret_cast<Object *>(
                     reinterpret_cast<std::uintptr_t>(this) | TAG_OBJECT)};
  }
};

} // namespace lisp