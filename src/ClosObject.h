#pragma once

#include "Object.h"
#include <unordered_map>

namespace lisp {

class ClosObject: public Object {
  std::unordered_map<Value, Value> slots;
  Value obj_class;

public:
  explicit ClosObject(Value obj_class);
};

} // namespace lisp
