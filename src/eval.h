#pragma once

#include "object.h"

namespace lisp::eval {
  using Object = object::Object;

  Object eval(Object &obj, object::MemoryManager& mem);
}