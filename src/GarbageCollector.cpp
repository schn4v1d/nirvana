#include "GarbageCollector.h"

namespace lisp {

GarbageCollector &GarbageCollector::get() {
  static GarbageCollector gc{};

  return gc;
}

} // namespace lisp