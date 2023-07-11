#include "GarbageCollector.h"

namespace lisp {

GarbageCollector &GarbageCollector::get() {
  static GarbageCollector gc{};

  return gc;
}

void GarbageCollector::mark(std::ranges::input_range auto &&roots) {
  for (Object *root : roots) {
    if (root->get_marked() == color) {
      continue;
    }

    root->trace(color);
  }
}

void GarbageCollector::sweep() {
  for (auto it = objects.begin(); it != objects.end();) {
    if ((*it)->get_marked() != color) {
      delete *it;

      std::iter_swap(it, objects.end() - 1);
      objects.pop_back();
    }
  }

  color = !color;
}

} // namespace lisp