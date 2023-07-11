#pragma once

#include "Object.h"
#include <type_traits>
#include <vector>

namespace lisp {

class GarbageCollector {
  std::vector<Object *> objects;

public:
  template <class T, class... Args> T *make_object(Args &&...args) {
    static_assert(std::is_base_of<Object, T>::value,
                  "T must be a subclass of Object");
    T *object = new T(args...);
    objects.push_back(object);
    return object;
  }

  static GarbageCollector &get();
};

template <class T, class... Args> T *make_object(Args &&...args) {
  return GarbageCollector::get().make_object<T>(args...);
}

template <class T, class... Args> Value make_object_v(Args &&...args) {
  return GarbageCollector::get().make_object<T>(args...)->make_value();
}

} // namespace lisp
