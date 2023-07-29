#include "Vector.h"
#include "GarbageCollector.h"

namespace lisp {

Vector::Vector() : Object{OBJ_VECTOR} {}

void Vector::trace(bool marking) {
  mark(marking);

  for (auto &v : elements) {
    trace_value(v, marking);
  }
}

std::ostream &Vector::print(std::ostream &os) const {
  os << "#a(";
  for (auto it = elements.begin(); it != elements.end(); ++it) {
    os << *it;
    if (it + 1 != elements.end()) {
      os << ' ';
    }
  }
  return os << ")";
}

Value Vector::get(size_t index) { return elements[index]; }

void Vector::push(Value value) { elements.push_back(value); }

std::size_t Vector::size() const { return elements.size(); }

bool is_vector(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_VECTOR;
  }

  return false;
}

Vector *get_vector(Value value) {
  return reinterpret_cast<Vector *>(get_object(value));
}

Vector *make_vector() { return make_object<Vector>(); }

Value make_vector_v() { return make_vector()->make_value(); }

} // namespace lisp
