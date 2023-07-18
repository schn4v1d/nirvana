#include "Object.h"
#include <sstream>

namespace lisp {

void Object::mark(bool marking) { marked = marking; }

bool Object::get_marked() const { return marked; }

obj_tag Object::get_tag() const { return tag; }

Object::Object(obj_tag tag) : tag{tag} {}

std::ostream &Object::print(std::ostream &os) const { return os << "#<obj>"; }

std::string Object::print_to_string() const {
  std::ostringstream oss{};
  print(oss);
  return oss.str();
}

} // namespace lisp