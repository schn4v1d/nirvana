#include "Object.h"

namespace lisp {

void Object::mark(bool marking) { marked = marking; }

bool Object::get_marked() const { return marked; }

obj_tag Object::get_tag() const { return tag; }

Object::Object(obj_tag tag) : tag{tag} {}

} // namespace lisp