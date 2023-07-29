#include "GarbageCollector.h"
#include "LispString.h"

namespace lisp {

LispString::LispString(std::string content)
    : Object{OBJ_STRING}, content{std::move(content)} {}

void LispString::trace(bool marking) { mark(marking); }

std::ostream &LispString::print(std::ostream &os) const {
  return os << '"' << content << '"';
}

std::string &LispString::get_content() { return content; }

const std::string &LispString::get_content() const { return content; }

bool is_string(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_STRING;
  }

  return false;
}

LispString *get_string(Value value) {
  return reinterpret_cast<LispString *>(get_object(value));
}

LispString *make_string(std::string content) {
  return make_object<LispString>(std::move(content));
}

Value make_string_v(std::string content) {
  return make_string(std::move(content))->make_value();
}

} // namespace lisp
