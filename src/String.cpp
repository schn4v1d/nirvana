#include "String.h"
#include "GarbageCollector.h"

namespace lisp {

void String::trace(bool marking) { mark(marking); }

String::String(std::string content)
    : Object{OBJ_STRING}, content{std::move(content)} {}

std::string &String::get_content() { return content; }

const std::string &String::get_content() const { return content; }

bool is_string(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_STRING;
  }

  return false;
}

String *get_string(Value value) {
  return reinterpret_cast<String *>(get_object(value));
}

String *make_string(std::string content) {
  return make_object<String>(std::move(content));
}

Value make_string_v(std::string content) {
  return make_string(std::move(content))->make_value();
}

} // namespace lisp
