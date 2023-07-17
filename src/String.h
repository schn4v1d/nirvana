#pragma once

#include "Object.h"

namespace lisp {

class String: public Object {
  std::string content;

public:
  explicit String(std::string content);

  void trace(bool marking) override;

  std::string &get_content();
  [[nodiscard]] const std::string &get_content() const;
};

bool is_string(Value value);
String *get_string(Value value);
String *make_string(std::string content);
Value make_string_v(std::string content);

} // namespace lisp
