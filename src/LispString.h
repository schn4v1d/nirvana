#pragma once

#include "Object.h"
#include <string>

namespace lisp {

class LispString : public Object {
  std::string content;

public:
  explicit LispString(std::string content);

  void trace(bool marking) override;

  std::string &get_content();
  [[nodiscard]] const std::string &get_content() const;
};

bool is_string(Value value);
LispString *get_string(Value value);
LispString *make_string(std::string content);
Value make_string_v(std::string content);

} // namespace lisp
