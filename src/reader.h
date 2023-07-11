#pragma once

#include "Value.h"
#include <sstream>

namespace lisp {

void init_reader();

enum class syntax_type {
  constituent,
  invalid,
  macro_terminating,
  macro_non_terminating,
  multiple_escape,
  single_escape,
  whitespace,
};

Value read(std::istringstream &input);

void skip_whitespace(std::istringstream &input);

} // namespace lisp