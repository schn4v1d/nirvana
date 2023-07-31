#pragma once

#include "Object.h"

namespace lisp {

class Character: public Object {
  char character;

public:
  explicit Character(char character);

  void trace(bool marking) override;

  [[nodiscard]] char get_character() const;
};

bool is_character(Value value);
Character *get_character(Value value);

Character *make_character(char character);
Value make_character_v(char character);

} // namespace lisp
