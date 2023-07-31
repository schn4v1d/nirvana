#include "Character.h"
#include "GarbageCollector.h"

namespace lisp {

Character::Character(char character)
    : Object{OBJ_CHARACTER}, character{character} {}

void Character::trace(bool marking) { mark(marking); }

char Character::get_character() const { return character; }

bool is_character(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_CHARACTER;
  }

  return false;
}

Character *get_character(Value value) {
  return reinterpret_cast<Character *>(get_object(value));
}

Character *make_character(char character) {
  return make_object<Character>(character);
}

Value make_character_v(char character) {
  return make_character(character)->make_value();
}

} // namespace lisp
