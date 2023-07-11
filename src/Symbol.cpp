#include "Symbol.h"

namespace lisp {

Symbol::Symbol(std::string_view name, Value package)
    : Object{OBJ_SYMBOL}, name{name}, package{package} {}

void Symbol::trace(bool marking) {
  mark(marking);
  trace_value(package, marking);
}

bool is_symbol(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_SYMBOL;
  }
  return false;
}

Symbol *get_symbol(Value value) {
  return reinterpret_cast<Symbol *>(get_object(value));
}

} // namespace lisp