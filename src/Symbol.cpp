#include "Symbol.h"
#include "GarbageCollector.h"
#include "Package.h"

namespace lisp {

Symbol::Symbol(std::string_view name, Value package)
    : Object{OBJ_SYMBOL}, name{name}, package{package} {}

void Symbol::trace(bool marking) {
  if (get_marked() == marking)
    return;

  mark(marking);

  trace_value(package, marking);
}

Value Symbol::get_value() { return value; }

void Symbol::set_value(Value new_value) {
  value = new_value;
  flags.bound = true;
}

bool Symbol::is_bound() const { return flags.bound; }

std::ostream &Symbol::print(std::ostream &os) { return os << name; }

bool is_symbol(Value value) {
  if (is_nil(value)) {
    return true;
  }

  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_SYMBOL;
  }

  return false;
}

Symbol *get_symbol(Value value) {
  if (is_nil(value)) {
    value = SYM_NIL;
  }

  return reinterpret_cast<Symbol *>(get_object(value));
}

Symbol *make_symbol(std::string_view name, Value package) {
  return make_object<Symbol>(name, package);
}

Value make_symbol_v(std::string_view name, Value package) {
  return make_symbol(name, package)->make_value();
}

Value SYM_NIL;
Value SYM_T;

void init_symbols() {
  SYM_NIL = PKG_CL->add_external_symbol("NIL");
  get_symbol(SYM_NIL)->set_value(NIL);

  SYM_T = PKG_CL->add_external_symbol("T");
  get_symbol(SYM_T)->set_value(SYM_T);
  T = SYM_T;
}

} // namespace lisp