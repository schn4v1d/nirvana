#include "Symbol.h"
#include "GarbageCollector.h"
#include "Package.h"

namespace lisp {

Symbol::Symbol(std::string_view name, Value package)
    : Object{OBJ_SYMBOL}, name{name}, package{package} {}

void Symbol::trace(bool marking) {
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
Value SYM_BLOCK;
Value SYM_CATCH;
Value SYM_EVAL_WHEN;
Value SYM_FLET;
Value SYM_FUNCTION;
Value SYM_GO;
Value SYM_IF;
Value SYM_LABELS;
Value SYM_LET;
Value SYM_LET_STAR;
Value SYM_LOAD_TIME_VALUE;
Value SYM_LOCALLY;
Value SYM_MACROLET;
Value SYM_MULTIPLE_VALUE_CALL;
Value SYM_MULTIPLE_VALUE_PROG1;
Value SYM_PROGN;
Value SYM_PROGV;
Value SYM_QUOTE;
Value SYM_RETURN_FROM;
Value SYM_SETQ;
Value SYM_SYMBOL_MACROLET;
Value SYM_TAGBODY;
Value SYM_THE;
Value SYM_THROW;
Value SYM_UNWIND_PROTECT;
Value SYM_STAR_PACKAGE_STAR;

void init_symbols() {
  SYM_NIL = PKG_CL->add_external_symbol("NIL");
  get_symbol(SYM_NIL)->set_value(NIL);

  SYM_T = PKG_CL->add_external_symbol("T");
  get_symbol(SYM_T)->set_value(SYM_T);
  T = SYM_T;

  SYM_BLOCK = PKG_CL->add_external_symbol("BLOCK");
  SYM_CATCH = PKG_CL->add_external_symbol("CATCH");
  SYM_EVAL_WHEN = PKG_CL->add_external_symbol("EVAL-WHEN");
  SYM_FLET = PKG_CL->add_external_symbol("FLET");
  SYM_FUNCTION = PKG_CL->add_external_symbol("FUNCTION");
  SYM_GO = PKG_CL->add_external_symbol("GO");
  SYM_IF = PKG_CL->add_external_symbol("IF");
  SYM_LABELS = PKG_CL->add_external_symbol("LABELS");
  SYM_LET = PKG_CL->add_external_symbol("LET");
  SYM_LET_STAR = PKG_CL->add_external_symbol("LET*");
  SYM_LOAD_TIME_VALUE = PKG_CL->add_external_symbol("LOAD-TIME-VALUE");
  SYM_LOCALLY = PKG_CL->add_external_symbol("LOCALLY");
  SYM_MACROLET = PKG_CL->add_external_symbol("MACROLET");
  SYM_MULTIPLE_VALUE_CALL = PKG_CL->add_external_symbol("MULTIPLE_VALUE_CALL");
  SYM_MULTIPLE_VALUE_PROG1 =
      PKG_CL->add_external_symbol("MULTIPLE_VALUE_PROG1");
  SYM_PROGN = PKG_CL->add_external_symbol("PROGN");
  SYM_PROGV = PKG_CL->add_external_symbol("PROGV");
  SYM_QUOTE = PKG_CL->add_external_symbol("QUOTE");
  SYM_RETURN_FROM = PKG_CL->add_external_symbol("RETURN-FROM");
  SYM_SETQ = PKG_CL->add_external_symbol("SETQ");
  SYM_SYMBOL_MACROLET = PKG_CL->add_external_symbol("SYMBOL_MACROLET");
  SYM_TAGBODY = PKG_CL->add_external_symbol("TAGBODY");
  SYM_THE = PKG_CL->add_external_symbol("THE");
  SYM_THROW = PKG_CL->add_external_symbol("THROW");
  SYM_UNWIND_PROTECT = PKG_CL->add_external_symbol("UNWIND-PROTECT");
  SYM_STAR_PACKAGE_STAR = PKG_CL->add_external_symbol("*PACKAGE*");
}

} // namespace lisp