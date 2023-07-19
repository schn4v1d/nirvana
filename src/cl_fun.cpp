#include "cl_fun.h"
#include "Cons.h"
#include "LispString.h"
#include "Symbol.h"
#include <cassert>

namespace lisp::cl {

Value cons(Value car, Value cdr) { return make_cons_v(car, cdr); }

Value car(Value list) {
  if (is_nil(list)) {
    return NIL;
  } else if (is_cons(list)) {
    return get_cons(list)->get_car();
  } else {
    assert(false);
  }
}

Value cdr(Value list) {
  if (is_nil(list)) {
    return NIL;
  } else if (is_cons(list)) {
    return get_cons(list)->get_cdr();
  } else {
    assert(false);
  }
}

Value caar(Value list) { return car(car(list)); }
Value cadr(Value list) { return car(cdr(list)); }
Value cdar(Value list) { return cdr(car(list)); }
Value cddr(Value list) { return cdr(cdr(list)); }

Value caaar(Value list) { return car(car(car(list))); }
Value caadr(Value list) { return car(car(cdr(list))); }
Value cadar(Value list) { return car(cdr(car(list))); }
Value caddr(Value list) { return car(cdr(cdr(list))); }
Value cdaar(Value list) { return cdr(car(car(list))); }
Value cdadr(Value list) { return cdr(car(cdr(list))); }
Value cddar(Value list) { return cdr(cdr(car(list))); }
Value cdddr(Value list) { return cdr(cdr(cdr(list))); }

Value caaaar(Value list) { return car(car(car(car(list)))); }
Value caaadr(Value list) { return car(car(car(cdr(list)))); }
Value caadar(Value list) { return car(car(cdr(car(list)))); }
Value caaddr(Value list) { return car(car(cdr(cdr(list)))); }
Value cadaar(Value list) { return car(cdr(car(car(list)))); }
Value cadadr(Value list) { return car(cdr(car(cdr(list)))); }
Value caddar(Value list) { return car(cdr(cdr(car(list)))); }
Value cadddr(Value list) { return car(cdr(cdr(cdr(list)))); }
Value cdaaar(Value list) { return cdr(car(car(car(list)))); }
Value cdaadr(Value list) { return cdr(car(car(cdr(list)))); }
Value cdadar(Value list) { return cdr(car(cdr(car(list)))); }
Value cdaddr(Value list) { return cdr(car(cdr(cdr(list)))); }
Value cddaar(Value list) { return cdr(cdr(car(car(list)))); }
Value cddadr(Value list) { return cdr(cdr(car(cdr(list)))); }
Value cdddar(Value list) { return cdr(cdr(cdr(car(list)))); }
Value cddddr(Value list) { return cdr(cdr(cdr(cdr(list)))); }

Value first(Value list) { return car(list); }
Value second(Value list) { return cadr(list); }
Value third(Value list) { return caddr(list); }
Value fourth(Value list) { return cadddr(list); }

Value string(Value arg) {
  if (is_string(arg)) {
    return arg;
  } else if (is_symbol(arg)) {
    return make_string_v(get_symbol(arg)->get_name());
//  } else if (is_character(arg)) {
//    return TODO
  } else {
    throw std::exception{"type error"};
  }
}

} // namespace lisp::cl