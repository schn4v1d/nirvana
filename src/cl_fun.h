#pragma once

#include "Value.h"

namespace lisp::cl {

Value cons(Value car, Value cdr);
Value car(Value list);
Value cdr(Value list);
Value caar(Value list);
Value cadr(Value list);
Value cdar(Value list);
Value cddr(Value list);

Value caaar(Value list);
Value caadr(Value list);
Value cadar(Value list);
Value caddr(Value list);
Value cdaar(Value list);
Value cdadr(Value list);
Value cddar(Value list);
Value cdddr(Value list);

Value caaaar(Value list);
Value caaadr(Value list);
Value caadar(Value list);
Value caaddr(Value list);
Value cadaar(Value list);
Value cadadr(Value list);
Value caddar(Value list);
Value cadddr(Value list);
Value cdaaar(Value list);
Value cdaadr(Value list);
Value cdadar(Value list);
Value cdaddr(Value list);
Value cddaar(Value list);
Value cddadr(Value list);
Value cdddar(Value list);
Value cddddr(Value list);

Value first(Value list);
Value second(Value list);
Value third(Value list);
Value fourth(Value list);

} // namespace lisp::cl