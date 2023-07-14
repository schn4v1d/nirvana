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

} // namespace lisp::cl