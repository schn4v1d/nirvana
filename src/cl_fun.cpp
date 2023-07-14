#include "cl_fun.h"
#include "Cons.h"
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

} // namespace lisp::cl