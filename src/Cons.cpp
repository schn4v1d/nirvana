#include "Cons.h"
#include "GarbageCollector.h"

namespace lisp {

Cons::Cons(Value car, Value cdr) : Object(OBJ_CONS), car{car}, cdr{cdr} {}

void Cons::trace(bool marking) {
  mark(marking);

  trace_value(car, marking);
  trace_value(cdr, marking);
}

Cons *make_cons(Value car, Value cdr) { return make_object<Cons>(car, cdr); }

Value make_cons_v(Value car, Value cdr) {
  return make_cons(car, cdr)->make_value();
}

} // namespace lisp