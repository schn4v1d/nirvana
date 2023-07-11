#include "Cons.h"
#include "GarbageCollector.h"

namespace lisp {

Cons::Cons(Value car, Value cdr) : Object(OBJ_CONS), car{car}, cdr{cdr} {}

Value Cons::get_car() { return car; }

Value Cons::get_cdr() { return cdr; }

void Cons::set_car(Value new_car) { car = new_car; }

void Cons::set_cdr(Value new_cdr) { cdr = new_cdr; }

void Cons::trace(bool marking) {
  if (get_marked() == marking)
    return;

  mark(marking);

  trace_value(car, marking);
  trace_value(cdr, marking);
}

std::ostream &Cons::print(std::ostream &os) {
  os << '(' << car;

  Value current = cdr;

  while (!is_nil(current)) {
    if (is_cons(current)) {
      Cons *cons = get_cons(current);
      os << ' ' << cons->car;
      current = cons->cdr;
    } else {
      os << " . " << current;
      break;
    }
  }

  return os << ')';
}

bool is_cons(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_CONS;
  }

  return false;
}

Cons *get_cons(Value value) {
  return reinterpret_cast<Cons *>(get_object(value));
}

Cons *make_cons(Value car, Value cdr) { return make_object<Cons>(car, cdr); }

Value make_cons_v(Value car, Value cdr) {
  return make_cons(car, cdr)->make_value();
}

} // namespace lisp