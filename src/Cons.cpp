#include "Cons.h"
#include "GarbageCollector.h"
#include "Symbol.h"
#include "cl_fun.h"

namespace lisp {

Cons::Cons(Value car, Value cdr) : Object(OBJ_CONS), car{car}, cdr{cdr} {}

Value &Cons::get_car() { return car; }

Value &Cons::get_cdr() { return cdr; }

void Cons::set_car(Value new_car) { car = new_car; }

void Cons::set_cdr(Value new_cdr) { cdr = new_cdr; }

void Cons::trace(bool marking) {
  mark(marking);

  trace_value(car, marking);
  trace_value(cdr, marking);
}

std::ostream &Cons::print(std::ostream &os) const {
  if (is_cons(cdr) && is_nil(cl::cdr(cdr))) {
    if (car == SYM_QUOTE) {
      return os << '\'' << cl::car(cdr);
    } else if (car == SYM_BACKQUOTE) {
      return os << '`' << cl::car(cdr);
    } else if (car == SYM_UNQUOTE) {
      return os << ',' << cl::car(cdr);
    } else if (car == SYM_UNQUOTE_SPLICING) {
      return os << ",@" << cl::car(cdr);
    }
  }

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

Cons::iterator Cons::begin() const { return Cons::iterator{make_value()}; }

Cons::iterator Cons::end() const { return Cons::iterator{NIL}; }

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

Value iter_list(const std::function<std::optional<Value>(Value)> &func,
                Value list) {
  Value current = list;

  while (is_cons(current)) {
    Cons *current_cons = get_cons(current);

    std::optional<Value> result = func(current_cons->get_car());

    if (result.has_value()) {
      return result.value();
    } else {
      current = current_cons->get_cdr();
    }
  }

  return UNBOUND;
}

Value map_list(const std::function<Value(Value)> &func, Value list) {
  Value current = list;
  Value result = NIL;
  Value result_current = NIL;

  while (is_cons(current)) {
    Cons *current_cons = get_cons(current);

    Value value = func(current_cons->get_car());

    if (is_nil(result)) {
      result = make_cons_v(value, NIL);
      result_current = result;
    } else {
      Cons *next = make_cons(value, NIL);
      get_cons(result_current)->set_cdr(next->make_value());
      result_current = next->make_value();
    }

    current = current_cons->get_cdr();
  }

  return result;
}

Value list_from_cppvector(std::vector<Value> &&vec) {
  Value result = NIL;

  while (!vec.empty()) {
    result = make_cons_v(vec.back(), result);
    vec.pop_back();
  }

  return result;
}

Cons::iterator::iterator(Value value) : value{value} {}

Cons::iterator &Cons::iterator::operator++() {
  value = cl::cdr(value);
  return *this;
}

Value &Cons::iterator::operator*() const { return get_cons(value)->get_car(); }

bool Cons::iterator::operator!=(const Cons::iterator &other) const {
  return value != other.value;
}

} // namespace lisp