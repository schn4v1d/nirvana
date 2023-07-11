#pragma once

#include "Object.h"

namespace lisp {

class Cons: public Object {
  Value car, cdr;

protected:
  void trace(bool marking) override;

public:
  Cons(Value car, Value cdr);
};

Cons *make_cons(Value car, Value cdr);
Value make_cons_v(Value car, Value cdr);

} // namespace lisp
