#pragma once

#include "Object.h"
#include <functional>
#include <optional>

namespace lisp {

class Cons : public Object {
  Value car, cdr;

public:
  Cons(Value car, Value cdr);

  Value get_car();
  Value get_cdr();
  void set_car(Value new_car);
  void set_cdr(Value new_cdr);

  void trace(bool marking) override;

  std::ostream &print(std::ostream &os) override;
};

bool is_cons(Value value);
Cons *get_cons(Value value);

Cons *make_cons(Value car, Value cdr);
Value make_cons_v(Value car, Value cdr);

Value iter_list(const std::function<std::optional<Value>(Value)>& func, Value list);

} // namespace lisp
