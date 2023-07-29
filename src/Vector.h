#pragma once

#include "Object.h"
#include <vector>

namespace lisp {

class Vector : public Object {
  std::vector<Value> elements{};

public:
  Vector();

  void trace(bool marking) override;
  std::ostream &print(std::ostream &os) const override;

  Value get(size_t index);
  void push(Value value);
  [[nodiscard]] std::size_t size() const;
};

bool is_vector(Value value);
Vector *get_vector(Value value);
Vector *make_vector();
Value make_vector_v();

} // namespace lisp
