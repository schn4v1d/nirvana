#pragma once

#include "Object.h"
#include <vector>

namespace lisp {

class Values : public Object {
  std::vector<Value> values;

public:
  explicit Values(std::vector<Value> &&values);

  void trace(bool marking) override;

  [[nodiscard]] const Value &get_value(size_t i) const;
  [[nodiscard]] const std::vector<Value> &get_values() const;
};

bool is_values(Value value);
Values *get_values(Value value);
Values *make_values(std::vector<Value> &&values);
Value make_values_v(std::vector<Value> &&values);

} // namespace lisp
