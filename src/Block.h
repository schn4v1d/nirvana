#pragma once

#include "Object.h"
#include <csetjmp>

namespace lisp {

class Block : public Object {
  Value name;
  Value return_value{NIL};
  std::jmp_buf jmp_buf;

public:
  explicit Block(Value name);

  void trace(bool marking) override;

  [[nodiscard]] Value get_name() const;
  [[nodiscard]] Value get_return_value() const;
  [[nodiscard]] std::jmp_buf *get_jmp_buf();
  void set_return_value(Value value);
};

bool is_block(Value value);
Block *get_block(Value value);
Block *make_block(Value name);
Value make_block_v(Value name);

} // namespace lisp
