#pragma once

#include "Frame.h"
#include "Object.h"
#include <csetjmp>

namespace lisp {

class Block : public Object {
  Value name;
  Frame *frame;

public:
  explicit Block(Value name, Frame *frame);

  void trace(bool marking) override;

  [[nodiscard]] Frame *get_frame() const;
  [[nodiscard]] Value get_name() const;
  [[nodiscard]] Value get_return_value() const;
  [[nodiscard]] std::jmp_buf *get_jmp_buf();
  void set_return_value(Value value);
};

bool is_block(Value value);
Block *get_block(Value value);
Block *make_block(Value name, Frame *frame);
Value make_block_v(Value name, Frame *frame);

} // namespace lisp
