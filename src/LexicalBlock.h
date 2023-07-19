#pragma once

#include "Object.h"
#include <csetjmp>

namespace lisp {

class Frame;

class LexicalBlock : public Object {
  Value name;
  Frame *frame;

public:
  explicit LexicalBlock(Value name, Frame *frame);

  void trace(bool marking) override;

  [[nodiscard]] Frame *get_frame() const;
  [[nodiscard]] Value get_name() const;
  [[nodiscard]] Value get_return_value() const;
  [[nodiscard]] std::jmp_buf *get_jmp_buf();
  void set_return_value(Value value);
};

bool is_block(Value value);
LexicalBlock *get_block(Value value);
LexicalBlock *make_block(Value name, Frame *frame);
Value make_block_v(Value name, Frame *frame);

} // namespace lisp
