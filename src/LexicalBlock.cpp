#include "GarbageCollector.h"
#include "LexicalBlock.h"
#include "Frame.h"

namespace lisp {

LexicalBlock::LexicalBlock(Value name, Frame *frame)
    : Object{OBJ_LEXICAL_BLOCK}, name{name}, frame{frame} {}

void LexicalBlock::trace(bool marking) {
  mark(marking);
  trace_value(name, marking);
  trace_value(frame->make_value(), marking);
}

Frame *LexicalBlock::get_frame() const { return frame; }

Value LexicalBlock::get_name() const { return name; }

Value LexicalBlock::get_return_value() const {
  return frame->get_block().return_value;
}

std::jmp_buf *LexicalBlock::get_jmp_buf() { return &frame->get_block().jmp_buf; }

void LexicalBlock::set_return_value(Value value) {
  frame->get_block().return_value = value;
}

bool is_block(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_LEXICAL_BLOCK;
  }

  return false;
}

LexicalBlock *get_block(Value value) {
  return reinterpret_cast<LexicalBlock *>(get_object(value));
}

LexicalBlock *make_block(Value name, Frame *frame) {
  return make_object<LexicalBlock>(name, frame);
}

Value make_block_v(Value name, Frame *frame) {
  return make_block(name, frame)->make_value();
}

} // namespace lisp
