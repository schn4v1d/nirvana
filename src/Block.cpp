#include "Block.h"
#include "GarbageCollector.h"

namespace lisp {

Block::Block(Value name, Frame *frame)
    : Object{OBJ_BLOCK}, name{name}, frame{frame} {}

void Block::trace(bool marking) {
  mark(marking);
  trace_value(name, marking);
  trace_value(frame->make_value(), marking);
}

Frame *Block::get_frame() const { return frame; }

Value Block::get_name() const { return name; }

Value Block::get_return_value() const {
  return frame->get_block().return_value;
}

std::jmp_buf *Block::get_jmp_buf() { return &frame->get_block().jmp_buf; }

void Block::set_return_value(Value value) {
  frame->get_block().return_value = value;
}

bool is_block(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_BLOCK;
  }

  return false;
}

Block *get_block(Value value) {
  return reinterpret_cast<Block *>(get_object(value));
}

Block *make_block(Value name, Frame *frame) {
  return make_object<Block>(name, frame);
}

Value make_block_v(Value name, Frame *frame) {
  return make_block(name, frame)->make_value();
}

} // namespace lisp
