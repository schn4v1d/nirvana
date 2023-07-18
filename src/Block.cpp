#include "Block.h"
#include "GarbageCollector.h"

namespace lisp {

Block::Block(Value name) : Object{OBJ_BLOCK}, name{name}, jmp_buf{} {}

void Block::trace(bool marking) {
  mark(marking);
  trace_value(name, marking);
  trace_value(return_value, marking);
}

Value Block::get_name() const { return name; }

Value Block::get_return_value() const { return return_value; }

std::jmp_buf *Block::get_jmp_buf() { return &jmp_buf; }

void Block::set_return_value(Value value) { return_value = value; }

bool is_block(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_BLOCK;
  }

  return false;
}

Block *get_block(Value value) {
  return reinterpret_cast<Block *>(get_object(value));
}

Block *make_block(Value name) { return make_object<Block>(name); }

Value make_block_v(Value name) { return make_block(name)->make_value(); }

} // namespace lisp
