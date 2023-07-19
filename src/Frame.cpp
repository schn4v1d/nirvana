#include "Frame.h"
#include "GarbageCollector.h"
#include "cl_fun.h"
#include "eval.h"

namespace lisp {

BlockFrame::BlockFrame(Value name) : name{name} {}

void BlockFrame::unwind() {}

void BlockFrame::trace(bool marking) {
  trace_value(name, marking);
  trace_value(return_value, marking);
}

UnwindProtectFrame::UnwindProtectFrame(Value cleanup_forms, Environment *env)
    : cleanup_forms{cleanup_forms}, env{env} {}

void UnwindProtectFrame::unwind() {
  Value form = cleanup_forms;

  while (!is_nil(form)) {
    eval(cl::car(form), env);
    form = cl::cdr(form);
  }
}

void UnwindProtectFrame::trace(bool marking) {
  trace_value(cleanup_forms, marking);
  trace_value(env->make_value(), marking);
}

Frame::Frame(FrameData data) : Object{OBJ_FRAME}, data{data} {}

void Frame::trace(bool marking) {
  mark(marking);
  std::visit([marking](auto &x) { x.trace(marking); }, data);
}

void Frame::unwind() {
  std::visit([](auto &x) { x.unwind(); }, data);
}

bool Frame::is_block() const {
  return std::holds_alternative<BlockFrame>(data);
}

BlockFrame &Frame::get_block() { return std::get<BlockFrame>(data); }

bool Frame::is_unwind_protect() const {
  return std::holds_alternative<UnwindProtectFrame>(data);
}

UnwindProtectFrame &Frame::get_unwind_protect() {
  return std::get<UnwindProtectFrame>(data);
}

bool is_frame(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_FRAME;
  }

  return false;
}

Frame *get_frame(Value value) {
  return reinterpret_cast<Frame *>(get_object(value));
}

Frame *make_frame(FrameData data) {
  return make_object<Frame>(data);
}

Value make_frame_v(FrameData data) {
  return make_frame(data)->make_value();
}

} // namespace lisp
