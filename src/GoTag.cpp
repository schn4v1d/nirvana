#include "GoTag.h"
#include "GarbageCollector.h"

namespace lisp {

GoTag::GoTag(Value tag, Frame *frame)
    : Object{OBJ_GO_TAG}, go_tag{tag}, frame{frame} {}

Value GoTag::get_tag() { return go_tag; }

Frame *GoTag::get_frame() { return frame; }

void GoTag::trace(bool marking) {
  mark(marking);
  trace_value(go_tag, marking);
  trace_value(frame->make_value(), marking);
}

bool is_go_tag(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_GO_TAG;
  }

  return false;
}

GoTag *get_go_tag(Value value) {
  return reinterpret_cast<GoTag *>(get_object(value));
}

GoTag *make_go_tag(Value tag, Frame *frame) {
  return make_object<GoTag>(tag, frame);
}

Value make_go_tag_v(Value tag, Frame *frame) {
  return make_go_tag(tag, frame)->make_value();
}

} // namespace lisp