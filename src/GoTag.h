#pragma once

#include "Frame.h"
#include "Object.h"

namespace lisp {

class GoTag: public Object {
  Value go_tag;
  Frame *frame;

public:
  GoTag(Value tag, Frame *frame);

  void trace(bool marking) override;

  Value get_tag();
  Frame *get_frame();
};

bool is_go_tag(Value value);
GoTag *get_go_tag(Value value);
GoTag *make_go_tag(Value tag, Frame *frame);
Value make_go_tag_v(Value tag, Frame *frame);

} // namespace lisp
