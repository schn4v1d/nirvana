#include "DynamicBindings.h"
#include "Binding.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "cl_fun.h"

namespace lisp {

DynamicBindings::DynamicBindings() : Object{OBJ_DYNAMIC_BINDINGS} {}

void DynamicBindings::trace(bool marking) {
  mark(marking);
  trace_value(bindings, marking);
}

Value DynamicBindings::checkpoint() { return bindings; }

void DynamicBindings::push_binding(Value name, Value value) {
  bindings = make_cons_v(make_binding_v(name, value, false), bindings);
}

void DynamicBindings::restore_checkpoint(Value checkpoint) {
  while (!is_nil(bindings) && bindings != checkpoint) {
    bindings = cl::cdr(bindings);
  }
}

Value DynamicBindings::lookup_value(Value name) {
  return lisp::lookup_value(name, bindings);
}

Binding *DynamicBindings::lookup_binding(Value name) {
  return lisp::lookup_binding(name, bindings);
}

Frame *DynamicBindings::lookup_catch(Value tag) {
  Value result = iter_list(
      [tag](Value framev) -> std::optional<Value> {
        Frame *frame = get_frame(framev);
        if (frame->is_catch() && frame->get_catch().tag == tag) {
          return framev;
        } else {
          return std::nullopt;
        }
      },
      frames);

  if (is_unbound(result)) {
    return nullptr;
  } else {
    return get_frame(result);
  }
}

Frame *DynamicBindings::push_frame(FrameData data) {
  Frame *frame = make_frame(data);
  frames = make_cons_v(frame->make_value(), frames);
  return frame;
}

void DynamicBindings::unwind(Frame *frame, bool inclusive) {
  while (!is_nil(frames)) {
    Frame *current = get_frame(cl::car(frames));

    if (!inclusive && current == frame) {
      break;
    }

    frames = cl::cdr(frames);

    current->unwind();

    if (current == frame) {
      break;
    }
  }
}

bool is_dynamic_bindings(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_DYNAMIC_BINDINGS;
  }

  return false;
}

DynamicBindings *get_dynamic_bindings(Value value) {
  return reinterpret_cast<DynamicBindings *>(get_object(value));
}

DynamicBindings *make_dynamic_bindings() {
  return make_object<DynamicBindings>();
}

Value make_dynamic_bindings_v() {
  return make_dynamic_bindings()->make_value();
}

} // namespace lisp
