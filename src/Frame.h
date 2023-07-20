#pragma once

#include "Object.h"
#include "Value.h"
#include <csetjmp>
#include <optional>
#include <unordered_map>
#include <variant>

namespace lisp {

class Environment;

struct BlockFrame {
  Value name;
  Value return_value{NIL};
  std::jmp_buf jmp_buf{};

  explicit BlockFrame(Value name);

  void unwind();
  void trace(bool marking);
};

struct UnwindProtectFrame {
  Value cleanup_forms;
  Environment *env;

  explicit UnwindProtectFrame(Value cleanup_forms, Environment *env);

  void unwind();
  void trace(bool marking);
};

struct CatchFrame {
  Value tag;
  Value return_value{NIL};
  std::jmp_buf jmp_buf{};

  explicit CatchFrame(Value tag);

  void unwind();
  void trace(bool marking);
};

struct TagbodyFrame {
  std::jmp_buf jmp_buf{};
  std::unordered_map<Value, int> tags;

  explicit TagbodyFrame(std::unordered_map<Value, int> tags);

  void unwind();
  void trace(bool marking);

  std::optional<int> get_tag(Value tag);
};

using FrameData =
    std::variant<BlockFrame, UnwindProtectFrame, CatchFrame, TagbodyFrame>;

class Frame : public Object {
  FrameData data;

public:
  explicit Frame(FrameData data);

  void trace(bool marking) override;

  void unwind();

  [[nodiscard]] bool is_block() const;
  [[nodiscard]] BlockFrame &get_block();

  [[nodiscard]] bool is_unwind_protect() const;
  [[nodiscard]] UnwindProtectFrame &get_unwind_protect();

  [[nodiscard]] bool is_catch() const;
  [[nodiscard]] CatchFrame &get_catch();

  [[nodiscard]] bool is_tagbody() const;
  [[nodiscard]] TagbodyFrame &get_tagbody();
};

bool is_frame(Value value);
Frame *get_frame(Value value);
Frame *make_frame(FrameData data);
Value make_frame_v(FrameData data);

} // namespace lisp
