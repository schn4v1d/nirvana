#pragma once

#include "Symbol.h"
#include "Value.h"
#include <optional>
#include <vector>

namespace lisp {

struct OrdinaryLambdaList {
  std::vector<Symbol *> required{};
  std::vector<
      std::tuple<Symbol *, std::optional<Value>, std::optional<Symbol *>>>
      optional{};
  std::optional<Symbol *> rest{};
  std::vector<std::pair<Symbol *, std::optional<Value>>> auxiliaries{};

  explicit OrdinaryLambdaList(Value arguments);
  OrdinaryLambdaList(OrdinaryLambdaList &&other) noexcept;
};

} // namespace lisp
