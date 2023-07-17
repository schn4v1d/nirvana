#pragma once

#include "Symbol.h"
#include "Value.h"
#include <vector>

namespace lisp {

class OrdinaryLambdaList {
  std::vector<Symbol *> required{};

public:
  explicit OrdinaryLambdaList(Value arguments);
};

} // namespace lisp
