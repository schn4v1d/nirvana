#pragma once

#include "Environment.h"
#include "Lambda.h"
#include "Object.h"
#include "OrdinaryLambdaList.h"

namespace lisp {

class MacroFunction: public Object {
  Lambda *lambda;

public:
  MacroFunction(Lambda *lambda);

  void trace(bool marking) override;
  Value expand(Value args, Environment *env);
};

bool is_macro_function(Value value);
MacroFunction *get_macro_function(Value value);

MacroFunction *make_macro_function(Lambda *lambda);
Value make_macro_function_v(Lambda *lambda);

} // namespace lisp
