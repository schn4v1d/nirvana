#pragma once

#include "Environment.h"
#include "Value.h"

namespace lisp {

void init_eval();

Value eval(Value arg, Environment *env, bool multiple_values = false);

} // namespace lisp