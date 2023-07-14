#pragma once

#include "Value.h"
#include "Environment.h"

namespace lisp {

void init_eval();

Value eval(Value value, Environment *env);

}