#pragma once

#include "Value.h"
#include <functional>
#include <unordered_map>

namespace lisp {

class Environment;

using special_operator = std::function<Value(Value args, Environment *env)>;

void init_special_operators();
bool is_special_operator(Value name);
const special_operator &get_special_operator(Value name);

} // namespace lisp

namespace lisp::special_operators {

Value op_backquote(Value args, Environment *env);
Value op_block(Value args, Environment *env);
Value op_catch(Value args, Environment *env);
Value op_function(Value args, Environment *env);
Value op_if(Value args, Environment *env);
Value op_let(Value args, Environment *env);
Value op_let_star(Value args, Environment *env);
Value op_quote(Value args, Environment *env);
Value op_return_from(Value args, Environment *env);
Value op_setq(Value args, Environment *env);
Value op_throw(Value args, Environment *env);
Value op_unwind_protect(Value args, Environment *env);

} // namespace lisp::special_operators