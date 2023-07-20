#include "special_operators.h"
#include "Symbol.h"
#include "util.h"

namespace lisp {

static std::unordered_map<Value, special_operator> special_operators_map;

void init_special_operators() {
  auto not_implemented = [](Value _args, Environment *_env) -> Value {
    throw NotImplemented{};
  };

  special_operators_map = {
      {SYM_BACKQUOTE, special_operators::op_backquote},
      {SYM_BLOCK, special_operators::op_block},
      {SYM_CATCH, special_operators::op_catch},
      {SYM_EVAL_WHEN, not_implemented},
      {SYM_FLET, not_implemented},
      {SYM_FUNCTION, special_operators::op_function},
      {SYM_GO, special_operators::op_go},
      {SYM_IF, special_operators::op_if},
      {SYM_LABELS, not_implemented},
      {SYM_LET, special_operators::op_let},
      {SYM_LET_STAR, special_operators::op_let_star},
      {SYM_LOAD_TIME_VALUE, not_implemented},
      {SYM_LOCALLY, not_implemented},
      {SYM_MACROLET, not_implemented},
      {SYM_MULTIPLE_VALUE_CALL, not_implemented},
      {SYM_MULTIPLE_VALUE_PROG1, not_implemented},
      {SYM_PROGN, not_implemented},
      {SYM_PROGV, not_implemented},
      {SYM_QUOTE, special_operators::op_quote},
      {SYM_RETURN_FROM, special_operators::op_return_from},
      {SYM_SETQ, special_operators::op_setq},
      {SYM_SYMBOL_MACROLET, not_implemented},
      {SYM_TAGBODY, special_operators::op_tagbody},
      {SYM_THE, not_implemented},
      {SYM_THROW, special_operators::op_throw},
      {SYM_UNWIND_PROTECT, special_operators::op_unwind_protect}};
}

bool is_special_operator(Value name) {
  return special_operators_map.contains(name);
}

const special_operator &get_special_operator(Value name) {
  return (*special_operators_map.find(name)).second;
}

} // namespace lisp