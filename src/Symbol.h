#pragma once

#include "Object.h"
#include <string>

namespace lisp {

class Symbol : public Object {
  std::string name;
  Value package;
  Value value{UNBOUND};
  Value function{UNBOUND};
  bool specialp{false};
  bool constantp{false};

public:
  Symbol(std::string_view name, Value package);

  Value get_package();
  [[nodiscard]] const std::string &get_name() const;
  Value get_value();
  void set_value(Value new_value);
  void set_constant(Value initial_value);
  Value get_function();
  void set_function(Value new_function);
  [[nodiscard]] bool is_special() const;
  [[nodiscard]] bool is_constant() const;
  [[nodiscard]] bool is_bound() const;
  [[nodiscard]] bool is_fbound() const;
  void declare_special();

  void trace(bool marking) override;

  std::ostream &print(std::ostream &os) override;
};

bool is_symbol(Value value);
Symbol *get_symbol(Value value);

Symbol *make_symbol(std::string_view name, Value package);
Value make_symbol_v(std::string_view name, Value package);

extern Value SYM_NIL;
extern Value SYM_T;
extern Value SYM_BLOCK;
extern Value SYM_CATCH;
extern Value SYM_EVAL_WHEN;
extern Value SYM_FLET;
extern Value SYM_FUNCTION;
extern Value SYM_GO;
extern Value SYM_IF;
extern Value SYM_LABELS;
extern Value SYM_LET;
extern Value SYM_LET_STAR;
extern Value SYM_LOAD_TIME_VALUE;
extern Value SYM_LOCALLY;
extern Value SYM_MACROLET;
extern Value SYM_MULTIPLE_VALUE_CALL;
extern Value SYM_MULTIPLE_VALUE_PROG1;
extern Value SYM_PROGN;
extern Value SYM_PROGV;
extern Value SYM_QUOTE;
extern Value SYM_RETURN_FROM;
extern Value SYM_SETQ;
extern Value SYM_SYMBOL_MACROLET;
extern Value SYM_TAGBODY;
extern Value SYM_THE;
extern Value SYM_THROW;
extern Value SYM_UNWIND_PROTECT;
extern Value SYM_STAR_PACKAGE_STAR;
extern Value SYM_DEFUN;
extern Value SYM_DEFPARAMETER;
extern Value SYM_DEFVAR;
extern Value SYM_AND_ALLOW_OTHER_KEYS;
extern Value SYM_AND_AUX;
extern Value SYM_AND_KEY;
extern Value SYM_AND_OPTIONAL;
extern Value SYM_AND_REST;

void init_symbols();

} // namespace lisp
