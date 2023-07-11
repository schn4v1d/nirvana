#pragma once

#include "Object.h"
#include <string>

namespace lisp {

struct SymbolFlags {
  bool bound : 1;
};

class Symbol : public Object {
  SymbolFlags flags;
  std::string name;
  Value package;
  Value value;

public:
  Symbol(std::string_view name, Value package);

  Value get_value();
  void set_value(Value new_value);
  bool is_bound() const;

  void trace(bool marking) override;

  std::ostream &print(std::ostream &os) override;
};

bool is_symbol(Value value);
Symbol *get_symbol(Value value);

Symbol *make_symbol(std::string_view name, Value package);
Value make_symbol_v(std::string_view name, Value package);

extern Value SYM_NIL;
extern Value SYM_T;

void init_symbols();

} // namespace lisp
