#pragma once

#include "Object.h"
#include <string>

namespace lisp {

class Symbol : public Object {
  std::string name;
  Value package;

protected:
  void trace(bool marking) override;

public:
  Symbol(std::string_view name, Value package);
};

bool is_symbol(Value value);
Symbol *get_symbol(Value value);

} // namespace lisp
