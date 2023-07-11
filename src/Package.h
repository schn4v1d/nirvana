#pragma once

#include "Object.h"
#include "Symbol.h"
#include <map>
#include <string>

namespace lisp {

class Package : public Object {
  std::string name;
  std::map<std::string, Symbol *, std::less<>> internal_symbols;
  std::map<std::string, Symbol *, std::less<>> external_symbols;

  Symbol *get_internal_symbol(std::string_view symbol_name);
  Symbol *get_external_symbol(std::string_view symbol_name);

public:
  explicit Package(std::string_view name);

  void trace(bool marking) override;

  Value add_external_symbol(std::string_view symbol_name);

  Value intern(std::string_view symbol_name, bool external);
};

Package *make_package(std::string_view name);
Value make_package_v(std::string_view name);

extern Package *PKG_CL;
extern Package *PKG_CL_USER;
extern Package *PKG_CURRENT;

void init_packages();

} // namespace lisp
