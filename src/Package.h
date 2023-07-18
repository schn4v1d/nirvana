#pragma once

#include "Object.h"
#include "Symbol.h"
#include <map>
#include <string>
#include <vector>

namespace lisp {

class Package : public Object {
  std::string name;
  std::map<std::string, Symbol *, std::less<>> internal_symbols;
  std::map<std::string, Symbol *, std::less<>> external_symbols;
  std::vector<Package *> used_packages;

  Symbol *get_internal_symbol(std::string_view symbol_name);
  Symbol *get_external_symbol(std::string_view symbol_name);

public:
  explicit Package(std::string_view name);

  void trace(bool marking) override;

  std::ostream &print(std::ostream &os) const override;

  [[nodiscard]] const std::string &get_name() const;

  Value add_external_symbol(std::string_view symbol_name);
  Value intern(std::string_view symbol_name, bool external);
  void use_package(Package *package);
  void export_symbol(Symbol *symbol);
};

Package *make_package(std::string_view name);
Value make_package_v(std::string_view name);

bool is_package(Value value);
Package *get_package(Value value);

extern Package *PKG_CL;
extern Package *PKG_CL_USER;

Value find_package(Value arg);
Package *coerce_to_package(Value value);

void init_packages();

} // namespace lisp
