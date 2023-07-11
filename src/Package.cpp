#include "Package.h"
#include "GarbageCollector.h"
#include <cassert>

namespace lisp {

Package::Package(std::string_view name) : Object{OBJ_PACKAGE}, name{name} {}

void Package::trace(bool marking) {
  if (get_marked() == marking)
    return;

  mark(marking);

  for (auto &[_, sym] : internal_symbols) {
    sym->trace(marking);
  }

  for (auto &[_, sym] : external_symbols) {
    sym->trace(marking);
  }
}

Symbol *Package::get_internal_symbol(std::string_view symbol_name) {
  auto it = internal_symbols.find(symbol_name);

  if (it != internal_symbols.end()) {
    return it->second;
  } else {
    return nullptr;
  }
}

Symbol *Package::get_external_symbol(std::string_view symbol_name) {
  auto it = external_symbols.find(symbol_name);

  if (it != external_symbols.end()) {
    return it->second;
  } else {
    return nullptr;
  }
}

Value Package::add_external_symbol(std::string_view symbol_name) {
  Symbol *symbol = make_symbol(symbol_name, make_value());
  external_symbols.insert(std::make_pair(symbol_name, symbol));
  return symbol->make_value();
}

Value Package::intern(std::string_view symbol_name, bool external) {
  Symbol *symbol = get_external_symbol(symbol_name);

  if (symbol) {
    return symbol->make_value();
  }

  symbol = get_internal_symbol(symbol_name);

  if (symbol) {
    assert(!external);
    return symbol->make_value();
  }

  symbol = make_symbol(symbol_name, make_value());

  if (external) {
    external_symbols.insert(std::make_pair(symbol_name, symbol));
  } else {
    internal_symbols.insert(std::make_pair(symbol_name, symbol));
  }

  return symbol->make_value();
}

Package *make_package(std::string_view name) {
  return make_object<Package>(name);
}

Value make_package_v(std::string_view name) {
  return make_package(name)->make_value();
}

Package *PKG_CL;
Package *PKG_CL_USER;
Package *PKG_CURRENT;

void init_packages() {
  PKG_CL = make_package("COMMON-LISP");

  PKG_CL_USER = make_package("COMMON-LISP-USER");

  PKG_CURRENT = PKG_CL_USER;
}

} // namespace lisp