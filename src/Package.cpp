#include "Package.h"
#include "GarbageCollector.h"
#include "String.h"
#include "cl_fun.h"
#include "util.h"
#include <cassert>
#include <sstream>
#include <unordered_map>

namespace lisp {

std::unordered_map<std::string, Package *> packages{};

Package::Package(std::string_view name) : Object{OBJ_PACKAGE}, name{name} {
  packages.insert(std::make_pair(name, this));
}

void Package::trace(bool marking) {
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

  for (Package *use : used_packages) {
    symbol = use->get_external_symbol(symbol_name);
    if (symbol) {
      return symbol->make_value();
    }
  }

  symbol = make_symbol(symbol_name, make_value());

  if (external) {
    external_symbols.insert(std::make_pair(symbol_name, symbol));
  } else {
    internal_symbols.insert(std::make_pair(symbol_name, symbol));
  }

  return symbol->make_value();
}

void Package::use_package(Package *package) {
  for (const auto &[symbol_name, _] : package->external_symbols) {
    if (package->get_internal_symbol(symbol_name) ||
        !package->get_external_symbol(symbol_name)) {
      assert(false);
    }
  }

  used_packages.push_back(package);
}

std::ostream &Package::print(std::ostream &os) const {
  return os << "#<PACKAGE " << name << '>';
}

void Package::export_symbol(Symbol *symbol) {
  if (symbol->get_package() == make_value()) {
    if (get_external_symbol(symbol->get_name()) == symbol) {
      return;
    }
    internal_symbols.erase(symbol->get_name());
    external_symbols.insert(std::make_pair(symbol->get_name(), symbol));
    return;
  }

  throw NotImplemented{};
}
const std::string &Package::get_name() const { return name; }

Package *make_package(std::string_view name) {
  return make_object<Package>(name);
}

Value make_package_v(std::string_view name) {
  return make_package(name)->make_value();
}

bool is_package(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_PACKAGE;
  }

  return false;
}

Package *get_package(Value value) {
  return reinterpret_cast<Package *>(get_object(value));
}

Value find_package(Value arg) {
  if (is_package(arg)) {
    return arg;
  }

  String *name = get_string(cl::string(arg));

  auto it = packages.find(name->get_content());

  if (it != packages.end()) {
    return (*it).second->make_value();
  } else {
    return NIL;
  }
}

Package *coerce_to_package(Value value) {
  Value package = find_package(value);

  if (is_nil(package)) {
    std::ostringstream oss{};
    oss << value << " is not the name of a package";
    std::string err{oss.str()};
    throw std::exception{err.c_str()};
  }

  return get_package(package);
}

Package *PKG_CL;
Package *PKG_CL_USER;

void init_packages() {
  PKG_CL = make_package("COMMON-LISP");

  PKG_CL_USER = make_package("COMMON-LISP-USER");
  PKG_CL_USER->use_package(PKG_CL);
}

} // namespace lisp