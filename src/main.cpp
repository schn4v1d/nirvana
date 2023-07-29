#include "BuiltinFunction.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "Package.h"
#include "cl_fun.h"
#include "eval.h"
#include "reader.h"
#include <filesystem>
#include <fstream>
#include <iostream>

using namespace lisp;

Value execute(std::string_view code, Environment *env) {
  std::istringstream input_stream{code.data()};
  
  Value result = NIL;

  try {
    while (true) {
      Value v = read(input_stream, env);
      result = eval(v, env);
    }
  } catch (ReadEndOfFile &e) {
  }

  return result;
}

int main() {
  try {
    init_packages();
    init_symbols();
    init_reader();
    init_eval();
    init_builtin_functions();

    Environment *environment = make_environment();

    get_symbol(SYM_STAR_PACKAGE_STAR)->set_value(PKG_CL->make_value());

    execute("(nirvana-builtins:%load \"cl/init.lisp\")", environment);

    std::cout << "loaded core successfully" << std::endl;

    std::cout << execute("(load \"cl/test.lisp\")", environment) << std::endl;

    try {
      execute("(load \"cl/ansi-test/doit.lsp\")", environment);

      std::cout << "ansi tests succeeded!" << std::endl;
    } catch (std::exception &e) {
      std::cout << e.what() << std::endl;
      std::cout << "ansi tests failed." << std::endl;
    }
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
}
