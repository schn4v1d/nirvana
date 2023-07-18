#include "BuiltinFunction.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "Package.h"
#include "eval.h"
#include "reader.h"
#include <filesystem>
#include <fstream>
#include <iostream>

using namespace lisp;

Value load_file(const char *file_name, Environment *env) {
  std::ifstream t{file_name};
  std::stringstream buffer;
  buffer << t.rdbuf();
  std::istringstream input_stream{buffer.str()};

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

    load_file("cl/core.lisp", environment);
    load_file("cl/defun.lisp", environment);
    std::cout << load_file("cl/test.lisp", environment);
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
}
