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

int main() {
  try {
    init_packages();
    init_symbols();
    init_reader();
    init_eval();
    init_builtin_functions();

    std::ifstream t{"cl/test.lisp"};
    std::stringstream buffer;
    buffer << t.rdbuf();
    std::istringstream input_stream{buffer.str()};

    Environment *environment = make_environment();

    try {
      while (true) {
        Value v = read(input_stream, environment);

        std::cout << "> " << v << std::endl;

        Value result = eval(v, environment);

        std::cout << result << std::endl;
      }
    } catch (ReadEndOfFile &e) {
    }
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
}
