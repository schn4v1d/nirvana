#include "Cons.h"
#include "GarbageCollector.h"
#include "Package.h"
#include "eval.h"
#include "reader.h"
#include <iostream>

using namespace lisp;

int main() {
  try {
    init_packages();
    init_symbols();
    init_reader();
    init_eval();

    std::istringstream input_stream{"'(1 2 3)"};

    Environment *environment = make_environment();

    Value v = read(input_stream, environment);

    Value result = eval(v, environment);

    std::cout << "> " << v << std::endl;
    std::cout << result << std::endl;
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
}
