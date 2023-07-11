#include "Cons.h"
#include "GarbageCollector.h"
#include "Package.h"
#include "reader.h"
#include <iostream>

using namespace lisp;

int main() {
  try {
    init_packages();
    init_symbols();
    init_reader();

    std::string input = "(1 . (2 . (3 . nil)))";
    std::istringstream input_stream{input};

    Value v = read(input_stream);

    std::cout << v << std::endl;
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
}
