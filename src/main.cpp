#include <iostream>
#include "object.h"

using namespace lisp::object;

int main() {
  MemoryManager mem{};

  {
    ManagedObjectRoot root = mem.allocate_object(std::pair{1, 2});
  }

  mem.collect();

  return 0;
}
