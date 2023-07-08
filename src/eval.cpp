#include "eval.h"

namespace lisp::eval {

Object lisp::eval::eval(Object &obj, object::MemoryManager &mem) {
  if (obj.is_cons()) {
    auto &cons = obj.as_cons();

    auto &car = cons.car();

    if (car.is_symbol()) {

      throw NotImplemented();
    } else {
      throw NotImplemented();
    }
  } else if (obj.is_symbol()) {
    auto &symbol = obj.as_symbol();

    throw NotImplemented();
  } else {
    return obj;
  }
}

} // namespace lisp::eval