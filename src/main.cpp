#include "Cons.h"
#include "GarbageCollector.h"

using namespace lisp;

int main() {
  Value v = make_object_v<Cons>(make_value(1),
                                make_object_v<Cons>(make_value(2), NIL));

}
