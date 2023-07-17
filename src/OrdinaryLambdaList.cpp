#include "OrdinaryLambdaList.h"
#include "Cons.h"
#include "util.h"

namespace lisp {

enum class read_mode {
  required,
  optional,
  rest,
  key,
  aux,
};

OrdinaryLambdaList::OrdinaryLambdaList(Value arguments) {
  if (is_nil(arguments)) {
  } else if (is_cons(arguments)) {
    Cons *cons = get_cons(arguments);

    read_mode read_mode{read_mode::required};

    for (Cons::iterator it = cons->begin(); it != cons->end(); ++it) {
      Value v = *it;

      switch (read_mode) {
      case read_mode::required:
        if (v == SYM_AND_OPTIONAL) {
          read_mode = read_mode::optional;
        } else if (v == SYM_AND_REST) {
          read_mode = read_mode::rest;
        } else if (v == SYM_AND_KEY) {
          read_mode = read_mode::key;
        } else if (v == SYM_AND_AUX) {
          read_mode = read_mode::aux;
        } else if (is_symbol(v)) {
          required.push_back(get_symbol(v));
        } else {
          throw std::exception{"non-symbol in lambda list"};
        }
        break;
      case read_mode::optional:
        if (v == SYM_AND_OPTIONAL) {
          throw std::exception{"repeated &optional in lambda list"};
        } else if (v == SYM_AND_REST) {
          read_mode = read_mode::rest;
        } else if (v == SYM_AND_KEY) {
          read_mode = read_mode::key;
        } else if (v == SYM_AND_AUX) {
          read_mode = read_mode::aux;
        } else if (is_symbol(v)) {
          throw NotImplemented();
        } else {
          throw std::exception{"non-symbol in lambda list"};
        }
        break;
      case read_mode::rest:
        if (v == SYM_AND_OPTIONAL) {
          throw std::exception{"misplaced &optional in lambda list"};
        } else if (v == SYM_AND_REST) {
          throw std::exception{"repeated &rest in lambda list"};
        } else if (v == SYM_AND_KEY) {
          read_mode = read_mode::key;
        } else if (v == SYM_AND_AUX) {
          read_mode = read_mode::aux;
        } else if (is_symbol(v)) {
          throw NotImplemented();
        } else {
          throw std::exception{"non-symbol in lambda list"};
        }
        break;
      case read_mode::key:
        if (v == SYM_AND_OPTIONAL) {
          throw std::exception{"misplaced &optional in lambda list"};
        } else if (v == SYM_AND_REST) {
          throw std::exception{"misplaced &rest in lambda list"};
        } else if (v == SYM_AND_KEY) {
          throw std::exception{"repeated &key in lambda list"};
        } else if (v == SYM_AND_AUX) {
          read_mode = read_mode::aux;
        } else if (is_symbol(v)) {
          throw NotImplemented();
        } else {
          throw std::exception{"non-symbol in lambda list"};
        }
        break;
      case read_mode::aux:
        if (v == SYM_AND_OPTIONAL) {
          throw std::exception{"misplaced &optional in lambda list"};
        } else if (v == SYM_AND_REST) {
          throw std::exception{"misplaced &rest in lambda list"};
        } else if (v == SYM_AND_KEY) {
          throw std::exception{"misplaced &key in lambda list"};
        } else if (v == SYM_AND_AUX) {
          throw std::exception{"repeated &aux in lambda list"};
        } else if (is_symbol(v)) {
          throw NotImplemented();
        } else {
          throw std::exception{"non-symbol in lambda list"};
        }
        break;
      }
    }
  }
}

} // namespace lisp