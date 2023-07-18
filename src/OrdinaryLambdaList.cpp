#include "OrdinaryLambdaList.h"
#include "Cons.h"
#include "util.h"
#include <set>
#include <sstream>

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

    std::set<Value> used_symbols{};

    auto check_symbol_used = [&](Value v) {
      if (used_symbols.contains(v)) {
        std::ostringstream oss{};
        oss << "the variable " << v
            << " occurs more than once in the lambda list";
        std::string str{oss.str()};
        throw std::exception{str.c_str()};
      }
    };

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
          check_symbol_used(v);
          used_symbols.insert(v);
          required.push_back(get_symbol(v));
        } else {
          throw std::exception{"required argument not a symbol"};
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
          check_symbol_used(v);
          used_symbols.insert(v);
          optional.emplace_back(get_symbol(v), std::nullopt, std::nullopt);
        } else if (is_cons(v)) {
          Cons *opt = get_cons(v);

          Symbol *sym;
          if (is_symbol(opt->get_car())) {
            check_symbol_used(opt->get_car());
            used_symbols.insert(opt->get_car());
            sym = get_symbol(opt->get_car());
          } else {
            throw std::exception{"non-symbol in lambda list"};
          }

          if (is_cons(opt->get_cdr())) {
            opt = get_cons(opt->get_cdr());
            Value init_form = opt->get_car();
            if (is_cons(opt->get_cdr())) {
              opt = get_cons(opt->get_cdr());
              if (is_symbol(opt->get_car())) {
                check_symbol_used(opt->get_car());
                used_symbols.insert(opt->get_car());
                Symbol *supplied = get_symbol(opt->get_car());
                if (!is_nil(opt->get_cdr())) {
                  throw std::exception{"invalid lambda list syntax"};
                }
                optional.emplace_back(sym, init_form, supplied);
              } else {
                throw std::exception{"non-symbol in lambda list"};
              }
            } else if (!is_nil(opt->get_cdr())) {
              throw std::exception{"invalid lambda list syntax"};
            }
          } else {
            optional.emplace_back(sym, std::nullopt, std::nullopt);
          }
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

OrdinaryLambdaList::OrdinaryLambdaList(OrdinaryLambdaList &&other) noexcept =
    default;

void OrdinaryLambdaList::bind_arguments(Value argsv, Environment *env) {
  if (is_nil(argsv)) {
    return;
  }

  if (!is_cons(argsv)) {
    throw std::exception{"invalid call arguments"};
  }

  Cons *args = get_cons(argsv);
  size_t i = 0;

  for (Cons::iterator it = args->begin(); it != args->end(); ++it, ++i) {
    if (i < required.size()) {
      env->bind_lexical_variable(required[i]->make_value(), *it);
    }
  }

  if (i < required.size()) {
    throw std::exception{"not enough arguments"};
  }
}

} // namespace lisp