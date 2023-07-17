#include "Lambda.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "eval.h"
#include <iostream>

namespace lisp {

Lambda::Lambda(OrdinaryLambdaList lambda_list, Environment *env, Value body)
    : Object{OBJ_LAMBDA}, lambda_list(std::move(lambda_list)), env{env},
      body{body} {}

void Lambda::trace(bool marking) {
  mark(marking);
  trace_value(env->make_value(), marking);
  trace_value(body, marking);
}

Value Lambda::call(Value argsv) {
  Environment *inner = make_environment(env);

  if (is_cons(argsv)) {
    Cons *args = get_cons(argsv);
    size_t i = 0;

    for (Cons::iterator it = args->begin(); it != args->end(); ++it, ++i) {
      if (i < lambda_list.required.size()) {
        inner->bind_lexical_variable(lambda_list.required[i]->make_value(),
                                     *it);
      }
    }

    if (i < lambda_list.required.size()) {
      throw std::exception{"not enough arguments"};
    }
  } else if (!is_nil(argsv)) {
    throw std::exception{"invalid call arguments"};
  }

  Value result = NIL;
  map_list([&](Value expr) { return result = eval(expr, inner); }, body);
  return result;
}

bool is_lambda(Value value) {
  if (is_object(value)) {
    return get_object(value)->get_tag() == OBJ_LAMBDA;
  }

  return false;
}

Lambda *get_lambda(Value value) {
  return reinterpret_cast<Lambda *>(get_object(value));
}

Lambda *make_lambda(OrdinaryLambdaList lambda_list, Environment *env,
                    Value body) {
  return make_object<Lambda>(std::move(lambda_list), env, body);
}

Value make_lambda_v(OrdinaryLambdaList lambda_list, Environment *env,
                    Value body) {
  return make_lambda(std::move(lambda_list), env, body)->make_value();
}

} // namespace lisp
