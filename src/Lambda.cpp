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

  lambda_list.bind_arguments(argsv, inner);

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
