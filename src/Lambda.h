#pragma once

#include "Environment.h"
#include "Object.h"
#include "OrdinaryLambdaList.h"

namespace lisp {

class Lambda : public Object {
  OrdinaryLambdaList lambda_list;
  Environment *env;
  Value body;

public:
  Lambda(OrdinaryLambdaList lambda_list, Environment *env, Value body);

  void trace(bool marking) override;
  Value call(Value args);
};

bool is_lambda(Value value);
Lambda *get_lambda(Value value);

Lambda *make_lambda(OrdinaryLambdaList lambda_list, Environment *env, Value body);
Value make_lambda_v(OrdinaryLambdaList lambda_list, Environment *env, Value body);

} // namespace lisp
