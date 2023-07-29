#include "Cons.h"
#include "Frame.h"
#include "LexicalBlock.h"
#include "Symbol.h"
#include "cl_fun.h"
#include "eval.h"
#include "special_operators.h"

namespace lisp::special_operators {

Value op_tagbody(Value args, Environment *parent) {
  Environment *env = make_environment(parent);

  std::vector<Value> forms;
  std::unordered_map<Value, int> tags{};

  map_list(
      [&](Value form) {
        if (is_symbol(form) || is_integer(form)) {
          tags.insert({form, (int)forms.size()});
        } else {
          forms.push_back(form);
        }

        return NIL;
      },
      args);

  Frame *frame = env->establish_tagbody(tags);

  frame->get_tagbody().set_go(0);

  setjmp(frame->get_tagbody().jmp_buf);
  int go = frame->get_tagbody().get_go();
  while (go < forms.size()) {
    eval(forms[go], env);
    ++go;
  }

  env->unwind(frame);

  return NIL;
}

} // namespace lisp::special_operators