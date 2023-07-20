#include "BuiltinFunction.h"
#include "Cons.h"
#include "GarbageCollector.h"
#include "Package.h"
#include "cl_fun.h"
#include "eval.h"
#include "reader.h"
#include <filesystem>
#include <fstream>
#include <iostream>

using namespace lisp;

Value execute(std::string_view code, Environment *env) {
  std::istringstream input_stream{code.data()};

  Value result = NIL;

  try {
    while (true) {
      Value v = read(input_stream, env);
      result = eval(v, env);
    }
  } catch (ReadEndOfFile &e) {
  }

  return result;
}

void scan_dependencies(Environment *env) {
  auto load = [&](auto path) {
    std::ifstream t{path};
    if (!t.is_open()) {
      throw std::exception("invalid load path");
    }
    std::stringstream buffer;
    buffer << t.rdbuf();
    std::istringstream input_stream{buffer.str()};
    std::vector<Value> forms{};

    try {
      while (true) {
        forms.push_back(read(input_stream, env));
      }
    } catch (ReadEndOfFile &e) {
    }

    return forms;
  };

  std::ofstream f{"dependencies.md"};

  f << "```mermaid\nflowchart BT" << std::endl;

  std::function<void(Value, Value)> read_form = [&](Value source, Value form) {
    if (is_cons(form) && is_symbol(cl::car(form))) {
      f << "id" << source.tag << "[" << source << "]-->id" << cl::car(form).tag << "[" << cl::car(form) << "]" << std::endl;

      map_list(
          [&](Value form) {
            read_form(source, form);
            return NIL;
          },
          cl::cdr(form));
    }
  };

  auto read_dependencies = [&](auto forms) {
    for (Value form : forms) {
      if (!is_cons(form)) {
        continue;
      }

      if (cl::car(form) == SYM_DEFUN) {
        Value source = cl::cadr(form);
        form = cl::cdddr(form);

        map_list(
            [&](Value form) {
              read_form(source, form);

              return NIL;
            },
            form);
      }
    }
  };

  read_dependencies(load("cl/core.lisp"));
  read_dependencies(load("cl/cons.lisp"));
  read_dependencies(load("cl/dolist.lisp"));

  f << "```" << std::endl;
}

int main() {
  try {
    init_packages();
    init_symbols();
    init_reader();
    init_eval();
    init_builtin_functions();

    Environment *environment = make_environment();

//    scan_dependencies(environment);

    execute("(load \"cl/common-lisp.lisp\")", environment);

    std::cout << "loaded core successfully" << std::endl;

    std::cout << execute("(load \"cl/test.lisp\")", environment) << std::endl;

    try {
      execute("(load \"cl/ansi-test/doit.lsp\")", environment);

      std::cout << "ansi tests succeeded!" << std::endl;
    } catch (std::exception &e) {
      std::cout << e.what() << std::endl;
      std::cout << "ansi tests failed." << std::endl;
    }
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
}
