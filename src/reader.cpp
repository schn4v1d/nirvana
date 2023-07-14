#include "reader.h"
#include "Cons.h"
#include "Package.h"
#include "util.h"

#include <array>
#include <cassert>
#include <functional>
#include <iostream>

namespace lisp {

static std::array<syntax_type, 128> syntax_types{};
static std::map<char,
                std::function<Value(std::istringstream &, char, Environment *)>>
    reader_macros{};

void init_syntax_types() {
  using st = syntax_type;

  for (size_t i = 0; i < 128; i++) {
    syntax_types[i] = st::invalid;
  }

  syntax_types[0x8] = st::constituent;
  syntax_types['\t'] = st::whitespace;
  syntax_types['\n'] = st::whitespace;
  syntax_types['\r'] = st::whitespace;
  syntax_types[0xA] = st::whitespace;
  syntax_types[0xC] = st::whitespace;
  syntax_types[' '] = st::whitespace;
  syntax_types['!'] = st::constituent;
  syntax_types['"'] = st::macro_terminating;
  syntax_types['#'] = st::macro_non_terminating;
  syntax_types['$'] = st::constituent;
  syntax_types['%'] = st::constituent;
  syntax_types['&'] = st::constituent;
  syntax_types['\''] = st::macro_terminating;
  syntax_types['('] = st::macro_terminating;
  syntax_types[')'] = st::macro_terminating;
  syntax_types['*'] = st::constituent;
  syntax_types['+'] = st::constituent;
  syntax_types[','] = st::macro_terminating;
  syntax_types['-'] = st::constituent;
  syntax_types['.'] = st::constituent;
  syntax_types['/'] = st::constituent;

  for (char x = '0'; x <= '9'; x++) {
    syntax_types[x] = st::constituent;
  }

  syntax_types[':'] = st::constituent;
  syntax_types[';'] = st::macro_terminating;
  syntax_types['<'] = st::constituent;
  syntax_types['='] = st::constituent;
  syntax_types['>'] = st::constituent;
  syntax_types['?'] = st::constituent;
  syntax_types['@'] = st::constituent;

  for (char x = 'A'; x <= 'Z'; x++) {
    syntax_types[x] = st::constituent;
  }

  syntax_types['['] = st::constituent;
  syntax_types['\\'] = st::single_escape;
  syntax_types[']'] = st::constituent;
  syntax_types['^'] = st::constituent;
  syntax_types['_'] = st::constituent;
  syntax_types['`'] = st::macro_terminating;

  for (char x = 'a'; x <= 'z'; x++) {
    syntax_types[x] = st::constituent;
  }

  syntax_types['{'] = st::constituent;
  syntax_types['|'] = st::multiple_escape;
  syntax_types['}'] = st::constituent;
  syntax_types['~'] = st::constituent;
  syntax_types['{'] = st::constituent;
  syntax_types[0x7F] = st::constituent;
}

void init_reader_macros() {
  reader_macros['('] = [](std::istringstream &input, char _, Environment *env) {
    Value result = NIL;
    Cons *current_cons;

    while (true) {
      skip_whitespace(input);

      if (input.peek() == ')') {
        input.get();
        break;
      } else if (input.peek() == '.') {
        input.get();
        current_cons->set_cdr(read(input, env));
        skip_whitespace(input);
        assert(input.get() == ')');
        break;
      }

      Value next = read(input, env);

      if (is_nil(result)) {
        current_cons = make_cons(next, NIL);
        result = current_cons->make_value();
      } else {
        Cons *next_cons = make_cons(next, NIL);
        current_cons->set_cdr(next_cons->make_value());
        current_cons = next_cons;
      }
    }

    return result;
  };
}

void init_reader() {
  init_syntax_types();
  init_reader_macros();
}

bool is_whitespace(char x) {
  return syntax_types[x] == syntax_type::whitespace;
}

bool is_invalid(char x) { return syntax_types[x] == syntax_type::invalid; }

bool is_macro_terminating(char x) {
  return syntax_types[x] == syntax_type::macro_terminating;
}

bool is_macro_non_terminating(char x) {
  return syntax_types[x] == syntax_type::macro_non_terminating;
}

bool is_macro(char x) {
  return is_macro_terminating(x) || is_macro_non_terminating(x);
}

bool is_constituent(char x) {
  return syntax_types[x] == syntax_type::constituent;
}

bool is_single_escape(char x) {
  return syntax_types[x] == syntax_type::single_escape;
}

bool is_multiple_escape(char x) {
  return syntax_types[x] == syntax_type::multiple_escape;
}

void skip_whitespace(std::istringstream &input) {
  while (is_whitespace(input.peek())) {
    input.get();
  }
}

char readtable_case(char x) {
  if (x >= 'a' && x <= 'z') {
    return (char)(x + ('A' - 'a'));
  }

  return x;
}

std::string read_token(std::istringstream &input, std::string token) {
  while (input.peek(), !input.eof()) {
    char y = (char)input.get();

    if (is_constituent(y) || is_macro_non_terminating(y)) {
      y = readtable_case(y);

      token.push_back(y);
    } else if (is_single_escape(y)) {
      assert(false);
    } else if (is_multiple_escape(y)) {
      assert(false);
    } else if (is_invalid(y)) {
      assert(false);
    } else if (is_macro_terminating(y)) {
      input.unget();

      return token;
    } else if (is_whitespace(y)) {
      return token;
    }
  }

  return token;
}

bool is_digit(char x) { return '0' <= x && x <= '9'; }

bool parse_integer(std::string_view token, Value &value) {
  int sign = 1;
  if (token[0] == '+') {
    token = token.substr(1);
  } else if (token[0] == '-') {
    sign = -1;
    token = token.substr(1);
  }

  if (token.empty()) {
    return false;
  }

  int integer = 0;

  for (char x : token) {
    integer *= 10;

    if (is_digit(x)) {
      integer += (x - '0');
    } else {
      return false;
    }
  }

  value = make_value(integer * sign);

  return true;
}

bool parse_number(std::string_view token, Value &value) {
  return parse_integer(token, value);
}

bool parse_symbol(std::string_view token, Value &value, Environment *env) {
  value = get_package(env->lookup_special(SYM_STAR_PACKAGE_STAR))
              ->intern(token, false);

  return true;
}

Value parse_token(std::string_view token) {
  Value value{};

  parse_number(token, value) || parse_symbol(token, value);

  return value;
}

Value read(std::istringstream &input, Environment *env) {
  skip_whitespace(input);

  if (input.eof()) {
    assert(false);
  }

  char x = (char)input.get();

  std::string token;

  if (is_invalid(x)) {
    assert(false);
  }

  if (is_macro(x)) {
    return reader_macros[x](input, x, env);
  }

  if (is_single_escape(x)) {
    assert(false);
  }

  if (is_multiple_escape(x)) {
    assert(false);
  }

  if (is_constituent(x)) {
    x = readtable_case(x);
    token = read_token(input, {x});
  }

  return parse_token(token, env);
}

} // namespace lisp