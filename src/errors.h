#pragma once

#include <exception>
#include <string>

namespace lisp {

struct CellError : public std::exception {
  [[nodiscard]] const char *what() const override = 0;
};

class UnboundVariable : public CellError {
  std::string message;

public:
  explicit UnboundVariable(const std::string& name);

  [[nodiscard]] const char *what() const override;
};

class UndefinedFunction : public CellError {
  std::string message;

public:
  explicit UndefinedFunction(const std::string& name);

  [[nodiscard]] const char *what() const override;
};

} // namespace lisp