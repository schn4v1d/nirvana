#pragma once

#include <optional>
#include <stdexcept>

template <class T> std::optional<T> optional_ptr(T ptr) {
  if (ptr == nullptr) {
    return std::nullopt;
  }

  return ptr;
}

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

class NotImplemented : public std::logic_error {
public:
  NotImplemented();
};