#pragma once

#include <memory>
#include <ranges>
#include <variant>
#include <vector>

#include "util.h"

namespace lisp::object {

template <class... args> struct object_variant_helper;

template <class... args, class ref>
struct object_variant_helper<std::variant<args...>, ref> {
  using type = std::variant<args..., ref>;
};

template <class variant, class ref>
using object_variant = typename object_variant_helper<variant, ref>::type;

class nil {};

struct GcObject;

class ObjectPtrInfo {
  GcObject *ptr;
  bool traced;
  size_t root_count = 0;
  size_t ref_count = 0;

  void check();

public:
  explicit ObjectPtrInfo(GcObject *ptr, bool traced);

  void dec_root();
  void inc_root();
  void dec_ref();
  void inc_ref();
  [[nodiscard]] size_t get_root_count() const;
  [[nodiscard]] size_t get_ref_count() const;
  bool get_traced() const;
  void set_traced(bool value);
};

class ManagedObjectRoot {
  GcObject *ptr;
  ObjectPtrInfo *info;

  friend class ManagedObjectRef;

public:
  explicit ManagedObjectRoot(GcObject &&obj, bool traced);

  ManagedObjectRoot(const ManagedObjectRoot &root);
  ManagedObjectRoot(ManagedObjectRoot &&root) noexcept;

  ManagedObjectRoot &operator=(const ManagedObjectRoot &other);
  ManagedObjectRoot &operator=(ManagedObjectRoot &&other) noexcept;

  ~ManagedObjectRoot();

  GcObject *operator->() const;

  [[nodiscard]] size_t get_root_count() const;
  [[nodiscard]] size_t get_ref_count() const;
};

class ManagedObjectRef {
  GcObject *ptr;
  ObjectPtrInfo *info;

public:
  ManagedObjectRef(ManagedObjectRoot &&root) : ptr{root.ptr}, info{root.info} {
    info->dec_root();
    info->inc_ref();
  }

  ManagedObjectRef(const ManagedObjectRoot &root)
      : ptr{root.ptr}, info{root.info} {
    info->inc_ref();
  }

  ManagedObjectRef(ManagedObjectRef &&ref) noexcept
      : ptr{ref.ptr}, info{ref.info} {}

  ManagedObjectRef(const ManagedObjectRef &ref) : ptr{ref.ptr}, info{ref.info} {
    info->inc_ref();
  }

  ManagedObjectRef &operator=(const ManagedObjectRef &other) {
    if (&other != this) {
      info->dec_ref();
      ptr = other.ptr;
      info = other.info;
      info->inc_ref();
    }

    return *this;
  }

  ManagedObjectRef &operator=(ManagedObjectRef &&other) noexcept {
    if (&other != this) {
      info->dec_ref();
      ptr = other.ptr;
      info = other.info;
    }

    return *this;
  }

  ~ManagedObjectRef() { info->dec_ref(); }

  GcObject *operator->() const;

  [[nodiscard]] bool get_traced() const;
  void set_traced(bool value);
};

class Tracer {
  bool traced_color;
  std::vector<ManagedObjectRef> worklist;

public:
  Tracer(bool traced_color, std::vector<ManagedObjectRef> &&worklist);

  void trace_obj_ref(const ManagedObjectRef &ref);
  void mark_all();
};

using InlineObject = std::variant<int, char, nil>;

struct Object;

struct Cons;

struct Symbol;

class Object : public object_variant<InlineObject, ManagedObjectRef> {
  bool is_managed();
  ManagedObjectRef &as_managed();

public:
  using variant::variant;

  bool is_integer();
  int &as_integer();

  bool is_nil();

  bool is_cons();
  Cons &as_cons();

  bool is_symbol();
  Symbol &as_symbol();

  void trace(Tracer &tracer);
};

struct Cons : public std::pair<Object, Object> {
  using pair::pair;

  Object &car();
  Object &cdr();
};

class InternedSymbol {
  std::string name;
  Object package;
};

class UninternedSymbol {
  std::string name;
};

struct Symbol : public std::variant<InternedSymbol, UninternedSymbol> {
  using variant::variant;
};

class Package {};

struct GcObject : public std::variant<Cons, Symbol, Package> {
  using variant::variant;

  [[nodiscard]] bool is_cons() const;
  Cons &as_cons();

  [[nodiscard]] bool is_symbol() const;
  Symbol &as_symbol();

  void trace(Tracer &tracer);
};

class MemoryManager {
  std::vector<ManagedObjectRoot> objects{};
  bool traced_color = true;
  size_t next_gc = 32;

  ManagedObjectRoot raw_allocate(GcObject &&object);

  auto roots();

public:
  ManagedObjectRoot allocate_object(GcObject &&object);
  void collect();
};
} // namespace lisp::object