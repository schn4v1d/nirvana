#include "object.h"

#include <utility>

namespace lisp::object {

bool Object::is_managed() {
  return std::holds_alternative<ManagedObjectRef>(*this);
}

ManagedObjectRef &Object::as_managed() {
  return std::get<ManagedObjectRef>(*this);
}

bool Object::is_cons() { return is_managed() && as_managed()->is_cons(); }

Cons &Object::as_cons() { return as_managed()->as_cons(); }

bool Object::is_integer() { return std::holds_alternative<int>(*this); }

int &Object::as_integer() { return std::get<int>(*this); }

bool Object::is_nil() { return std::holds_alternative<nil>(*this); }

bool Object::is_symbol() { return is_managed() && as_managed()->is_symbol(); }

Symbol &Object::as_symbol() { return as_managed()->as_symbol(); }

void Object::trace(Tracer &tracer) {
  std::visit(overloaded{[&tracer](const ManagedObjectRef &ref) {
                          tracer.trace_obj_ref(ref);
                          ref->trace(tracer);
                        },
                        [](const auto &) {}},
             *this);
}

void ObjectPtrInfo::check() {
  if (root_count == 0) {
    delete ptr;
    delete this;
  }
}

ObjectPtrInfo::ObjectPtrInfo(GcObject *ptr, bool traced)
    : ptr{ptr}, traced{traced} {}
void ObjectPtrInfo::dec_root() {
  root_count--;
  check();
}
void ObjectPtrInfo::inc_root() { root_count++; }
void ObjectPtrInfo::dec_ref() { ref_count--; }
void ObjectPtrInfo::inc_ref() { ref_count++; }
size_t ObjectPtrInfo::get_root_count() const { return root_count; }
size_t ObjectPtrInfo::get_ref_count() const { return ref_count; }
bool ObjectPtrInfo::get_traced() const { return traced; }
void ObjectPtrInfo::set_traced(bool value) { traced = value; }

ManagedObjectRoot::ManagedObjectRoot(GcObject &&obj, bool traced)
    : ptr{new GcObject(obj)}, info{new ObjectPtrInfo(ptr, traced)} {
  info->inc_root();
}

GcObject *ManagedObjectRoot::operator->() const { return ptr; }
ManagedObjectRoot::ManagedObjectRoot(const ManagedObjectRoot &root)
    : ptr{root.ptr}, info{root.info} {
  info->inc_root();
}
ManagedObjectRoot::ManagedObjectRoot(ManagedObjectRoot &&root) noexcept
    : ptr{root.ptr}, info{root.info} {}
ManagedObjectRoot &
ManagedObjectRoot::operator=(const ManagedObjectRoot &other) {
  if (&other != this) {
    if (other.info != info) {
      info->dec_root();
      ptr = other.ptr;
      info = other.info;
    }

    info->inc_root();
  }

  return *this;
}
ManagedObjectRoot &
ManagedObjectRoot::operator=(ManagedObjectRoot &&other) noexcept {
  if (&other != this) {
    if (other.info != info) {
      info->dec_root();
      ptr = other.ptr;
      info = other.info;
    }
  }

  return *this;
}

ManagedObjectRoot::~ManagedObjectRoot() { info->dec_root(); }

size_t ManagedObjectRoot::get_root_count() const {
  return info->get_root_count();
}

size_t ManagedObjectRoot::get_ref_count() const {
  return info->get_ref_count();
}

GcObject *ManagedObjectRef::operator->() const { return ptr; }
bool ManagedObjectRef::get_traced() const { return info->get_traced(); }
void ManagedObjectRef::set_traced(bool value) { info->set_traced(value); }

bool GcObject::is_cons() const { return std::holds_alternative<Cons>(*this); }

Cons &GcObject::as_cons() { return std::get<Cons>(*this); }

bool GcObject::is_symbol() const {
  return std::holds_alternative<Symbol>(*this);
}

Symbol &GcObject::as_symbol() { return std::get<Symbol>(*this); }

void GcObject::trace(Tracer &tracer) {
  std::visit(overloaded{[&tracer](Cons &obj) {
                          obj.car().trace(tracer);
                          obj.car().trace(tracer);
                        },
                        [&tracer](Symbol &obj) {}, [&tracer](Package &obj) {}},
             *this);
}

ManagedObjectRoot MemoryManager::raw_allocate(GcObject &&object) {
  ManagedObjectRoot root{
      std::move(object),
      !traced_color,
  };

  objects.push_back(root);

  return root;
}

auto MemoryManager::roots() {
  return objects | std::views::filter([](const ManagedObjectRoot &obj) {
           return obj.get_root_count() > 1;
         });
}

void MemoryManager::collect() {
  while (true) {
    auto before = objects.size();

    std::erase_if(objects, [](const ManagedObjectRoot &obj) {
      return !(obj.get_root_count() > 1 || obj.get_ref_count() > 0);
    });

    if (objects.size() == before) {
      break;
    }
  }

  std::vector<ManagedObjectRef> worklist =
      roots() |
      std::views::transform(
          [](const ManagedObjectRoot &obj) -> ManagedObjectRef {
            return obj;
          }) |
      std::ranges::to<std::vector>();

  Tracer tracer{traced_color, std::move(worklist)};

  tracer.mark_all();

  std::erase_if(objects, [this](const ManagedObjectRef &obj) {
    return obj.get_traced() != traced_color;
  });

  traced_color = !traced_color;
}

ManagedObjectRoot MemoryManager::allocate_object(GcObject &&object) {
  ManagedObjectRoot root{raw_allocate(std::move(object))};

  if (objects.size() >= next_gc) {
    collect();

    next_gc *= 2;
  }

  return root;
}

Object &Cons::car() { return first; }

Object &Cons::cdr() { return second; }

Tracer::Tracer(bool traced_color, std::vector<ManagedObjectRef> &&worklist)
    : traced_color{traced_color}, worklist{std::move(worklist)} {}

void Tracer::trace_obj_ref(const ManagedObjectRef &ref) {
  if (ref.get_traced() != traced_color) {
    worklist.push_back(ref);
  }
}

void Tracer::mark_all() {
  std::vector<ManagedObjectRef> current_worklist{};

  std::swap(worklist, current_worklist);

  while (!current_worklist.empty()) {
    for (ManagedObjectRef &obj : current_worklist) {
      if (obj.get_traced() != traced_color) {
        obj.set_traced(traced_color);
        obj->trace(*this);
      }
    }

    current_worklist.clear();

    std::swap(worklist, current_worklist);
  }
}

} // namespace lisp::object
