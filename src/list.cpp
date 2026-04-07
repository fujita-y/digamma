// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "list.h"

static inline bool container_p(scm_obj_t obj) { return is_cons(obj) || is_vector(obj); }

static scm_obj_t classify_list(scm_obj_t lst) {
  scm_obj_t fast = lst;
  scm_obj_t slow = lst;
  bool parent = false;
  while (is_cons(fast)) {
    scm_obj_t next = cons_cdr(fast);
    if (is_cons(next)) [[likely]] {
      if (cons_cdr(next) == cons_cdr(slow)) return scm_true;
      if (!parent) parent = container_p(cons_car(fast)) || container_p(cons_car(next));
      fast = cons_cdr(next);
      slow = cons_cdr(slow);
      continue;
    }
    // Odd-length list or improper list tail
    return (parent || container_p(cons_car(fast)) || container_p(next)) ? scm_nil : scm_false;
  }
  return (parent || container_p(fast)) ? scm_nil : scm_false;
}

struct ancestor_t {
  scm_obj_t* stack;
  int capacity;
  scm_obj_t buf[1024];

  ancestor_t() : stack(buf), capacity(array_sizeof(buf)) {}

  ~ancestor_t() {
    if (stack != buf) free(stack);
  }

  void push(scm_obj_t obj, int depth) {
    if (depth >= capacity) [[unlikely]] {
      int next_capacity = capacity + capacity;
      if (depth >= next_capacity) next_capacity = depth + 1;  // guard against large jumps
      if (stack == buf) {
        stack = (scm_obj_t*)malloc(sizeof(scm_obj_t) * next_capacity);
        memcpy(stack, buf, sizeof(buf));
      } else {
        stack = (scm_obj_t*)realloc(stack, sizeof(scm_obj_t) * next_capacity);
        if (stack == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
      }
      capacity = next_capacity;
    }
    stack[depth] = obj;
  }

  bool contains(scm_obj_t obj, int depth) const {
    for (int i = depth - 1; i >= 0; i--) {
      if (stack[i] == obj) return true;
    }
    return false;
  }
};

static bool cyclic_object_test(scm_obj_t obj, ancestor_t& ancestor, int depth) {
top:
  if (container_p(obj)) {
    if (ancestor.contains(obj, depth)) [[unlikely]]
      return true;
    if (is_cons(obj)) {
      // Fast-path: check if car is a cons (needs classify_list) or a non-cons container
      // (vector) or a leaf (no recursion needed).
      scm_obj_t car = cons_car(obj);
      if (is_cons(car)) {
        scm_obj_t type = classify_list(car);
        if (type == scm_true) [[unlikely]]
          return true;
        if (type == scm_nil) {
          ancestor.push(obj, depth++);
          if (cons_cdr(obj) == scm_nil) {
            obj = car;
            goto top;
          }
          if (cyclic_object_test(car, ancestor, depth)) return true;
        }
      } else if (is_vector(car)) {
        // Non-cons container (e.g. vector): always needs recursion
        ancestor.push(obj, depth++);
        if (cons_cdr(obj) == scm_nil) {
          obj = car;
          goto top;
        }
        if (cyclic_object_test(car, ancestor, depth)) return true;
      }
      // car is a leaf (fixnum, symbol, bool, etc.): no recursion needed
      obj = cons_cdr(obj);
      goto top;
    }
    if (is_vector(obj)) {
      int n = vector_nsize(obj);
      if (n == 0) return false;
      ancestor.push(obj, depth++);
      for (int i = 0; i < n - 1; i++) {
        if (cyclic_object_test(vector_elts(obj)[i], ancestor, depth)) return true;
      }
      obj = vector_elts(obj)[n - 1];
      goto top;
    }
  }
  return false;
}

bool cyclic_object_p(scm_obj_t obj) {
  scm_obj_t type = classify_list(obj);
  if (type == scm_true) return true;
  if (type == scm_false) return false;
  ancestor_t ancestor;
  return cyclic_object_test(obj, ancestor, 0);
}