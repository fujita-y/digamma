// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "equiv.h"
#include "hash.h"
#include "object_heap.h"

#include <cstring>

static inline scm_obj_t pair_car(scm_obj_t obj) { return ((scm_cons_rec_t*)obj)->car; }

static inline scm_obj_t pair_cdr(scm_obj_t obj) { return ((scm_cons_rec_t*)obj)->cdr; }

static bool number_pred(scm_obj_t obj) { return is_fixnum(obj) || is_short_flonum(obj) || is_long_flonum(obj); }

static bool n_exact_pred(scm_obj_t obj) { return is_fixnum(obj); }

static bool n_exact_equal_pred(scm_obj_t obj1, scm_obj_t obj2) { return obj1 == obj2; }

static bool n_inexact_equal_pred(scm_obj_t obj1, scm_obj_t obj2) { return flonum(obj1) == flonum(obj2); }

bool eqv_pred(scm_obj_t obj1, scm_obj_t obj2) {
  if (obj1 == obj2) return true;

  if (number_pred(obj1)) {
    if (number_pred(obj2)) {
      if (n_exact_pred(obj1)) {
        if (n_exact_pred(obj2)) {
          return n_exact_equal_pred(obj1, obj2);
        }
      } else {
        if (n_exact_pred(obj2)) return false;
        return n_inexact_equal_pred(obj1, obj2);
      }
    }
  }
  return false;
}

static bool simple_equal_p(scm_obj_t lst1, scm_obj_t lst2) {
top:
  if (lst1 == lst2) return true;
  if (is_cons(lst1)) {
    if (is_cons(lst2)) {
      if (simple_equal_p(pair_car(lst1), pair_car(lst2))) {
        lst1 = pair_cdr(lst1);
        lst2 = pair_cdr(lst2);
        goto top;
      }
    }
    return false;
  }
  if (is_vector(lst1)) {
    if (is_vector(lst2)) {
      int n1 = vector_nsize(lst1);
      int n2 = vector_nsize(lst2);
      if (n1 == n2) {
        scm_obj_t* elts1 = vector_elts(lst1);
        scm_obj_t* elts2 = vector_elts(lst2);
        for (int i = 0; i < n1; i++) {
          if (simple_equal_p(elts1[i], elts2[i])) continue;
          return false;
        }
        return true;
      }
    }
    return false;
  }
  if (is_u8vector(lst1)) {
    if (is_u8vector(lst2)) {
      int n1 = u8vector_nsize(lst1);
      int n2 = u8vector_nsize(lst2);
      if (n1 == n2) {
        return (memcmp(u8vector_elts(lst1), u8vector_elts(lst2), n1) == 0);
      }
    }
    return false;
  }
  if (is_string(lst1) && is_string(lst2)) {
    return string_equiv(lst1, lst2);
  }
  return eqv_pred(lst1, lst2);
}

static int is_terminal_list(scm_obj_t maybe_list) {
  int count = 1;
  if (maybe_list == scm_nil) return count;
  scm_obj_t fast = maybe_list;
  scm_obj_t slow = fast;
  while (is_cons(fast)) {
    scm_obj_t elt = pair_car(fast);
    if (is_cons(elt) || is_vector(elt)) return 0;
    fast = pair_cdr(fast);
    count++;
    if (is_cons(fast)) {
      scm_obj_t elt = pair_car(fast);
      if (is_cons(elt) || is_vector(elt)) return 0;
    } else {
      return count;
    }
    fast = pair_cdr(fast);
    count++;
    slow = pair_cdr(slow);
    if (slow == fast) return 0;
  }
  if (is_vector(fast)) return 0;
  return count;
}

static bool find_and_merge_opponent(object_heap_t* heap, scm_obj_t visited, scm_obj_t lst1, scm_obj_t lst2) {
  scm_obj_t opponents = hashtable_ref(visited, lst1, scm_undef);
  if (opponents != scm_undef) {
    scm_obj_t lst = opponents;
    while (is_cons(lst)) {
      if (pair_car(lst) != lst2) {
        lst = pair_cdr(lst);
        continue;
      }
      return true;
    }
    hashtable_set(visited, lst1, make_cons(lst2, opponents));
  } else {
    hashtable_set(visited, lst1, scm_nil);
  }
  return false;
}

bool equal_p(object_heap_t* heap, scm_obj_t visited, scm_obj_t lst1, scm_obj_t lst2) {
  int c1 = is_terminal_list(lst1);
  if (c1) {
    if (c1 == is_terminal_list(lst2)) return simple_equal_p(lst1, lst2);
    return false;
  } else {
    if (is_terminal_list(lst2)) return false;
  }

top:
  if (lst1 == lst2) return true;
  if (is_cons(lst1)) {
    if (is_cons(lst2)) {
      if (find_and_merge_opponent(heap, visited, lst1, lst2)) return true;
      if (equal_p(heap, visited, pair_car(lst1), pair_car(lst2))) {
        lst1 = pair_cdr(lst1);
        lst2 = pair_cdr(lst2);
        goto top;
      }
    }
    return false;
  }
  if (is_vector(lst1)) {
    if (is_vector(lst2)) {
      if (find_and_merge_opponent(heap, visited, lst1, lst2)) return true;
      int n1 = vector_nsize(lst1);
      int n2 = vector_nsize(lst2);
      if (n1 == n2) {
        scm_obj_t* elts1 = vector_elts(lst1);
        scm_obj_t* elts2 = vector_elts(lst2);
        for (int i = 0; i < n1; i++) {
          if (equal_p(heap, visited, elts1[i], elts2[i])) continue;
          return false;
        }
        return true;
      }
    }
    return false;
  }
  if (is_u8vector(lst1)) {
    if (is_u8vector(lst2)) {
      int n1 = u8vector_nsize(lst1);
      int n2 = u8vector_nsize(lst2);
      if (n1 == n2) {
        return (memcmp(u8vector_elts(lst1), u8vector_elts(lst2), n1) == 0);
      }
    }
    return false;
  }
  if (is_string(lst1) && is_string(lst2)) {
    return string_equiv(lst1, lst2);
  }
  return eqv_pred(lst1, lst2);
}
