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

static bool is_number(scm_obj_t obj) { return is_fixnum(obj) || is_short_flonum(obj) || is_long_flonum(obj); }

static bool is_exact(scm_obj_t obj) { return is_fixnum(obj); }

static bool exact_eq_p(scm_obj_t obj1, scm_obj_t obj2) { return obj1 == obj2; }

static bool inexact_eq_p(scm_obj_t obj1, scm_obj_t obj2) { return flonum(obj1) == flonum(obj2); }

static bool find_and_merge_opponent(scm_obj_t visited, scm_obj_t lst1, scm_obj_t lst2) {
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

bool eqv_p(scm_obj_t obj1, scm_obj_t obj2) {
  if (obj1 == obj2) return true;
  if (is_number(obj1)) {
    if (is_number(obj2)) {
      if (is_exact(obj1)) {
        if (is_exact(obj2)) {
          return exact_eq_p(obj1, obj2);
        }
      } else {
        if (is_exact(obj2)) return false;
        return inexact_eq_p(obj1, obj2);
      }
    }
  }
  return false;
}

bool equal_p(scm_obj_t visited, scm_obj_t lst1, scm_obj_t lst2) {
top:
  if (lst1 == lst2) return true;
  if (is_cons(lst1)) {
    if (is_cons(lst2)) {
      if (find_and_merge_opponent(visited, lst1, lst2)) return true;
      if (equal_p(visited, pair_car(lst1), pair_car(lst2))) {
        lst1 = pair_cdr(lst1);
        lst2 = pair_cdr(lst2);
        goto top;
      }
    }
    return false;
  }
  if (is_vector(lst1)) {
    if (is_vector(lst2)) {
      if (find_and_merge_opponent(visited, lst1, lst2)) return true;
      int n1 = vector_nsize(lst1);
      int n2 = vector_nsize(lst2);
      if (n1 == n2) {
        scm_obj_t* elts1 = vector_elts(lst1);
        scm_obj_t* elts2 = vector_elts(lst2);
        for (int i = 0; i < n1; i++) {
          if (equal_p(visited, elts1[i], elts2[i])) continue;
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
  return eqv_p(lst1, lst2);
}
