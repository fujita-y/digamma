// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "equiv.h"
#include "hash.h"

#include <cstring>

static bool is_number(scm_obj_t obj) { return is_fixnum(obj) || is_short_flonum(obj) || is_long_flonum(obj); }

static bool is_exact(scm_obj_t obj) { return is_fixnum(obj); }

static bool exact_eq_p(scm_obj_t obj1, scm_obj_t obj2) { return obj1 == obj2; }

static bool inexact_eq_p(scm_obj_t obj1, scm_obj_t obj2) { return flonum(obj1) == flonum(obj2); }

static bool find_and_merge_opponent(scm_obj_t visited, scm_obj_t lst1, scm_obj_t lst2) {
  scm_obj_t opponents = hashtable_ref(visited, lst1, scm_undef);
  if (opponents != scm_undef) {
    scm_obj_t lst = opponents;
    while (is_cons(lst)) {
      if (cons_car(lst) != lst2) {
        lst = cons_cdr(lst);
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

static bool safe_equal_p(scm_obj_t visited, scm_obj_t lst1, scm_obj_t lst2) {
top:
  if (lst1 == lst2) return true;
  if (is_cons(lst1)) {
    if (is_cons(lst2)) {
      if (find_and_merge_opponent(visited, lst1, lst2)) return true;
      if (safe_equal_p(visited, cons_car(lst1), cons_car(lst2))) {
        lst1 = cons_cdr(lst1);
        lst2 = cons_cdr(lst2);
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
          if (safe_equal_p(visited, elts1[i], elts2[i])) continue;
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

constexpr int is_not_equal = 0;
constexpr int is_equal = 1;
constexpr int is_unknown = -1;

constexpr int pt_depth = 5;
constexpr int pt_siblings = 1;

static int limited_equal_p(scm_obj_t lst1, scm_obj_t lst2, int limit) {
top:
  if (lst1 == lst2) return is_equal;
  if (limit < 0) return is_unknown;
  if (is_cons(lst1)) {
    if (is_cons(lst2)) {
      int result = limited_equal_p(cons_car(lst1), cons_car(lst2), limit - pt_depth);
      if (result == is_equal) {
        lst1 = cons_cdr(lst1);
        lst2 = cons_cdr(lst2);
        limit -= pt_siblings;
        goto top;
      }
      return result;
    }
    return is_not_equal;
  }
  if (is_vector(lst1)) {
    if (is_vector(lst2)) {
      int n1 = vector_nsize(lst1);
      int n2 = vector_nsize(lst2);
      if (n1 == n2) {
        scm_obj_t* elts1 = vector_elts(lst1);
        scm_obj_t* elts2 = vector_elts(lst2);
        for (int i = 0; i < n1; i++) {
          int result = limited_equal_p(elts1[i], elts2[i], limit - pt_depth - i * pt_siblings);
          if (result == is_equal) continue;
          return result;
        }
        return is_equal;
      }
    }
    return is_not_equal;
  }
  if (is_u8vector(lst1)) {
    if (is_u8vector(lst2)) {
      int n1 = u8vector_nsize(lst1);
      int n2 = u8vector_nsize(lst2);
      if (n1 == n2) {
        return (memcmp(u8vector_elts(lst1), u8vector_elts(lst2), n1) == 0) ? is_equal : is_not_equal;
      }
    }
    return is_not_equal;
  }
  if (is_string(lst1) && is_string(lst2)) {
    return string_equiv(lst1, lst2) ? is_equal : is_not_equal;
  }
  return eqv_p(lst1, lst2) ? is_equal : is_not_equal;
}

bool equal_p(scm_obj_t a1, scm_obj_t a2) {
  int result = limited_equal_p(a1, a2, 1000);
  if (result == is_not_equal) return false;
  if (result == is_equal) return true;
  scm_obj_t visited = make_hashtable(address_hash, address_equiv, 128);
  return safe_equal_p(visited, a1, a2);
}
