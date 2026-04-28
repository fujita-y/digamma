// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "hash.h"
#include "subr.h"

// ============================================================================
// Hashtables
// ============================================================================

// hashtable?  - R6RS 12.1
SUBR subr_hashtable_p(scm_obj_t self, scm_obj_t a1) { return is_hashtable(a1) ? scm_true : scm_false; }

// equal-hash  - R6RS 12.4
SUBR subr_equal_hash(scm_obj_t self, scm_obj_t a1) { return make_fixnum((intptr_t)equal_hash(a1, INT32_MAX)); }

// make-eq-hashtable  - R6RS 12.2
SUBR subr_make_eq_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("make-eq-hashtable: too many arguments");
  int capacity = 32;
  if (argc == 1) {
    if (!is_fixnum(argv[0])) throw std::runtime_error("make-eq-hashtable: capacity must be an exact integer");
    intptr_t cap = fixnum(argv[0]);
    if (cap < 0) throw std::runtime_error("make-eq-hashtable: capacity must be non-negative");
    capacity = (int)cap;
  }
  return make_hashtable(address_hash, address_equiv, capacity);
}

// make-eqv-hashtable  - R6RS 12.2
SUBR subr_make_eqv_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("make-eqv-hashtable: too many arguments");
  int capacity = 32;
  if (argc == 1) {
    if (!is_fixnum(argv[0])) throw std::runtime_error("make-eqv-hashtable: capacity must be an exact integer");
    intptr_t cap = fixnum(argv[0]);
    if (cap < 0) throw std::runtime_error("make-eqv-hashtable: capacity must be non-negative");
    capacity = (int)cap;
  }
  return make_hashtable(eqv_hash, eqv_equiv, capacity);
}

// make-equal-hashtable  - digamma extension
SUBR subr_make_equal_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc > 1) throw std::runtime_error("make-equal-hashtable: too many arguments");
  int capacity = 32;
  if (argc == 1) {
    if (!is_fixnum(argv[0])) throw std::runtime_error("make-equal-hashtable: capacity must be an exact integer");
    intptr_t cap = fixnum(argv[0]);
    if (cap < 0) throw std::runtime_error("make-equal-hashtable: capacity must be non-negative");
    capacity = (int)cap;
  }
  return make_hashtable(equal_hash, equal_equiv, capacity);
}

// hashtable-ref  - R6RS 12.3
// (hashtable-ref ht key default)
SUBR subr_hashtable_ref(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 2 || argc > 3) throw std::runtime_error("hashtable-ref: wrong number of arguments");
  if (!is_hashtable(argv[0])) throw std::runtime_error("hashtable-ref: first argument must be a hashtable");
  scm_obj_t default_val = (argc == 3) ? argv[2] : scm_undef;
  scm_obj_t result = hashtable_ref(argv[0], argv[1], default_val);
  if (argc == 2 && result == scm_undef) throw std::runtime_error("hashtable-ref: key not found");
  return result;
}

// hashtable-set!  - R6RS 12.3
// (hashtable-set! ht key value)
SUBR subr_hashtable_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-set!: first argument must be a hashtable");
  hashtable_set(a1, a2, a3);
  return scm_unspecified;
}

// hashtable-delete!  - R6RS 12.3
// (hashtable-delete! ht key)
SUBR subr_hashtable_delete(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-delete!: first argument must be a hashtable");
  hashtable_delete(a1, a2);
  return scm_unspecified;
}

// hashtable-contains?  - R6RS 12.3
// (hashtable-contains? ht key)
SUBR subr_hashtable_contains(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-contains?: first argument must be a hashtable");
  scm_obj_t result = hashtable_ref(a1, a2, scm_undef);
  return (result != scm_undef) ? scm_true : scm_false;
}

// hashtable-clear!  - R6RS 12.3
// (hashtable-clear! ht)
SUBR subr_hashtable_clear(scm_obj_t self, scm_obj_t a1) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-clear!: first argument must be a hashtable");
  hashtable_clear(a1);
  return scm_unspecified;
}

// hashtable-entries  - R6RS 12.3
// (hashtable-entries ht) => (values keys-vector values-vector)
SUBR subr_hashtable_entries(scm_obj_t self, scm_obj_t a1) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable-entries: argument must be a hashtable");
  scm_hashtable_rec_t* ht = (scm_hashtable_rec_t*)to_address(a1);
  hashtable_aux_t* aux = ht->aux;
  int nsize = aux->capacity;

  // Count live entries
  int live = aux->live;

  // Allocate result vectors
  scm_obj_t keys_vec = make_vector(live, scm_unspecified);
  scm_obj_t vals_vec = make_vector(live, scm_unspecified);
  scm_obj_t* keys_elts = vector_elts(keys_vec);
  scm_obj_t* vals_elts = vector_elts(vals_vec);

  int out = 0;
  for (int i = 0; i < nsize; i++) {
    scm_obj_t k = aux->elts[i];
    if (k == scm_hash_free || k == scm_hash_deleted) continue;
    keys_elts[out] = k;
    vals_elts[out] = aux->elts[i + nsize];
    out++;
  }

  // Pack into a 2-element values object
  scm_obj_t result = make_values(2);
  values_elts(result)[0] = keys_vec;
  values_elts(result)[1] = vals_vec;
  return result;
}

// hashtable->alist  - digamma extension
// (hashtable->alist ht) => ((key . value) ...)
// Like hashtable-entries but returns an association list instead of two vectors.
SUBR subr_hashtable_alist(scm_obj_t self, scm_obj_t a1) {
  if (!is_hashtable(a1)) throw std::runtime_error("hashtable->alist: argument must be a hashtable");
  scm_hashtable_rec_t* ht = (scm_hashtable_rec_t*)to_address(a1);
  hashtable_aux_t* aux = ht->aux;
  int nsize = aux->capacity;

  scm_obj_t head = scm_nil;
  scm_obj_t tail = scm_nil;
  for (int i = 0; i < nsize; i++) {
    scm_obj_t k = aux->elts[i];
    if (k == scm_hash_free || k == scm_hash_deleted) continue;
    scm_obj_t pair = make_cons(k, aux->elts[i + nsize]);
    scm_obj_t cell = make_cons(pair, scm_nil);
    if (head == scm_nil) {
      head = cell;
      tail = cell;
    } else {
      scm_cons_rec_t* tail_cons = (scm_cons_rec_t*)tail;
      tail_cons->cdr = cell;
      tail = cell;
    }
  }
  return head;
}

// ============================================================================
// Registration
// ============================================================================

void init_subr_hash() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("hashtable?", (void*)subr_hashtable_p, 1, false);
  reg("equal-hash", (void*)subr_equal_hash, 1, false);
  reg("make-eq-hashtable", (void*)subr_make_eq_hashtable, 0, true);
  reg("make-eqv-hashtable", (void*)subr_make_eqv_hashtable, 0, true);
  reg("make-equal-hashtable", (void*)subr_make_equal_hashtable, 0, true);
  reg("hashtable-ref", (void*)subr_hashtable_ref, 2, true);
  reg("hashtable-set!", (void*)subr_hashtable_set, 3, false);
  reg("hashtable-delete!", (void*)subr_hashtable_delete, 2, false);
  reg("hashtable-contains?", (void*)subr_hashtable_contains, 2, false);
  reg("hashtable-clear!", (void*)subr_hashtable_clear, 1, false);
  reg("hashtable-entries", (void*)subr_hashtable_entries, 1, false);
  reg("hashtable->alist", (void*)subr_hashtable_alist, 1, false);
}
