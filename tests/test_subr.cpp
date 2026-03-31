// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

// Direct tests for predicate subrs in nanos_subr.cpp.
// Each test calls the subr C function directly and checks the result
// against the R6RS specification.

#include "core.h"
#include "object.h"
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <initializer_list>
#include <vector>
#include "continuation.h"
#include "object_heap.h"

// ---------------------------------------------------------------------------
// Subr declarations (extern "C" via SUBR macro)
// ---------------------------------------------------------------------------

SUBR subr_boolean_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_char_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_eq_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_eqv_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_equal_p(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_exact_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_inexact_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_infinite_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_integer_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_list_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_null_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_number_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_nan_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_pair_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_procedure_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_real_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_string_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_symbol_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_vector_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_undefined_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_unspecified_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_not(scm_obj_t self, scm_obj_t a1);
SUBR subr_cons(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);

// New subrs (vectors, strings, chars, arithmetic)
SUBR subr_vector(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_vector_length(scm_obj_t self, scm_obj_t a1);
SUBR subr_vector_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_vector_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3);
SUBR subr_vector_to_list(scm_obj_t self, scm_obj_t a1);
SUBR subr_string_length(scm_obj_t self, scm_obj_t a1);
SUBR subr_string_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_string_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_string_append(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_substring(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3);
SUBR subr_symbol_to_string(scm_obj_t self, scm_obj_t a1);
SUBR subr_string_to_symbol(scm_obj_t self, scm_obj_t a1);
SUBR subr_number_to_string(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_string_to_number(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_char_eq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_char_numeric_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_max(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_caar(scm_obj_t self, scm_obj_t a1);
SUBR subr_cadar(scm_obj_t self, scm_obj_t a1);
SUBR subr_cadddr(scm_obj_t self, scm_obj_t a1);
SUBR subr_cddr(scm_obj_t self, scm_obj_t a1);
SUBR subr_cdddr(scm_obj_t self, scm_obj_t a1);
SUBR subr_set_car(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_length(scm_obj_t self, scm_obj_t a1);
SUBR subr_list_ref(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_list_to_vector(scm_obj_t self, scm_obj_t a1);
SUBR subr_memq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_memv(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_member(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_assq(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_assoc(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_reverse(scm_obj_t self, scm_obj_t a1);
SUBR subr_list(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_list_transpose(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_list_transpose_plus(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_cons_ast(scm_obj_t self, int argc, scm_obj_t argv[]);

// Hashtables
SUBR subr_hashtable_p(scm_obj_t self, scm_obj_t a1);
SUBR subr_equal_hash(scm_obj_t self, scm_obj_t a1);
SUBR subr_make_eq_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_make_eqv_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_make_equal_hashtable(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_hashtable_ref(scm_obj_t self, int argc, scm_obj_t argv[]);
SUBR subr_hashtable_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2, scm_obj_t a3);
SUBR subr_hashtable_delete(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_hashtable_contains(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_hashtable_clear(scm_obj_t self, scm_obj_t a1);
SUBR subr_hashtable_entries(scm_obj_t self, scm_obj_t a1);
SUBR subr_hashtable_alist(scm_obj_t self, scm_obj_t a1);
SUBR subr_values(scm_obj_t self, int argc, scm_obj_t argv[]);
// (call-with-values requires JIT bridge; tested in test_codegen)

// Environment access
SUBR subr_environment_macro_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_environment_macro_ref(scm_obj_t self, scm_obj_t a1);
SUBR subr_environment_macro_contains(scm_obj_t self, scm_obj_t a1);
SUBR subr_environment_variable_set(scm_obj_t self, scm_obj_t a1, scm_obj_t a2);
SUBR subr_environment_variable_ref(scm_obj_t self, scm_obj_t a1);
SUBR subr_environment_variable_contains(scm_obj_t self, scm_obj_t a1);
SUBR subr_uuid(scm_obj_t self);

// ---------------------------------------------------------------------------
// Required stubs
// ---------------------------------------------------------------------------

void fatal(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void warning(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
}

void trace(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

// ---------------------------------------------------------------------------
// Test infrastructure
// ---------------------------------------------------------------------------

static bool some_test_failed = false;

#define ASSERT_TRUE(expr)                         \
  do {                                            \
    if (!(expr)) {                                \
      printf("\033[31mFAIL: %s\033[0m\n", #expr); \
      some_test_failed = true;                    \
    } else {                                      \
      printf("\033[32mPASS: %s\033[0m\n", #expr); \
    }                                             \
  } while (0)

#define ASSERT_FALSE(expr)                           \
  do {                                               \
    if ((expr)) {                                    \
      printf("\033[31mFAIL: !(%s)\033[0m\n", #expr); \
      some_test_failed = true;                       \
    } else {                                         \
      printf("\033[32mPASS: !(%s)\033[0m\n", #expr); \
    }                                                \
  } while (0)

// Convenience: call a 1-arg predicate and check it returns scm_true
#define PRED_TRUE(fn, arg)    ASSERT_TRUE(fn(scm_nil, arg) == scm_true)
#define PRED_FALSE(fn, arg)   ASSERT_TRUE(fn(scm_nil, arg) == scm_false)

// Call a 2-arg predicate
#define PRED2_TRUE(fn, a, b)  ASSERT_TRUE(fn(scm_nil, a, b) == scm_true)
#define PRED2_FALSE(fn, a, b) ASSERT_TRUE(fn(scm_nil, a, b) == scm_false)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// Dummy closure used for procedure? tests
static scm_obj_t dummy_closure() { return make_closure(nullptr, 0, 0, 0, nullptr, 0); }

// Build a proper list (1 2 3)
static scm_obj_t make_proper_list_123() {
  scm_obj_t lst = scm_nil;
  lst = make_cons(make_fixnum(3), lst);
  lst = make_cons(make_fixnum(2), lst);
  lst = make_cons(make_fixnum(1), lst);
  return lst;
}

// Build an improper list (1 2 . 3)
static scm_obj_t make_improper_list() {
  scm_obj_t tail = make_cons(make_fixnum(2), make_fixnum(3));  // (2 . 3)
  return make_cons(make_fixnum(1), tail);                      // (1 2 . 3)
}

static scm_obj_t make_alist() {
  return make_cons(make_cons(make_symbol("a"), make_fixnum(1)),
                   make_cons(make_cons(make_symbol("b"), make_fixnum(2)), make_cons(make_cons(make_string("c"), make_fixnum(3)), scm_nil)));
}

// Build a circular list: (1 2 3 1 2 3 ...)
static scm_obj_t make_circular_list() {
  scm_obj_t p1 = make_cons(make_fixnum(1), scm_nil);
  scm_obj_t p2 = make_cons(make_fixnum(2), scm_nil);
  scm_obj_t p3 = make_cons(make_fixnum(3), scm_nil);
  CDR(p1) = p2;
  CDR(p2) = p3;
  CDR(p3) = p1;  // cycle
  return p1;
}

// ---------------------------------------------------------------------------
// boolean?  — R6RS §9.10
// ---------------------------------------------------------------------------

void test_boolean_p() {
  printf("--- boolean? ---\n");
  // #t and #f are the only booleans
  PRED_TRUE(subr_boolean_p, scm_true);
  PRED_TRUE(subr_boolean_p, scm_false);
  // Everything else is not a boolean
  PRED_FALSE(subr_boolean_p, make_fixnum(0));
  PRED_FALSE(subr_boolean_p, scm_nil);
  PRED_FALSE(subr_boolean_p, scm_undef);
  PRED_FALSE(subr_boolean_p, make_char('a'));
  PRED_FALSE(subr_boolean_p, make_symbol("foo"));
  PRED_FALSE(subr_boolean_p, make_string("foo"));
}

// ---------------------------------------------------------------------------
// char?  — R6RS §9.13
// ---------------------------------------------------------------------------

void test_char_p() {
  printf("--- char? ---\n");
  PRED_TRUE(subr_char_p, make_char('a'));
  PRED_TRUE(subr_char_p, make_char('Z'));
  PRED_TRUE(subr_char_p, make_char(0));        // NUL
  PRED_TRUE(subr_char_p, make_char(0x1F600));  // emoji (U+1F600)
  PRED_FALSE(subr_char_p, scm_true);
  PRED_FALSE(subr_char_p, make_fixnum(65));  // integer 65, not #\A
  PRED_FALSE(subr_char_p, scm_nil);
  PRED_FALSE(subr_char_p, make_string("a"));
}

// ---------------------------------------------------------------------------
// eq?  — R6RS §9.6
// ---------------------------------------------------------------------------

void test_eq_p() {
  printf("--- eq? ---\n");
  // Booleans: same singleton
  PRED2_TRUE(subr_eq_p, scm_true, scm_true);
  PRED2_TRUE(subr_eq_p, scm_false, scm_false);
  PRED2_FALSE(subr_eq_p, scm_true, scm_false);
  // Fixnums with same value are eq?
  PRED2_TRUE(subr_eq_p, make_fixnum(42), make_fixnum(42));
  PRED2_FALSE(subr_eq_p, make_fixnum(42), make_fixnum(43));
  // Symbols with the same name are eq? (interned)
  scm_obj_t sym1 = make_symbol("foo");
  scm_obj_t sym2 = make_symbol("foo");
  PRED2_TRUE(subr_eq_p, sym1, sym2);
  // nil is eq? to itself
  PRED2_TRUE(subr_eq_p, scm_nil, scm_nil);
  // Different singletons
  PRED2_FALSE(subr_eq_p, scm_nil, scm_undef);
  // Chars with same codepoint are eq? (immediate value)
  PRED2_TRUE(subr_eq_p, make_char('x'), make_char('x'));
  PRED2_FALSE(subr_eq_p, make_char('x'), make_char('y'));
  // Two separately allocated strings are not eq? even if equal content
  scm_obj_t s1 = make_string("hello");
  scm_obj_t s2 = make_string("hello");
  PRED2_FALSE(subr_eq_p, s1, s2);
  // But the same object is eq? to itself
  PRED2_TRUE(subr_eq_p, s1, s1);
}

// ---------------------------------------------------------------------------
// eqv?  — R6RS §9.6
// ---------------------------------------------------------------------------

void test_eqv_p() {
  printf("--- eqv? ---\n");
  // Same fixnums
  PRED2_TRUE(subr_eqv_p, make_fixnum(0), make_fixnum(0));
  PRED2_FALSE(subr_eqv_p, make_fixnum(1), make_fixnum(2));
  // Booleans
  PRED2_TRUE(subr_eqv_p, scm_true, scm_true);
  PRED2_FALSE(subr_eqv_p, scm_true, scm_false);
  // nil
  PRED2_TRUE(subr_eqv_p, scm_nil, scm_nil);
  // Flonums: eqv? compares by value
  PRED2_TRUE(subr_eqv_p, make_flonum(1.5), make_flonum(1.5));
  PRED2_FALSE(subr_eqv_p, make_flonum(1.5), make_flonum(2.0));
  // exact/inexact distinction: fixnum vs flonum
  PRED2_FALSE(subr_eqv_p, make_fixnum(1), make_flonum(1.0));
  // Symbols (interned)
  PRED2_TRUE(subr_eqv_p, make_symbol("bar"), make_symbol("bar"));
}

// ---------------------------------------------------------------------------
// equal?  — R6RS §9.6
// ---------------------------------------------------------------------------

void test_equal_p() {
  printf("--- equal? ---\n");
  // Fixnums
  PRED2_TRUE(subr_equal_p, make_fixnum(99), make_fixnum(99));
  PRED2_FALSE(subr_equal_p, make_fixnum(99), make_fixnum(100));
  // Strings: equal? compares by content
  PRED2_TRUE(subr_equal_p, make_string("abc"), make_string("abc"));
  PRED2_FALSE(subr_equal_p, make_string("abc"), make_string("xyz"));
  // Lists (structural equality)
  scm_obj_t l1 = make_proper_list_123();
  scm_obj_t l2 = make_proper_list_123();
  PRED2_TRUE(subr_equal_p, l1, l2);
  scm_obj_t l3 = make_cons(make_fixnum(1), make_cons(make_fixnum(2), make_cons(make_fixnum(9), scm_nil)));
  PRED2_FALSE(subr_equal_p, l1, l3);
  // Vectors
  scm_obj_t v1 = make_vector(3, make_fixnum(0));
  scm_obj_t v2 = make_vector(3, make_fixnum(0));
  PRED2_TRUE(subr_equal_p, v1, v2);
  scm_obj_t* elts = vector_elts(v2);
  elts[1] = make_fixnum(7);
  PRED2_FALSE(subr_equal_p, v1, v2);
  // Booleans
  PRED2_TRUE(subr_equal_p, scm_false, scm_false);
  PRED2_FALSE(subr_equal_p, scm_true, scm_false);
}

// ---------------------------------------------------------------------------
// exact?  — R6RS §9.9.4.1
// ---------------------------------------------------------------------------

void test_exact_p() {
  printf("--- exact? ---\n");
  // Fixnums are exact
  PRED_TRUE(subr_exact_p, make_fixnum(0));
  PRED_TRUE(subr_exact_p, make_fixnum(-1));
  PRED_TRUE(subr_exact_p, make_fixnum(1000000));
  // Flonums are inexact
  PRED_FALSE(subr_exact_p, make_flonum(1.0));
  PRED_FALSE(subr_exact_p, make_flonum(0.5));
  PRED_FALSE(subr_exact_p, make_flonum(-3.14));
}

// ---------------------------------------------------------------------------
// inexact?  — R6RS §9.9.4.1
// ---------------------------------------------------------------------------

void test_inexact_p() {
  printf("--- inexact? ---\n");
  // Flonums are inexact
  PRED_TRUE(subr_inexact_p, make_flonum(0.0));
  PRED_TRUE(subr_inexact_p, make_flonum(1.5));
  PRED_TRUE(subr_inexact_p, make_flonum(-999.9));
  // Fixnums are exact (not inexact)
  PRED_FALSE(subr_inexact_p, make_fixnum(0));
  PRED_FALSE(subr_inexact_p, make_fixnum(42));
}

// ---------------------------------------------------------------------------
// infinite?  — R6RS §9.9.4.5
// ---------------------------------------------------------------------------

void test_infinite_p() {
  printf("--- infinite? ---\n");
  // +inf.0 and -inf.0
  PRED_TRUE(subr_infinite_p, make_flonum(1.0 / 0.0));   // +inf
  PRED_TRUE(subr_infinite_p, make_flonum(-1.0 / 0.0));  // -inf
  // Finite flonum
  PRED_FALSE(subr_infinite_p, make_flonum(1.5));
  PRED_FALSE(subr_infinite_p, make_flonum(0.0));
  // NaN is not infinite
  PRED_FALSE(subr_infinite_p, make_flonum(0.0 / 0.0));
  // Exact integers are not infinite
  PRED_FALSE(subr_infinite_p, make_fixnum(0));
  PRED_FALSE(subr_infinite_p, make_fixnum(99999));
}

// ---------------------------------------------------------------------------
// integer?  — R6RS §9.9.4.1
// ---------------------------------------------------------------------------

void test_integer_p() {
  printf("--- integer? ---\n");
  // Fixnums are integers
  PRED_TRUE(subr_integer_p, make_fixnum(0));
  PRED_TRUE(subr_integer_p, make_fixnum(-42));
  PRED_TRUE(subr_integer_p, make_fixnum(1000));
  // Flonums that are whole numbers
  PRED_TRUE(subr_integer_p, make_flonum(1.0));
  PRED_TRUE(subr_integer_p, make_flonum(-3.0));
  PRED_TRUE(subr_integer_p, make_flonum(0.0));
  // Fractional flonum
  PRED_FALSE(subr_integer_p, make_flonum(1.5));
  PRED_FALSE(subr_integer_p, make_flonum(-0.1));
  // +inf.0 and nan are not integers
  PRED_FALSE(subr_integer_p, make_flonum(1.0 / 0.0));
  PRED_FALSE(subr_integer_p, make_flonum(0.0 / 0.0));
  // Non-numbers
  PRED_FALSE(subr_integer_p, scm_true);
  PRED_FALSE(subr_integer_p, make_string("1"));
}

// ---------------------------------------------------------------------------
// list?  — R6RS §9.12
// ---------------------------------------------------------------------------

void test_list_p() {
  printf("--- list? ---\n");
  // The empty list is a list
  PRED_TRUE(subr_list_p, scm_nil);
  // A proper list
  PRED_TRUE(subr_list_p, make_proper_list_123());
  // A one-element proper list
  PRED_TRUE(subr_list_p, make_cons(make_fixnum(1), scm_nil));
  // Improper list (1 2 . 3) is not a list
  PRED_FALSE(subr_list_p, make_improper_list());
  // A bare pair (dotted pair) is not a list
  PRED_FALSE(subr_list_p, make_cons(make_fixnum(1), make_fixnum(2)));
  // Circular list is not a list
  scm_obj_t circ = make_circular_list();
  PRED_FALSE(subr_list_p, circ);
  // Non-pairs are not lists (except '())
  PRED_FALSE(subr_list_p, make_fixnum(0));
  PRED_FALSE(subr_list_p, scm_true);
  PRED_FALSE(subr_list_p, make_string("x"));
}

// ---------------------------------------------------------------------------
// null?  — R6RS §9.12
// ---------------------------------------------------------------------------

void test_null_p() {
  printf("--- null? ---\n");
  PRED_TRUE(subr_null_p, scm_nil);
  PRED_FALSE(subr_null_p, scm_false);
  PRED_FALSE(subr_null_p, scm_true);
  PRED_FALSE(subr_null_p, make_fixnum(0));
  PRED_FALSE(subr_null_p, make_cons(scm_nil, scm_nil));
  PRED_FALSE(subr_null_p, scm_undef);
}

// ---------------------------------------------------------------------------
// number?  — R6RS §9.9.4.1
// ---------------------------------------------------------------------------

void test_number_p() {
  printf("--- number? ---\n");
  PRED_TRUE(subr_number_p, make_fixnum(0));
  PRED_TRUE(subr_number_p, make_fixnum(-1));
  PRED_TRUE(subr_number_p, make_flonum(3.14));
  PRED_TRUE(subr_number_p, make_flonum(0.0));
  PRED_TRUE(subr_number_p, make_flonum(1.0 / 0.0));  // +inf is a number
  PRED_TRUE(subr_number_p, make_flonum(0.0 / 0.0));  // nan is a number
  PRED_FALSE(subr_number_p, scm_true);
  PRED_FALSE(subr_number_p, scm_false);
  PRED_FALSE(subr_number_p, make_char('0'));
  PRED_FALSE(subr_number_p, make_string("42"));
  PRED_FALSE(subr_number_p, scm_nil);
}

// ---------------------------------------------------------------------------
// nan?  — R6RS §9.9.4.5
// ---------------------------------------------------------------------------

void test_nan_p() {
  printf("--- nan? ---\n");
  PRED_TRUE(subr_nan_p, make_flonum(0.0 / 0.0));  // +nan.0
  PRED_FALSE(subr_nan_p, make_flonum(1.5));
  PRED_FALSE(subr_nan_p, make_flonum(1.0 / 0.0));  // +inf is not nan
  PRED_FALSE(subr_nan_p, make_flonum(0.0));
  // Exact integers are not nan
  PRED_FALSE(subr_nan_p, make_fixnum(0));
  PRED_FALSE(subr_nan_p, make_fixnum(42));
}

// ---------------------------------------------------------------------------
// pair?  — R6RS §9.12
// ---------------------------------------------------------------------------

void test_pair_p() {
  printf("--- pair? ---\n");
  PRED_TRUE(subr_pair_p, make_cons(make_fixnum(1), make_fixnum(2)));
  PRED_TRUE(subr_pair_p, make_cons(scm_nil, scm_nil));
  PRED_TRUE(subr_pair_p, make_proper_list_123());  // a list head is a pair
  // '() is not a pair
  PRED_FALSE(subr_pair_p, scm_nil);
  PRED_FALSE(subr_pair_p, scm_true);
  PRED_FALSE(subr_pair_p, make_fixnum(0));
  PRED_FALSE(subr_pair_p, make_string("foo"));
  PRED_FALSE(subr_pair_p, make_vector(3, scm_nil));
}

// ---------------------------------------------------------------------------
// procedure?  — R6RS §9.7
// ---------------------------------------------------------------------------

void test_procedure_p() {
  printf("--- procedure? ---\n");
  // A closure is a procedure
  scm_obj_t cl = dummy_closure();
  PRED_TRUE(subr_procedure_p, cl);
  // Non-procedures
  PRED_FALSE(subr_procedure_p, make_fixnum(0));
  PRED_FALSE(subr_procedure_p, scm_true);
  PRED_FALSE(subr_procedure_p, scm_nil);
  PRED_FALSE(subr_procedure_p, make_string("lambda"));
  PRED_FALSE(subr_procedure_p, make_vector(1, scm_nil));
  PRED_FALSE(subr_procedure_p, make_symbol("+"));
}

// ---------------------------------------------------------------------------
// real?  — R6RS §9.9.4.1
// ---------------------------------------------------------------------------

void test_real_p() {
  printf("--- real? ---\n");
  PRED_TRUE(subr_real_p, make_fixnum(0));
  PRED_TRUE(subr_real_p, make_fixnum(-100));
  PRED_TRUE(subr_real_p, make_flonum(3.14));
  PRED_TRUE(subr_real_p, make_flonum(0.0));
  PRED_TRUE(subr_real_p, make_flonum(1.0 / 0.0));
  PRED_TRUE(subr_real_p, make_flonum(0.0 / 0.0));
  PRED_FALSE(subr_real_p, scm_true);
  PRED_FALSE(subr_real_p, make_char('1'));
  PRED_FALSE(subr_real_p, make_string("3.14"));
}

// ---------------------------------------------------------------------------
// string?  — R6RS §9.14
// ---------------------------------------------------------------------------

void test_string_p() {
  printf("--- string? ---\n");
  PRED_TRUE(subr_string_p, make_string(""));
  PRED_TRUE(subr_string_p, make_string("hello"));
  PRED_TRUE(subr_string_p, make_string("123"));
  PRED_FALSE(subr_string_p, make_symbol("hello"));
  PRED_FALSE(subr_string_p, make_char('h'));
  PRED_FALSE(subr_string_p, scm_nil);
  PRED_FALSE(subr_string_p, make_fixnum(0));
  PRED_FALSE(subr_string_p, scm_true);
}

// ---------------------------------------------------------------------------
// symbol?  — R6RS §9.13 (symbols)
// ---------------------------------------------------------------------------

void test_symbol_p() {
  printf("--- symbol? ---\n");
  PRED_TRUE(subr_symbol_p, make_symbol("foo"));
  PRED_TRUE(subr_symbol_p, make_symbol("+"));
  PRED_TRUE(subr_symbol_p, make_symbol("hello-world!"));
  PRED_FALSE(subr_symbol_p, make_string("foo"));
  PRED_FALSE(subr_symbol_p, scm_true);
  PRED_FALSE(subr_symbol_p, scm_nil);
  PRED_FALSE(subr_symbol_p, make_fixnum(0));
  PRED_FALSE(subr_symbol_p, make_char('a'));
}

// ---------------------------------------------------------------------------
// vector?  — R6RS §9.15
// ---------------------------------------------------------------------------

void test_vector_p() {
  printf("--- vector? ---\n");
  PRED_TRUE(subr_vector_p, make_vector(0, scm_nil));
  PRED_TRUE(subr_vector_p, make_vector(3, make_fixnum(0)));
  PRED_FALSE(subr_vector_p, make_cons(make_fixnum(1), scm_nil));
  PRED_FALSE(subr_vector_p, scm_nil);
  PRED_FALSE(subr_vector_p, make_fixnum(3));
  PRED_FALSE(subr_vector_p, scm_true);
  PRED_FALSE(subr_vector_p, make_string("vector"));
}

// ---------------------------------------------------------------------------
// not  — R6RS §9.10
// ---------------------------------------------------------------------------

void test_not() {
  printf("--- not ---\n");
  // Only #f is falsy; everything else is truthy
  ASSERT_TRUE(subr_not(scm_nil, scm_false) == scm_true);
  ASSERT_TRUE(subr_not(scm_nil, scm_true) == scm_false);
  ASSERT_TRUE(subr_not(scm_nil, make_fixnum(0)) == scm_false);
  ASSERT_TRUE(subr_not(scm_nil, scm_nil) == scm_false);
  ASSERT_TRUE(subr_not(scm_nil, make_string("")) == scm_false);
}

// ---------------------------------------------------------------------------
// vector  — R6RS §9.15
// ---------------------------------------------------------------------------

static scm_obj_t call_vector(std::initializer_list<scm_obj_t> args) {
  std::vector<scm_obj_t> v(args);
  return subr_vector(scm_nil, (int)v.size(), v.data());
}

void test_vector() {
  printf("--- vector / vector-length / vector-ref / vector-set! / vector->list ---\n");

  // (vector) → #()
  scm_obj_t v0 = call_vector({});
  ASSERT_TRUE(is_vector(v0));
  ASSERT_TRUE(subr_vector_length(scm_nil, v0) == make_fixnum(0));

  // (vector 10 20 30) → #(10 20 30)
  scm_obj_t v3 = call_vector({make_fixnum(10), make_fixnum(20), make_fixnum(30)});
  ASSERT_TRUE(is_vector(v3));
  ASSERT_TRUE(subr_vector_length(scm_nil, v3) == make_fixnum(3));

  // vector-ref
  ASSERT_TRUE(subr_vector_ref(scm_nil, v3, make_fixnum(0)) == make_fixnum(10));
  ASSERT_TRUE(subr_vector_ref(scm_nil, v3, make_fixnum(1)) == make_fixnum(20));
  ASSERT_TRUE(subr_vector_ref(scm_nil, v3, make_fixnum(2)) == make_fixnum(30));

  // vector-set!
  subr_vector_set(scm_nil, v3, make_fixnum(1), make_fixnum(99));
  ASSERT_TRUE(subr_vector_ref(scm_nil, v3, make_fixnum(1)) == make_fixnum(99));

  // vector->list:  #(10 99 30) → (10 99 30)
  scm_obj_t lst = subr_vector_to_list(scm_nil, v3);
  ASSERT_TRUE(is_cons(lst));
  ASSERT_TRUE(CAR(lst) == make_fixnum(10));
  ASSERT_TRUE(CAR(CDR(lst)) == make_fixnum(99));
  ASSERT_TRUE(CAR(CDR(CDR(lst))) == make_fixnum(30));
  ASSERT_TRUE(CDR(CDR(CDR(lst))) == scm_nil);

  // vector->list of empty vector → '()
  ASSERT_TRUE(subr_vector_to_list(scm_nil, v0) == scm_nil);
}

// ---------------------------------------------------------------------------
// string-length  — R6RS §9.14
// ---------------------------------------------------------------------------

void test_string_length() {
  printf("--- string-length ---\n");
  ASSERT_TRUE(subr_string_length(scm_nil, make_string("")) == make_fixnum(0));
  ASSERT_TRUE(subr_string_length(scm_nil, make_string("hello")) == make_fixnum(5));
  ASSERT_TRUE(subr_string_length(scm_nil, make_string("abc")) == make_fixnum(3));
}

// ---------------------------------------------------------------------------
// string-ref  — R6RS §9.14
// ---------------------------------------------------------------------------

void test_string_ref() {
  printf("--- string-ref ---\n");
  scm_obj_t s = make_string("hello");
  ASSERT_TRUE(subr_string_ref(scm_nil, s, make_fixnum(0)) == make_char('h'));
  ASSERT_TRUE(subr_string_ref(scm_nil, s, make_fixnum(4)) == make_char('o'));
  // Each ASCII char is its own byte, so index == byte offset
  ASSERT_TRUE(subr_string_ref(scm_nil, s, make_fixnum(1)) == make_char('e'));
}

// ---------------------------------------------------------------------------
// string=?  — R6RS §9.14
// ---------------------------------------------------------------------------

void test_string_eq() {
  printf("--- string=? ---\n");
  PRED2_TRUE(subr_string_eq, make_string(""), make_string(""));
  PRED2_TRUE(subr_string_eq, make_string("abc"), make_string("abc"));
  PRED2_FALSE(subr_string_eq, make_string("abc"), make_string("ABC"));
  PRED2_FALSE(subr_string_eq, make_string("hello"), make_string("world"));
  PRED2_FALSE(subr_string_eq, make_string("a"), make_string("ab"));
}

// ---------------------------------------------------------------------------
// string-append  — R6RS §9.14
// ---------------------------------------------------------------------------

void test_string_append() {
  printf("--- string-append ---\n");
  // (string-append) → ""
  {
    scm_obj_t result = subr_string_append(scm_nil, 0, nullptr);
    ASSERT_TRUE(is_string(result));
    ASSERT_TRUE(strcmp((const char*)string_name(result), "") == 0);
  }
  // (string-append "foo" "bar") → "foobar"
  {
    scm_obj_t args[] = {make_string("foo"), make_string("bar")};
    scm_obj_t result = subr_string_append(scm_nil, 2, args);
    ASSERT_TRUE(strcmp((const char*)string_name(result), "foobar") == 0);
  }
  // (string-append "a" "b" "c") → "abc"
  {
    scm_obj_t args[] = {make_string("a"), make_string("b"), make_string("c")};
    scm_obj_t result = subr_string_append(scm_nil, 3, args);
    ASSERT_TRUE(strcmp((const char*)string_name(result), "abc") == 0);
  }
}

// ---------------------------------------------------------------------------
// substring  — R6RS §9.14
// ---------------------------------------------------------------------------

void test_substring() {
  printf("--- substring ---\n");
  scm_obj_t s = make_string("hello world");
  // (substring "hello world" 0 5) → "hello"
  scm_obj_t r1 = subr_substring(scm_nil, s, make_fixnum(0), make_fixnum(5));
  ASSERT_TRUE(strcmp((const char*)string_name(r1), "hello") == 0);
  // (substring "hello world" 6 11) → "world"
  scm_obj_t r2 = subr_substring(scm_nil, s, make_fixnum(6), make_fixnum(11));
  ASSERT_TRUE(strcmp((const char*)string_name(r2), "world") == 0);
  // (substring s 0 0) → ""
  scm_obj_t r3 = subr_substring(scm_nil, s, make_fixnum(0), make_fixnum(0));
  ASSERT_TRUE(strcmp((const char*)string_name(r3), "") == 0);
  // (substring s 0 11) → whole string
  scm_obj_t r4 = subr_substring(scm_nil, s, make_fixnum(0), make_fixnum(11));
  ASSERT_TRUE(strcmp((const char*)string_name(r4), "hello world") == 0);
}

// ---------------------------------------------------------------------------
// symbol->string / string->symbol  — R6RS §9.13
// ---------------------------------------------------------------------------

void test_symbol_string_conv() {
  printf("--- symbol->string / string->symbol ---\n");
  // symbol->string
  scm_obj_t sym = make_symbol("hello");
  scm_obj_t str = subr_symbol_to_string(scm_nil, sym);
  ASSERT_TRUE(is_string(str));
  ASSERT_TRUE(strcmp((const char*)string_name(str), "hello") == 0);
  // The result is a fresh string object, not eq? to the symbol
  ASSERT_FALSE(str == sym);

  // string->symbol (interned)
  scm_obj_t s2 = make_string("world");
  scm_obj_t sym2a = subr_string_to_symbol(scm_nil, s2);
  scm_obj_t sym2b = subr_string_to_symbol(scm_nil, make_string("world"));
  ASSERT_TRUE(is_symbol(sym2a));
  ASSERT_TRUE(sym2a == sym2b);  // interned: same object
  ASSERT_TRUE(strcmp((const char*)symbol_name(sym2a), "world") == 0);
}

// ---------------------------------------------------------------------------
// number->string  — R6RS §9.9.4.4
// ---------------------------------------------------------------------------

void test_number_to_string() {
  printf("--- number->string ---\n");
  auto n2s = [](scm_obj_t n) -> const char* {
    scm_obj_t args[] = {n};
    return (const char*)string_name(subr_number_to_string(scm_nil, 1, args));
  };
  auto n2s_radix = [](scm_obj_t n, int r) -> const char* {
    scm_obj_t args[] = {n, make_fixnum(r)};
    return (const char*)string_name(subr_number_to_string(scm_nil, 2, args));
  };

  // Fixnum base 10
  ASSERT_TRUE(strcmp(n2s(make_fixnum(0)), "0") == 0);
  ASSERT_TRUE(strcmp(n2s(make_fixnum(42)), "42") == 0);
  ASSERT_TRUE(strcmp(n2s(make_fixnum(-7)), "-7") == 0);
  ASSERT_TRUE(strcmp(n2s(make_fixnum(255)), "255") == 0);

  // Fixnum other bases
  ASSERT_TRUE(strcmp(n2s_radix(make_fixnum(255), 16), "ff") == 0);
  ASSERT_TRUE(strcmp(n2s_radix(make_fixnum(8), 8), "10") == 0);
  ASSERT_TRUE(strcmp(n2s_radix(make_fixnum(10), 2), "1010") == 0);
  ASSERT_TRUE(strcmp(n2s_radix(make_fixnum(0), 2), "0") == 0);

  // Flonum base 10
  ASSERT_TRUE(strcmp(n2s(make_flonum(1.0 / 0.0)), "+inf.0") == 0);
  ASSERT_TRUE(strcmp(n2s(make_flonum(-1.0 / 0.0)), "-inf.0") == 0);
  ASSERT_TRUE(strcmp(n2s(make_flonum(0.0 / 0.0)), "+nan.0") == 0);
  // 1.5 must round-trip
  const char* s15 = n2s(make_flonum(1.5));
  ASSERT_TRUE(s15[0] != '\0');  // non-empty
  ASSERT_TRUE(strchr(s15, '.') != nullptr || strchr(s15, 'e') != nullptr);
}

// ---------------------------------------------------------------------------
// string->number  — R6RS §9.9.4.4
// ---------------------------------------------------------------------------

void test_string_to_number() {
  printf("--- string->number ---\n");
  auto s2n = [](const char* s) -> scm_obj_t {
    scm_obj_t args[] = {make_string(s)};
    return subr_string_to_number(scm_nil, 1, args);
  };
  auto s2n_radix = [](const char* s, int r) -> scm_obj_t {
    scm_obj_t args[] = {make_string(s), make_fixnum(r)};
    return subr_string_to_number(scm_nil, 2, args);
  };

  // Valid integers
  ASSERT_TRUE(s2n("0") == make_fixnum(0));
  ASSERT_TRUE(s2n("42") == make_fixnum(42));
  ASSERT_TRUE(s2n("-7") == make_fixnum(-7));
  ASSERT_TRUE(s2n("255") == make_fixnum(255));

  // Valid flonum
  scm_obj_t f15 = s2n("1.5");
  ASSERT_TRUE(is_short_flonum(f15) || is_long_flonum(f15));
  ASSERT_TRUE(flonum(f15) == 1.5);

  // Radixes
  ASSERT_TRUE(s2n_radix("ff", 16) == make_fixnum(255));
  ASSERT_TRUE(s2n_radix("10", 8) == make_fixnum(8));
  ASSERT_TRUE(s2n_radix("1010", 2) == make_fixnum(10));

  // Invalid → #f
  ASSERT_TRUE(s2n("") == scm_false);
  ASSERT_TRUE(s2n("abc") == scm_false);
  ASSERT_TRUE(s2n("1.2.3") == scm_false);
}

// ---------------------------------------------------------------------------
// char=?  — R6RS §9.13
// ---------------------------------------------------------------------------

void test_char_eq() {
  printf("--- char=? ---\n");
  PRED2_TRUE(subr_char_eq, make_char('a'), make_char('a'));
  PRED2_TRUE(subr_char_eq, make_char(0), make_char(0));
  PRED2_TRUE(subr_char_eq, make_char(0x1F600), make_char(0x1F600));
  PRED2_FALSE(subr_char_eq, make_char('a'), make_char('b'));
  PRED2_FALSE(subr_char_eq, make_char('A'), make_char('a'));
}

// ---------------------------------------------------------------------------
// char-numeric?  — R6RS §9.13
// ---------------------------------------------------------------------------

void test_char_numeric_p() {
  printf("--- char-numeric? ---\n");
  for (char c = '0'; c <= '9'; c++) PRED_TRUE(subr_char_numeric_p, make_char(c));
  PRED_FALSE(subr_char_numeric_p, make_char('a'));
  PRED_FALSE(subr_char_numeric_p, make_char('Z'));
  PRED_FALSE(subr_char_numeric_p, make_char(' '));
  PRED_FALSE(subr_char_numeric_p, make_char('/'));  // '/' is one before '0'
  PRED_FALSE(subr_char_numeric_p, make_char(':'));  // ':' is one after '9'
}

// ---------------------------------------------------------------------------
// max  — R6RS §9.9.3.2
// ---------------------------------------------------------------------------

void test_max() {
  printf("--- max ---\n");
  auto max1 = [](scm_obj_t a) {
    scm_obj_t args[] = {a};
    return subr_max(scm_nil, 1, args);
  };
  auto max2 = [](scm_obj_t a, scm_obj_t b) {
    scm_obj_t args[] = {a, b};
    return subr_max(scm_nil, 2, args);
  };
  auto max3 = [](scm_obj_t a, scm_obj_t b, scm_obj_t c) {
    scm_obj_t args[] = {a, b, c};
    return subr_max(scm_nil, 3, args);
  };

  // Single arg: identity
  ASSERT_TRUE(max1(make_fixnum(5)) == make_fixnum(5));

  // All exact
  ASSERT_TRUE(max2(make_fixnum(3), make_fixnum(7)) == make_fixnum(7));
  ASSERT_TRUE(max2(make_fixnum(7), make_fixnum(3)) == make_fixnum(7));
  ASSERT_TRUE(max2(make_fixnum(-1), make_fixnum(0)) == make_fixnum(0));
  ASSERT_TRUE(max3(make_fixnum(1), make_fixnum(3), make_fixnum(2)) == make_fixnum(3));

  // All inexact
  scm_obj_t r_ff = max2(make_flonum(1.5), make_flonum(2.5));
  ASSERT_TRUE(is_short_flonum(r_ff) || is_long_flonum(r_ff));
  ASSERT_TRUE(flonum(r_ff) == 2.5);

  // Mixed: result promoted to inexact (R6RS §9.9.3.2)
  scm_obj_t r_mix = max2(make_fixnum(5), make_flonum(3.0));
  ASSERT_TRUE(is_short_flonum(r_mix) || is_long_flonum(r_mix));
  ASSERT_TRUE(flonum(r_mix) == 5.0);  // 5 wins but becomes 5.0

  // Negative values
  ASSERT_TRUE(max2(make_fixnum(-3), make_fixnum(-1)) == make_fixnum(-1));
}

// ---------------------------------------------------------------------------
// pairs & lists (extra)
// ---------------------------------------------------------------------------

void test_pairs_lists_extra() {
  printf("--- pairs & lists extra ---\n");

  scm_obj_t lst = make_proper_list_123();  // (1 2 3)

  // cddr
  scm_obj_t cd2ar = subr_cddr(scm_nil, lst);
  ASSERT_TRUE(is_cons(cd2ar) && CAR(cd2ar) == make_fixnum(3) && CDR(cd2ar) == scm_nil);

  // length
  ASSERT_TRUE(subr_length(scm_nil, lst) == make_fixnum(3));
  ASSERT_TRUE(subr_length(scm_nil, scm_nil) == make_fixnum(0));

  // list-ref
  ASSERT_TRUE(subr_list_ref(scm_nil, lst, make_fixnum(0)) == make_fixnum(1));
  ASSERT_TRUE(subr_list_ref(scm_nil, lst, make_fixnum(2)) == make_fixnum(3));

  // memq / memv / member
  scm_obj_t mem_lst = make_cons(make_symbol("a"), make_cons(make_fixnum(2), make_cons(make_string("c"), scm_nil)));

  scm_obj_t mq = subr_memq(scm_nil, make_symbol("a"), mem_lst);
  ASSERT_TRUE(mq == mem_lst);  // found at head

  scm_obj_t mv = subr_memv(scm_nil, make_fixnum(2), mem_lst);
  ASSERT_TRUE(CAR(mv) == make_fixnum(2));

  // string is not eq/eqv
  scm_obj_t ms_q = subr_memv(scm_nil, make_string("c"), mem_lst);  // false since eqv on strings is false unless same ptr
  ASSERT_TRUE(ms_q == scm_false);

  scm_obj_t ms_m = subr_member(scm_nil, make_string("c"), mem_lst);  // true via equal?
  ASSERT_TRUE(ms_m != scm_false);
  ASSERT_TRUE(CAR(ms_m) != scm_false);

  // assq / assoc
  scm_obj_t alist = make_alist();
  scm_obj_t aq = subr_assq(scm_nil, make_symbol("b"), alist);
  ASSERT_TRUE(is_cons(aq));
  ASSERT_TRUE(CDR(aq) == make_fixnum(2));

  scm_obj_t ac = subr_assoc(scm_nil, make_string("c"), alist);
  ASSERT_TRUE(is_cons(ac));
  ASSERT_TRUE(CDR(ac) == make_fixnum(3));

  // reverse
  scm_obj_t rev = subr_reverse(scm_nil, lst);
  ASSERT_TRUE(CAR(rev) == make_fixnum(3));
  ASSERT_TRUE(CAR(CDR(rev)) == make_fixnum(2));
  ASSERT_TRUE(CAR(CDR(CDR(rev))) == make_fixnum(1));

  // list->vector
  scm_obj_t l2v = subr_list_to_vector(scm_nil, lst);
  ASSERT_TRUE(is_vector(l2v));
  ASSERT_TRUE(vector_nsize(l2v) == 3);
  ASSERT_TRUE(vector_elts(l2v)[0] == make_fixnum(1));
  ASSERT_TRUE(vector_elts(l2v)[2] == make_fixnum(3));

  // set-car!
  scm_obj_t p = make_cons(make_fixnum(10), make_fixnum(20));
  subr_set_car(scm_nil, p, make_fixnum(99));
  ASSERT_TRUE(CAR(p) == make_fixnum(99));

  // list
  {
    scm_obj_t args[] = {make_fixnum(1), make_fixnum(2), make_fixnum(3)};
    scm_obj_t l = subr_list(scm_nil, 3, args);
    ASSERT_TRUE(is_cons(l) && CAR(l) == make_fixnum(1));
    ASSERT_TRUE(is_cons(CDR(l)) && CAR(CDR(l)) == make_fixnum(2));
    ASSERT_TRUE(is_cons(CDR(CDR(l))) && CAR(CDR(CDR(l))) == make_fixnum(3));
    ASSERT_TRUE(CDR(CDR(CDR(l))) == scm_nil);
  }

  // list-transpose
  {
    // ((1 2) (3 4)) -> ((1 3) (2 4))
    scm_obj_t l1 = make_cons(make_fixnum(1), make_cons(make_fixnum(2), scm_nil));
    scm_obj_t l2 = make_cons(make_fixnum(3), make_cons(make_fixnum(4), scm_nil));
    scm_obj_t args[] = {l1, l2};
    scm_obj_t res = subr_list_transpose(scm_nil, 2, args);
    // res should be ((1 3) (2 4))
    scm_obj_t r1 = CAR(res);
    scm_obj_t r2 = CAR(CDR(res));
    ASSERT_TRUE(CAR(r1) == make_fixnum(1) && CAR(CDR(r1)) == make_fixnum(3));
    ASSERT_TRUE(CAR(r2) == make_fixnum(2) && CAR(CDR(r2)) == make_fixnum(4));
  }

  // list-transpose+
  {
    scm_obj_t l1 = make_cons(make_fixnum(1), make_cons(make_fixnum(2), scm_nil));
    scm_obj_t l2 = make_cons(make_fixnum(3), scm_nil);  // different length
    scm_obj_t args[] = {l1, l2};
    ASSERT_TRUE(subr_list_transpose_plus(scm_nil, 2, args) == scm_false);
  }

  // cons*
  {
    // (cons* 1 2 3) -> (1 2 . 3)
    scm_obj_t args[] = {make_fixnum(1), make_fixnum(2), make_fixnum(3)};
    scm_obj_t res = subr_cons_ast(scm_nil, 3, args);
    ASSERT_TRUE(CAR(res) == make_fixnum(1));
    ASSERT_TRUE(CAR(CDR(res)) == make_fixnum(2));
    ASSERT_TRUE(CDR(CDR(res)) == make_fixnum(3));

    // (cons* 1) -> 1
    scm_obj_t args1[] = {make_fixnum(1)};
    ASSERT_TRUE(subr_cons_ast(scm_nil, 1, args1) == make_fixnum(1));
  }
}

// ---------------------------------------------------------------------------
// hashtable? / equal-hash / make-eq-hashtable / make-eqv-hashtable  — R6RS §12
// ---------------------------------------------------------------------------

void test_hashtable_subrs() {
  printf("--- hashtable? / equal-hash / make-eq-hashtable / make-eqv-hashtable ---\n");

  // hashtable?
  scm_obj_t args0[] = {};
  scm_obj_t ht_eq = subr_make_eq_hashtable(scm_nil, 0, args0);
  ASSERT_TRUE(is_hashtable(ht_eq));
  PRED_TRUE(subr_hashtable_p, ht_eq);
  PRED_FALSE(subr_hashtable_p, make_fixnum(0));
  PRED_FALSE(subr_hashtable_p, scm_nil);
  PRED_FALSE(subr_hashtable_p, make_vector(2, scm_nil));

  // make-eq-hashtable without capacity
  ASSERT_TRUE(is_hashtable(ht_eq));

  // make-eq-hashtable with explicit capacity
  scm_obj_t args_cap[] = {make_fixnum(64)};
  scm_obj_t ht_eq2 = subr_make_eq_hashtable(scm_nil, 1, args_cap);
  ASSERT_TRUE(is_hashtable(ht_eq2));

  // make-eqv-hashtable without capacity
  scm_obj_t ht_eqv = subr_make_eqv_hashtable(scm_nil, 0, args0);
  ASSERT_TRUE(is_hashtable(ht_eqv));

  // make-eqv-hashtable with explicit capacity
  scm_obj_t ht_eqv2 = subr_make_eqv_hashtable(scm_nil, 1, args_cap);
  ASSERT_TRUE(is_hashtable(ht_eqv2));

  // make-equal-hashtable without capacity
  scm_obj_t ht_equal = subr_make_equal_hashtable(scm_nil, 0, args0);
  ASSERT_TRUE(is_hashtable(ht_equal));

  // make-equal-hashtable with explicit capacity
  scm_obj_t ht_equal2 = subr_make_equal_hashtable(scm_nil, 1, args_cap);
  ASSERT_TRUE(is_hashtable(ht_equal2));

  // equal-hash: same structural value → same hash
  scm_obj_t h1 = subr_equal_hash(scm_nil, make_fixnum(42));
  scm_obj_t h2 = subr_equal_hash(scm_nil, make_fixnum(42));
  ASSERT_TRUE(is_fixnum(h1));
  ASSERT_TRUE(h1 == h2);

  // equal-hash on strings: equal content → same hash
  scm_obj_t hs1 = subr_equal_hash(scm_nil, make_string("hello"));
  scm_obj_t hs2 = subr_equal_hash(scm_nil, make_string("hello"));
  ASSERT_TRUE(is_fixnum(hs1));
  ASSERT_TRUE(hs1 == hs2);

  // equal-hash on lists: equal structure → same hash
  scm_obj_t hl1 = subr_equal_hash(scm_nil, make_proper_list_123());
  scm_obj_t hl2 = subr_equal_hash(scm_nil, make_proper_list_123());
  ASSERT_TRUE(hl1 == hl2);

  // equal-hash result is a non-negative fixnum
  ASSERT_TRUE(fixnum(h1) >= 0);
  ASSERT_TRUE(fixnum(hs1) >= 0);
  ASSERT_TRUE(fixnum(hl1) >= 0);
}

// ---------------------------------------------------------------------------
// hashtable-ref / hashtable-set! / hashtable-delete! /
// hashtable-contains? / hashtable-clear!  — R6RS §12.3
// ---------------------------------------------------------------------------

// Helper: make an eq-hashtable with no capacity hint
static scm_obj_t make_eq_ht() {
  scm_obj_t args0[] = {};
  return subr_make_eq_hashtable(scm_nil, 0, args0);
}

// Helper: call hashtable-ref with 2 args (no default; raises on miss)
static scm_obj_t ht_ref2(scm_obj_t ht, scm_obj_t key) {
  scm_obj_t args[] = {ht, key};
  return subr_hashtable_ref(scm_nil, 2, args);
}

// Helper: call hashtable-ref with 3 args (explicit default)
static scm_obj_t ht_ref3(scm_obj_t ht, scm_obj_t key, scm_obj_t def) {
  scm_obj_t args[] = {ht, key, def};
  return subr_hashtable_ref(scm_nil, 3, args);
}

void test_hashtable_ops() {
  printf("--- hashtable-set! / hashtable-ref / hashtable-contains? / hashtable-delete! / hashtable-clear! ---\n");

  // ----------------------------------------------------------------
  // hashtable-set! + hashtable-ref (3-arg, with default)
  // ----------------------------------------------------------------
  {
    scm_obj_t ht = make_eq_ht();
    scm_obj_t k = make_symbol("x");
    scm_obj_t v = make_fixnum(42);

    // Key absent: returns default
    ASSERT_TRUE(ht_ref3(ht, k, scm_false) == scm_false);

    // After set!, ref returns the stored value
    ASSERT_TRUE(subr_hashtable_set(scm_nil, ht, k, v) == scm_unspecified);
    ASSERT_TRUE(ht_ref3(ht, k, scm_false) == v);

    // Overwrite: new value replaces old
    scm_obj_t v2 = make_fixnum(99);
    subr_hashtable_set(scm_nil, ht, k, v2);
    ASSERT_TRUE(ht_ref3(ht, k, scm_false) == v2);

    // Different key still absent
    scm_obj_t k2 = make_symbol("y");
    ASSERT_TRUE(ht_ref3(ht, k2, make_fixnum(0)) == make_fixnum(0));
  }

  // ----------------------------------------------------------------
  // hashtable-ref (2-arg): raises on missing key
  // ----------------------------------------------------------------
  {
    scm_obj_t ht = make_eq_ht();
    scm_obj_t k = make_symbol("missing");
    bool threw = false;
    try {
      ht_ref2(ht, k);
    } catch (const std::runtime_error&) {
      threw = true;
    }
    ASSERT_TRUE(threw);

    // After insertion, 2-arg ref succeeds
    scm_obj_t v = make_fixnum(7);
    subr_hashtable_set(scm_nil, ht, k, v);
    ASSERT_TRUE(ht_ref2(ht, k) == v);
  }

  // ----------------------------------------------------------------
  // hashtable-contains?
  // ----------------------------------------------------------------
  {
    scm_obj_t ht = make_eq_ht();
    scm_obj_t k = make_symbol("present");

    // Not present initially
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, k) == scm_false);

    // Present after set!
    subr_hashtable_set(scm_nil, ht, k, make_fixnum(1));
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, k) == scm_true);

    // Different key remains absent
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, make_symbol("absent")) == scm_false);
  }

  // ----------------------------------------------------------------
  // hashtable-delete!
  // ----------------------------------------------------------------
  {
    scm_obj_t ht = make_eq_ht();
    scm_obj_t k = make_symbol("del");
    scm_obj_t v = make_fixnum(55);

    // Delete absent key: no-op, returns unspecified
    ASSERT_TRUE(subr_hashtable_delete(scm_nil, ht, k) == scm_unspecified);

    // Insert then delete
    subr_hashtable_set(scm_nil, ht, k, v);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, k) == scm_true);
    ASSERT_TRUE(subr_hashtable_delete(scm_nil, ht, k) == scm_unspecified);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, k) == scm_false);
    ASSERT_TRUE(ht_ref3(ht, k, scm_false) == scm_false);

    // Re-inserting after delete works correctly
    subr_hashtable_set(scm_nil, ht, k, make_fixnum(77));
    ASSERT_TRUE(ht_ref3(ht, k, scm_false) == make_fixnum(77));
  }

  // ----------------------------------------------------------------
  // hashtable-clear!
  // ----------------------------------------------------------------
  {
    scm_obj_t ht = make_eq_ht();
    scm_obj_t ka = make_symbol("a");
    scm_obj_t kb = make_symbol("b");
    scm_obj_t kc = make_symbol("c");

    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(1));
    subr_hashtable_set(scm_nil, ht, kb, make_fixnum(2));
    subr_hashtable_set(scm_nil, ht, kc, make_fixnum(3));
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, ka) == scm_true);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, kb) == scm_true);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, kc) == scm_true);

    // Clear returns unspecified
    ASSERT_TRUE(subr_hashtable_clear(scm_nil, ht) == scm_unspecified);

    // All keys are gone
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, ka) == scm_false);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, kb) == scm_false);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, kc) == scm_false);
    ASSERT_TRUE(ht_ref3(ht, ka, scm_false) == scm_false);

    // Hashtable is still usable after clear
    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(10));
    ASSERT_TRUE(ht_ref3(ht, ka, scm_false) == make_fixnum(10));
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht, kb) == scm_false);
  }

  // ----------------------------------------------------------------
  // Multiple hashtable types: eqv-hashtable (fixnum keys), equal-hashtable (string keys)
  // ----------------------------------------------------------------
  {
    // eqv-hashtable with fixnum keys
    scm_obj_t args0[] = {};
    scm_obj_t ht_eqv = subr_make_eqv_hashtable(scm_nil, 0, args0);
    scm_obj_t ki = make_fixnum(123);
    subr_hashtable_set(scm_nil, ht_eqv, ki, make_fixnum(456));
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht_eqv, make_fixnum(123)) == scm_true);
    ASSERT_TRUE(ht_ref3(ht_eqv, make_fixnum(123), scm_false) == make_fixnum(456));
    ASSERT_TRUE(ht_ref3(ht_eqv, make_fixnum(124), scm_false) == scm_false);
    subr_hashtable_delete(scm_nil, ht_eqv, ki);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht_eqv, ki) == scm_false);

    // equal-hashtable with string keys (content-based lookup)
    scm_obj_t ht_eq = subr_make_equal_hashtable(scm_nil, 0, args0);
    scm_obj_t ks1 = make_string("hello");
    scm_obj_t ks2 = make_string("hello");  // same content, different object
    subr_hashtable_set(scm_nil, ht_eq, ks1, make_fixnum(99));
    // equal-hashtable uses equal? so ks2 (same content) should find the entry
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht_eq, ks2) == scm_true);
    ASSERT_TRUE(ht_ref3(ht_eq, ks2, scm_false) == make_fixnum(99));
    subr_hashtable_clear(scm_nil, ht_eq);
    ASSERT_TRUE(subr_hashtable_contains(scm_nil, ht_eq, ks1) == scm_false);
  }

  // ----------------------------------------------------------------
  // Error handling: type checks
  // ----------------------------------------------------------------
  {
    auto throws = [](auto fn) {
      bool threw = false;
      try {
        fn();
      } catch (const std::runtime_error&) {
        threw = true;
      }
      return threw;
    };
    scm_obj_t not_ht = make_fixnum(0);
    scm_obj_t dummy_k = make_fixnum(1);
    scm_obj_t dummy_v = make_fixnum(2);
    scm_obj_t ht = make_eq_ht();

    // non-hashtable first arg
    ASSERT_TRUE(throws([&] { subr_hashtable_set(scm_nil, not_ht, dummy_k, dummy_v); }));
    ASSERT_TRUE(throws([&] {
      scm_obj_t a[] = {not_ht, dummy_k};
      subr_hashtable_ref(scm_nil, 2, a);
    }));
    ASSERT_TRUE(throws([&] { subr_hashtable_delete(scm_nil, not_ht, dummy_k); }));
    ASSERT_TRUE(throws([&] { subr_hashtable_contains(scm_nil, not_ht, dummy_k); }));
    ASSERT_TRUE(throws([&] { subr_hashtable_clear(scm_nil, not_ht); }));
    (void)ht;
  }
}

// ---------------------------------------------------------------------------
// values / hashtable-entries  — R6RS §11.14, §12.3
// ---------------------------------------------------------------------------

void test_values_and_entries() {
  printf("--- values / hashtable-entries ---\n");

  // ----------------------------------------------------------------
  // values: single value is returned unwrapped (identity)
  // ----------------------------------------------------------------
  {
    scm_obj_t args1[] = {make_fixnum(42)};
    scm_obj_t v1 = subr_values(scm_nil, 1, args1);
    ASSERT_TRUE(v1 == make_fixnum(42));  // single value: passthrough
    ASSERT_TRUE(!is_values(v1));         // not wrapped in a values object
  }

  // ----------------------------------------------------------------
  // values: zero args → wrapped empty values object
  // ----------------------------------------------------------------
  {
    scm_obj_t v0 = subr_values(scm_nil, 0, nullptr);
    ASSERT_TRUE(is_values(v0));
    ASSERT_TRUE(values_nsize(v0) == 0);
  }

  // ----------------------------------------------------------------
  // values: two args → wrapped values object with correct elements
  // ----------------------------------------------------------------
  {
    scm_obj_t args2[] = {make_fixnum(10), make_fixnum(20)};
    scm_obj_t v2 = subr_values(scm_nil, 2, args2);
    ASSERT_TRUE(is_values(v2));
    ASSERT_TRUE(values_nsize(v2) == 2);
    ASSERT_TRUE(values_elts(v2)[0] == make_fixnum(10));
    ASSERT_TRUE(values_elts(v2)[1] == make_fixnum(20));
  }

  // ----------------------------------------------------------------
  // values: three args
  // ----------------------------------------------------------------
  {
    scm_obj_t args3[] = {make_symbol("a"), make_fixnum(99), scm_true};
    scm_obj_t v3 = subr_values(scm_nil, 3, args3);
    ASSERT_TRUE(is_values(v3));
    ASSERT_TRUE(values_nsize(v3) == 3);
    ASSERT_TRUE(values_elts(v3)[0] == make_symbol("a"));
    ASSERT_TRUE(values_elts(v3)[1] == make_fixnum(99));
    ASSERT_TRUE(values_elts(v3)[2] == scm_true);
  }

  // ----------------------------------------------------------------
  // hashtable-entries: empty hashtable
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t entries = subr_hashtable_entries(scm_nil, ht);
    ASSERT_TRUE(is_values(entries));
    ASSERT_TRUE(values_nsize(entries) == 2);
    scm_obj_t keys_v = values_elts(entries)[0];
    scm_obj_t vals_v = values_elts(entries)[1];
    ASSERT_TRUE(is_vector(keys_v));
    ASSERT_TRUE(is_vector(vals_v));
    ASSERT_TRUE(vector_nsize(keys_v) == 0);
    ASSERT_TRUE(vector_nsize(vals_v) == 0);
  }

  // ----------------------------------------------------------------
  // hashtable-entries: three entries — verify keys and values appear
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t ka = make_symbol("alpha");
    scm_obj_t kb = make_symbol("beta");
    scm_obj_t kc = make_symbol("gamma");
    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(1));
    subr_hashtable_set(scm_nil, ht, kb, make_fixnum(2));
    subr_hashtable_set(scm_nil, ht, kc, make_fixnum(3));

    scm_obj_t entries = subr_hashtable_entries(scm_nil, ht);
    ASSERT_TRUE(is_values(entries));
    scm_obj_t keys_v = values_elts(entries)[0];
    scm_obj_t vals_v = values_elts(entries)[1];
    ASSERT_TRUE(is_vector(keys_v));
    ASSERT_TRUE(is_vector(vals_v));
    ASSERT_TRUE(vector_nsize(keys_v) == 3);
    ASSERT_TRUE(vector_nsize(vals_v) == 3);

    // Verify each key has its value in the parallel position
    bool found_a = false, found_b = false, found_c = false;
    for (int i = 0; i < 3; i++) {
      scm_obj_t k = vector_elts(keys_v)[i];
      scm_obj_t v = vector_elts(vals_v)[i];
      if (k == ka) {
        ASSERT_TRUE(v == make_fixnum(1));
        found_a = true;
      }
      if (k == kb) {
        ASSERT_TRUE(v == make_fixnum(2));
        found_b = true;
      }
      if (k == kc) {
        ASSERT_TRUE(v == make_fixnum(3));
        found_c = true;
      }
    }
    ASSERT_TRUE(found_a);
    ASSERT_TRUE(found_b);
    ASSERT_TRUE(found_c);
  }

  // ----------------------------------------------------------------
  // hashtable-entries: after delete, deleted key absent from vectors
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t ka = make_symbol("x");
    scm_obj_t kb = make_symbol("y");
    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(10));
    subr_hashtable_set(scm_nil, ht, kb, make_fixnum(20));
    subr_hashtable_delete(scm_nil, ht, ka);

    scm_obj_t entries = subr_hashtable_entries(scm_nil, ht);
    scm_obj_t keys_v = values_elts(entries)[0];
    scm_obj_t vals_v = values_elts(entries)[1];
    ASSERT_TRUE(vector_nsize(keys_v) == 1);
    ASSERT_TRUE(vector_nsize(vals_v) == 1);
    ASSERT_TRUE(vector_elts(keys_v)[0] == kb);
    ASSERT_TRUE(vector_elts(vals_v)[0] == make_fixnum(20));
  }

  // ----------------------------------------------------------------
  // hashtable-entries: after clear, both vectors are empty
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    subr_hashtable_set(scm_nil, ht, make_symbol("p"), make_fixnum(1));
    subr_hashtable_set(scm_nil, ht, make_symbol("q"), make_fixnum(2));
    subr_hashtable_clear(scm_nil, ht);

    scm_obj_t entries = subr_hashtable_entries(scm_nil, ht);
    ASSERT_TRUE(vector_nsize(values_elts(entries)[0]) == 0);
    ASSERT_TRUE(vector_nsize(values_elts(entries)[1]) == 0);
  }

  // ----------------------------------------------------------------
  // hashtable-entries: type check
  // ----------------------------------------------------------------
  {
    bool threw = false;
    try {
      subr_hashtable_entries(scm_nil, make_fixnum(0));
    } catch (const std::runtime_error&) {
      threw = true;
    }
    ASSERT_TRUE(threw);
  }
}

// ---------------------------------------------------------------------------
// hashtable-alist  - digamma extension
// ---------------------------------------------------------------------------

void test_hashtable_alist() {
  printf("--- hashtable-alist ---\n");

  // ----------------------------------------------------------------
  // empty hashtable -> '()
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t alist = subr_hashtable_alist(scm_nil, ht);
    ASSERT_TRUE(alist == scm_nil);
  }

  // ----------------------------------------------------------------
  // three entries -> proper alist with correct pairs
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t ka = make_symbol("alpha");
    scm_obj_t kb = make_symbol("beta");
    scm_obj_t kc = make_symbol("gamma");
    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(1));
    subr_hashtable_set(scm_nil, ht, kb, make_fixnum(2));
    subr_hashtable_set(scm_nil, ht, kc, make_fixnum(3));

    scm_obj_t alist = subr_hashtable_alist(scm_nil, ht);

    // Must be a proper list of length 3
    int count = 0;
    bool found_a = false, found_b = false, found_c = false;
    scm_obj_t cur = alist;
    while (is_cons(cur)) {
      scm_obj_t p = CAR(cur);
      ASSERT_TRUE(is_cons(p));
      scm_obj_t k = CAR(p);
      scm_obj_t v = CDR(p);
      if (k == ka) {
        ASSERT_TRUE(v == make_fixnum(1));
        found_a = true;
      }
      if (k == kb) {
        ASSERT_TRUE(v == make_fixnum(2));
        found_b = true;
      }
      if (k == kc) {
        ASSERT_TRUE(v == make_fixnum(3));
        found_c = true;
      }
      count++;
      cur = CDR(cur);
    }
    ASSERT_TRUE(cur == scm_nil);  // proper list
    ASSERT_TRUE(count == 3);
    ASSERT_TRUE(found_a);
    ASSERT_TRUE(found_b);
    ASSERT_TRUE(found_c);
  }

  // ----------------------------------------------------------------
  // after delete, deleted key absent from alist
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t ka = make_symbol("x");
    scm_obj_t kb = make_symbol("y");
    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(10));
    subr_hashtable_set(scm_nil, ht, kb, make_fixnum(20));
    subr_hashtable_delete(scm_nil, ht, ka);

    scm_obj_t alist = subr_hashtable_alist(scm_nil, ht);
    ASSERT_TRUE(is_cons(alist));
    ASSERT_TRUE(CDR(alist) == scm_nil);  // exactly one entry
    scm_obj_t p = CAR(alist);
    ASSERT_TRUE(CAR(p) == kb);
    ASSERT_TRUE(CDR(p) == make_fixnum(20));
  }

  // ----------------------------------------------------------------
  // after clear, alist is empty
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    subr_hashtable_set(scm_nil, ht, make_symbol("p"), make_fixnum(1));
    subr_hashtable_clear(scm_nil, ht);
    ASSERT_TRUE(subr_hashtable_alist(scm_nil, ht) == scm_nil);
  }

  // ----------------------------------------------------------------
  // assq on the alist works correctly
  // ----------------------------------------------------------------
  {
    scm_obj_t args0[] = {};
    scm_obj_t ht = subr_make_eq_hashtable(scm_nil, 0, args0);
    scm_obj_t ka = make_symbol("foo");
    scm_obj_t kb = make_symbol("bar");
    subr_hashtable_set(scm_nil, ht, ka, make_fixnum(42));
    subr_hashtable_set(scm_nil, ht, kb, make_fixnum(99));

    scm_obj_t alist = subr_hashtable_alist(scm_nil, ht);
    scm_obj_t found_a = subr_assq(scm_nil, ka, alist);
    scm_obj_t found_b = subr_assq(scm_nil, kb, alist);
    ASSERT_TRUE(is_cons(found_a) && CDR(found_a) == make_fixnum(42));
    ASSERT_TRUE(is_cons(found_b) && CDR(found_b) == make_fixnum(99));
    ASSERT_TRUE(subr_assq(scm_nil, make_symbol("absent"), alist) == scm_false);
  }

  // ----------------------------------------------------------------
  // type check
  // ----------------------------------------------------------------
  {
    bool threw = false;
    try {
      subr_hashtable_alist(scm_nil, make_fixnum(0));
    } catch (const std::runtime_error&) {
      threw = true;
    }
    ASSERT_TRUE(threw);
  }
}

// ---------------------------------------------------------------------------
// environment-macro-set! / environment-macro-ref / environment-macro-contains?
// environment-variable-set! / environment-variable-ref / environment-variable-contains?
// ---------------------------------------------------------------------------

void test_environment_access() {
  printf("--- environment access ---\n");

  scm_obj_t sym_m = make_symbol("my-macro");
  scm_obj_t sym_v = make_symbol("my-var");
  scm_obj_t transformer = make_fixnum(42);
  scm_obj_t value = make_fixnum(99);

  // macro: set / ref / contains?
  subr_environment_macro_set(scm_nil, sym_m, transformer);
  ASSERT_TRUE(subr_environment_macro_ref(scm_nil, sym_m) == transformer);
  ASSERT_TRUE(subr_environment_macro_contains(scm_nil, sym_m) == scm_true);
  ASSERT_TRUE(subr_environment_macro_contains(scm_nil, make_symbol("absent")) == scm_false);

  // variable: set / ref / contains?
  subr_environment_variable_set(scm_nil, sym_v, value);
  ASSERT_TRUE(subr_environment_variable_ref(scm_nil, sym_v) == value);
  ASSERT_TRUE(subr_environment_variable_contains(scm_nil, sym_v) == scm_true);
  ASSERT_TRUE(subr_environment_variable_contains(scm_nil, make_symbol("absent-var")) == scm_false);

  // mutual exclusion
  scm_obj_t sym_x = make_symbol("x");
  subr_environment_variable_set(scm_nil, sym_x, make_fixnum(1));
  ASSERT_TRUE(subr_environment_variable_contains(scm_nil, sym_x) == scm_true);
  ASSERT_TRUE(subr_environment_macro_contains(scm_nil, sym_x) == scm_false);

  subr_environment_macro_set(scm_nil, sym_x, make_fixnum(2));
  ASSERT_TRUE(subr_environment_macro_contains(scm_nil, sym_x) == scm_true);
  ASSERT_TRUE(subr_environment_variable_contains(scm_nil, sym_x) == scm_false);
}

// ---------------------------------------------------------------------------
// undefined? / unspecified?
// ---------------------------------------------------------------------------

void test_undef_unspecified() {
  printf("--- undefined? / unspecified? ---\n");
  PRED_TRUE(subr_undefined_p, scm_undef);
  PRED_FALSE(subr_undefined_p, scm_unspecified);
  PRED_FALSE(subr_undefined_p, scm_nil);

  PRED_TRUE(subr_unspecified_p, scm_unspecified);
  PRED_FALSE(subr_unspecified_p, scm_undef);
  PRED_FALSE(subr_unspecified_p, scm_nil);
}

// ---------------------------------------------------------------------------
// uuid
// ---------------------------------------------------------------------------

void test_uuid() {
  printf("--- uuid ---\n");
  scm_obj_t u1 = subr_uuid(scm_nil);
  scm_obj_t u2 = subr_uuid(scm_nil);
  ASSERT_TRUE(is_string(u1));
  ASSERT_TRUE(is_string(u2));
  ASSERT_TRUE(subr_string_length(scm_nil, u1) == make_fixnum(36));
  ASSERT_TRUE(subr_string_length(scm_nil, u2) == make_fixnum(36));
  ASSERT_FALSE(u1 == u2);
  ASSERT_FALSE(strcmp((const char*)string_name(u1), (const char*)string_name(u2)) == 0);

  // Check format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
  const char* s1 = (const char*)string_name(u1);
  for (int i = 0; i < 36; i++) {
    if (i == 8 || i == 13 || i == 18 || i == 23) {
      ASSERT_TRUE(s1[i] == '-');
    } else if (i == 14) {
      ASSERT_TRUE(s1[i] == '4');
    } else if (i == 19) {
      char c = s1[i];
      ASSERT_TRUE(c == '8' || c == '9' || c == 'a' || c == 'b');
    } else {
      char c = s1[i];
      ASSERT_TRUE((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'));
    }
  }
}

int main(int argc, char** argv) {
  printf("Starting test_subr\n");
  fflush(stdout);

  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);

  test_boolean_p();
  test_char_p();
  test_eq_p();
  test_eqv_p();
  test_equal_p();
  test_exact_p();
  test_inexact_p();
  test_infinite_p();
  test_integer_p();
  test_list_p();
  test_null_p();
  test_number_p();
  test_nan_p();
  test_pair_p();
  test_procedure_p();
  test_real_p();
  test_string_p();
  test_symbol_p();
  test_vector_p();
  test_not();
  test_vector();
  test_string_length();
  test_string_ref();
  test_string_eq();
  test_string_append();
  test_substring();
  test_symbol_string_conv();
  test_number_to_string();
  test_string_to_number();
  test_char_eq();
  test_char_numeric_p();
  test_max();
  test_pairs_lists_extra();
  test_hashtable_subrs();
  test_hashtable_ops();
  test_values_and_entries();
  test_hashtable_alist();
  test_environment_access();
  test_undef_unspecified();
  test_uuid();

  heap->destroy();
  delete heap;

  if (some_test_failed) {
    printf("\033[31mSome tests FAILED\033[0m\n");
    return 1;
  }
  printf("\033[32mAll tests passed\033[0m\n");
  return 0;
}
