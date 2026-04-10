// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "../src/core.h"
#include "../src/equiv.h"
#include "../src/object.h"
#include "../src/object_heap.h"
#include "../src/context.h"

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


static bool some_test_failed = false;


#define ASSERT_TRUE(expr)                       \
  if (!(expr)) {                                \
    printf("\033[31mFAIL: %s\033[0m\n", #expr); \
    some_test_failed = true;                    \
  } else {                                      \
    printf("\033[32mPASS: %s\033[0m\n", #expr); \
  }

#define ASSERT_FALSE(expr)                         \
  if ((expr)) {                                    \
    printf("\033[31mFAIL: !(%s)\033[0m\n", #expr); \
    some_test_failed = true;                       \
  } else {                                         \
    printf("\033[32mPASS: !(%s)\033[0m\n", #expr); \
  }

unsigned int test_address_hash(scm_obj_t obj, unsigned int bound) {
  return (((uintptr_t)obj >> 3) * 2654435761U + ((uintptr_t)obj & 7)) % bound;
}

bool test_address_equiv(scm_obj_t obj1, scm_obj_t obj2) { return obj1 == obj2; }

void test_eqv_simple() {
  printf("test_eqv_simple\n");
  ASSERT_TRUE(eqv_p(scm_true, scm_true));
  ASSERT_FALSE(eqv_p(scm_true, scm_false));
  ASSERT_TRUE(eqv_p(make_fixnum(123), make_fixnum(123)));
  ASSERT_FALSE(eqv_p(make_fixnum(123), make_fixnum(124)));
  ASSERT_TRUE(eqv_p(scm_nil, scm_nil));
  ASSERT_FALSE(eqv_p(scm_nil, scm_undef));
}

void test_eqv_flonum() {
  printf("test_eqv_flonum\n");
  // Short flonum (if fits)
  ASSERT_TRUE(eqv_p(make_flonum(1.5), make_flonum(1.5)));
  ASSERT_FALSE(eqv_p(make_flonum(1.5), make_flonum(1.6)));

  // Long flonum
  scm_obj_t f1 = make_flonum(1.23e100);
  scm_obj_t f2 = make_flonum(1.23e100);
  ASSERT_TRUE(eqv_p(f1, f2));
  ASSERT_FALSE(eqv_p(f1, make_flonum(1.24e100)));

  ASSERT_TRUE(eqv_p(make_flonum(0.0), make_flonum(-0.0)));
}

void test_equal_list(object_heap_t* heap) {
  printf("test_equal_list\n");
  scm_obj_t l1 = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  scm_obj_t l2 = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  scm_obj_t l3 = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(4));

  scm_obj_t visited = make_hashtable(test_address_hash, test_address_equiv, 16);
  ASSERT_TRUE(equal_p(visited, l1, l2));

  visited = make_hashtable(test_address_hash, test_address_equiv, 16);
  ASSERT_FALSE(equal_p(visited, l1, l3));
}

void test_equal_vector(object_heap_t* heap) {
  printf("test_equal_vector\n");
  scm_obj_t v1 = make_vector(3, make_fixnum(0));
  scm_obj_t v2 = make_vector(3, make_fixnum(0));
  scm_obj_t v3 = make_vector(3, make_fixnum(1));
  scm_obj_t* elts1 = vector_elts(v1);
  scm_obj_t* elts2 = vector_elts(v2);
  elts1[0] = make_fixnum(10);
  elts2[0] = make_fixnum(10);

  scm_obj_t visited = make_hashtable(test_address_hash, test_address_equiv, 16);
  ASSERT_TRUE(equal_p(visited, v1, v2));

  visited = make_hashtable(test_address_hash, test_address_equiv, 16);
  ASSERT_FALSE(equal_p(visited, v1, v3));
}

void test_equal_string(object_heap_t* heap) {
  printf("test_equal_string\n");
  scm_obj_t s1 = make_string("hello");
  scm_obj_t s2 = make_string("hello");
  scm_obj_t s3 = make_string("world");

  scm_obj_t visited = make_hashtable(test_address_hash, test_address_equiv, 16);
  ASSERT_TRUE(equal_p(visited, s1, s2));

  visited = make_hashtable(test_address_hash, test_address_equiv, 16);
  ASSERT_FALSE(equal_p(visited, s1, s3));
}

void test_equal_circular(object_heap_t* heap) {
  printf("test_equal_circular\n");
  // l1 = (1 . #0)
  scm_obj_t pair1 = make_cons(make_fixnum(1), scm_nil);
  ((scm_cons_rec_t*)pair1)->cdr = pair1;

  // l2 = (1 . #0)
  scm_obj_t pair2 = make_cons(make_fixnum(1), scm_nil);
  ((scm_cons_rec_t*)pair2)->cdr = pair2;

  // l3 = (1 . 1 . #0)
  scm_obj_t pair3 = make_cons(make_fixnum(1), scm_nil);
  scm_obj_t pair3_head = make_cons(make_fixnum(1), pair3);
  ((scm_cons_rec_t*)pair3)->cdr = pair3_head;

  scm_obj_t visited = make_hashtable(test_address_hash, test_address_equiv, 16);

  ASSERT_TRUE(equal_p(visited, pair1, pair2));

  // Reset visited for next check
  visited = make_hashtable(test_address_hash, test_address_equiv, 16);
  ASSERT_TRUE(equal_p(visited, pair1, pair3_head));
}

int main(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  test_eqv_simple();
  test_eqv_flonum();

  test_equal_list(heap);
  test_equal_vector(heap);
  test_equal_string(heap);
  test_equal_circular(heap);

  context::destroy();
  heap->destroy();
  delete heap;
  return some_test_failed ? 1 : 0;
}
