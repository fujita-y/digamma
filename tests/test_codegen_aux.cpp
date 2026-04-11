// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#include "codegen_aux.h"
#include "context.h"
#include "object_heap.h"

// ---------------------------------------------------------------------------
// Stubs
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

void test_c_num_add() {
  printf("--- c_num_add ---\n");

  // flonum + flonum
  scm_obj_t res = c_num_add(make_flonum(1.5), make_flonum(2.5));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);

  // flonum + fixnum
  res = c_num_add(make_flonum(1.5), make_fixnum(2));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 3.5);

  // fixnum + flonum
  res = c_num_add(make_fixnum(10), make_flonum(0.5));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 10.5);
}

void test_c_num_sub() {
  printf("--- c_num_sub ---\n");

  // flonum - flonum
  scm_obj_t res = c_num_sub(make_flonum(5.5), make_flonum(1.5));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);

  // flonum - fixnum
  res = c_num_sub(make_flonum(5.5), make_fixnum(2));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 3.5);

  // fixnum - flonum
  res = c_num_sub(make_fixnum(10), make_flonum(0.5));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 9.5);
}

void test_c_num_mul() {
  printf("--- c_num_mul ---\n");

  // flonum * flonum
  scm_obj_t res = c_num_mul(make_flonum(1.5), make_flonum(2.0));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 3.0);

  // flonum * fixnum
  res = c_num_mul(make_flonum(1.5), make_fixnum(2));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 3.0);

  // fixnum * flonum
  res = c_num_mul(make_fixnum(10), make_flonum(0.5));
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 5.0);
}

void test_c_num_equal() {
  printf("--- c_num_equal ---\n");

  // fixnum == fixnum
  ASSERT_TRUE(c_num_eq(make_fixnum(5), make_fixnum(5)) == scm_true);
  ASSERT_TRUE(c_num_eq(make_fixnum(5), make_fixnum(6)) == scm_false);

  // flonum == flonum
  ASSERT_TRUE(c_num_eq(make_flonum(1.5), make_flonum(1.5)) == scm_true);
  ASSERT_TRUE(c_num_eq(make_flonum(1.5), make_flonum(1.6)) == scm_false);

  // fixnum == flonum
  ASSERT_TRUE(c_num_eq(make_fixnum(5), make_flonum(5.0)) == scm_true);
  ASSERT_TRUE(c_num_eq(make_fixnum(5), make_flonum(5.1)) == scm_false);

  // flonum == fixnum
  ASSERT_TRUE(c_num_eq(make_flonum(5.0), make_fixnum(5)) == scm_true);
  ASSERT_TRUE(c_num_eq(make_flonum(5.1), make_fixnum(5)) == scm_false);
}

void test_c_num_comparisons() {
  printf("--- c_num comparisons (<, >, <=, >=) ---\n");

  // lt
  ASSERT_TRUE(c_num_lt(make_fixnum(5), make_fixnum(6)) == scm_true);
  ASSERT_TRUE(c_num_lt(make_fixnum(5), make_fixnum(5)) == scm_false);
  ASSERT_TRUE(c_num_lt(make_flonum(1.5), make_flonum(1.6)) == scm_true);
  ASSERT_TRUE(c_num_lt(make_flonum(1.5), make_flonum(1.5)) == scm_false);
  ASSERT_TRUE(c_num_lt(make_fixnum(5), make_flonum(5.1)) == scm_true);
  ASSERT_TRUE(c_num_lt(make_flonum(5.1), make_fixnum(5)) == scm_false);

  // gt
  ASSERT_TRUE(c_num_gt(make_fixnum(6), make_fixnum(5)) == scm_true);
  ASSERT_TRUE(c_num_gt(make_fixnum(5), make_fixnum(5)) == scm_false);
  ASSERT_TRUE(c_num_gt(make_flonum(1.6), make_flonum(1.5)) == scm_true);
  ASSERT_TRUE(c_num_gt(make_flonum(1.5), make_flonum(1.5)) == scm_false);

  // le
  ASSERT_TRUE(c_num_le(make_fixnum(5), make_fixnum(6)) == scm_true);
  ASSERT_TRUE(c_num_le(make_fixnum(5), make_fixnum(5)) == scm_true);
  ASSERT_TRUE(c_num_le(make_fixnum(6), make_fixnum(5)) == scm_false);

  // ge
  ASSERT_TRUE(c_num_ge(make_fixnum(6), make_fixnum(5)) == scm_true);
  ASSERT_TRUE(c_num_ge(make_fixnum(5), make_fixnum(5)) == scm_true);
  ASSERT_TRUE(c_num_ge(make_fixnum(5), make_fixnum(6)) == scm_false);
}

int main() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  test_c_num_add();
  test_c_num_sub();
  test_c_num_mul();
  test_c_num_equal();
  test_c_num_comparisons();

  context::destroy();
  heap->destroy();
  delete heap;

  if (some_test_failed) {
    printf("\n\033[31mSOME TESTS FAILED\033[0m\n");
    return 1;
  } else {
    printf("\n\033[32mALL TESTS PASSED\033[0m\n");
    return 0;
  }
}
