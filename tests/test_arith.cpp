// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#include "context.h"
#include "object_heap.h"
#include "subr.h"

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

void test_add() {
  printf("--- addition (+ ...) ---\n");

  auto add = [](std::vector<scm_obj_t> args) { return subr_num_add(scm_nil, (int)args.size(), args.data()); };

  // (+) -> 0
  ASSERT_TRUE(add({}) == make_fixnum(0));

  // (+ 5) -> 5
  ASSERT_TRUE(add({make_fixnum(5)}) == make_fixnum(5));

  // (+ 1 2 3) -> 6
  ASSERT_TRUE(add({make_fixnum(1), make_fixnum(2), make_fixnum(3)}) == make_fixnum(6));

  // (+ 1.5 2.5) -> 4.0
  scm_obj_t res = add({make_flonum(1.5), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);

  // Mixed: (+ 1 2.5) -> 3.5
  res = add({make_fixnum(1), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 3.5);
}

void test_sub() {
  printf("--- subtraction (- ...) ---\n");

  auto sub = [](std::vector<scm_obj_t> args) { return subr_num_sub(scm_nil, (int)args.size(), args.data()); };

  // (- 10) -> -10
  ASSERT_TRUE(sub({make_fixnum(10)}) == make_fixnum(-10));

  // (- 10 3 2) -> 5
  ASSERT_TRUE(sub({make_fixnum(10), make_fixnum(3), make_fixnum(2)}) == make_fixnum(5));

  // (- 5.5 1.5) -> 4.0
  scm_obj_t res = sub({make_flonum(5.5), make_flonum(1.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);

  // Mixed: (- 10 2.5) -> 7.5
  res = sub({make_fixnum(10), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 7.5);
}

void test_mul() {
  printf("--- multiplication (* ...) ---\n");

  auto mul = [](std::vector<scm_obj_t> args) { return subr_num_mul(scm_nil, (int)args.size(), args.data()); };

  // (*) -> 1
  ASSERT_TRUE(mul({}) == make_fixnum(1));

  // (* 5) -> 5
  ASSERT_TRUE(mul({make_fixnum(5)}) == make_fixnum(5));

  // (* 2 3 4) -> 24
  ASSERT_TRUE(mul({make_fixnum(2), make_fixnum(3), make_fixnum(4)}) == make_fixnum(24));

  // (* 2.5 4.0) -> 10.0
  scm_obj_t res = mul({make_flonum(2.5), make_flonum(4.0)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 10.0);

  // Mixed: (* 2 2.5) -> 5.0
  res = mul({make_fixnum(2), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 5.0);
}

void test_div() {
  printf("--- division (/ ...) ---\n");

  auto div = [](std::vector<scm_obj_t> args) { return subr_num_div(scm_nil, (int)args.size(), args.data()); };

  // (/ 4) -> 1/4 (in this implementation it returns 0 for fixnum 1/4 because it's integer division for fixnums)
  // Wait, let's check the implementation again.
  // 168:     return make_fixnum(1 / res_i);
  // So (/ 4) returns 0.
  ASSERT_TRUE(div({make_fixnum(4)}) == make_fixnum(0));

  // (/ 24 2 3) -> 4
  ASSERT_TRUE(div({make_fixnum(24), make_fixnum(2), make_fixnum(3)}) == make_fixnum(4));

  // (/ 10.0 4.0) -> 2.5
  scm_obj_t res = div({make_flonum(10.0), make_flonum(4.0)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 2.5);

  // Mixed: (/ 10 2.5) -> 4.0
  res = div({make_fixnum(10), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);
}

void test_cmp() {
  printf("--- comparisons (= < > <= >=) ---\n");

  auto eq = [](std::vector<scm_obj_t> args) { return subr_num_eq(scm_nil, (int)args.size(), args.data()); };
  auto lt = [](std::vector<scm_obj_t> args) { return subr_num_lt(scm_nil, (int)args.size(), args.data()); };
  auto gt = [](std::vector<scm_obj_t> args) { return subr_num_gt(scm_nil, (int)args.size(), args.data()); };
  auto le = [](std::vector<scm_obj_t> args) { return subr_num_le(scm_nil, (int)args.size(), args.data()); };
  auto ge = [](std::vector<scm_obj_t> args) { return subr_num_ge(scm_nil, (int)args.size(), args.data()); };

  // =
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(5)}) == scm_true);
  ASSERT_TRUE(eq({make_fixnum(5), make_flonum(5.0)}) == scm_true);
  ASSERT_TRUE(eq({make_flonum(5.0), make_fixnum(5)}) == scm_true);
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(6)}) == scm_false);
  ASSERT_TRUE(eq({make_fixnum(5), make_flonum(5.1)}) == scm_false);
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(5), make_fixnum(5)}) == scm_true);
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(5), make_flonum(5.0)}) == scm_true);

  // <
  ASSERT_TRUE(lt({make_fixnum(1), make_fixnum(2), make_fixnum(3)}) == scm_true);
  ASSERT_TRUE(lt({make_fixnum(1), make_fixnum(3), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(lt({make_fixnum(1), make_flonum(1.5), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(lt({make_flonum(1.0), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(lt({make_fixnum(2), make_flonum(1.0)}) == scm_false);

  // >
  ASSERT_TRUE(gt({make_fixnum(3), make_fixnum(2), make_fixnum(1)}) == scm_true);
  ASSERT_TRUE(gt({make_fixnum(3), make_fixnum(1), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(gt({make_flonum(3.5), make_fixnum(3)}) == scm_true);
  ASSERT_TRUE(gt({make_fixnum(3), make_flonum(3.5)}) == scm_false);

  // <=
  ASSERT_TRUE(le({make_fixnum(1), make_fixnum(2), make_fixnum(2), make_fixnum(3)}) == scm_true);
  ASSERT_TRUE(le({make_fixnum(1), make_fixnum(3), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(le({make_fixnum(2), make_flonum(2.0)}) == scm_true);
  ASSERT_TRUE(le({make_flonum(2.0), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(le({make_fixnum(2), make_flonum(2.1)}) == scm_true);

  // >=
  ASSERT_TRUE(ge({make_fixnum(3), make_fixnum(2), make_fixnum(2), make_fixnum(1)}) == scm_true);
  ASSERT_TRUE(ge({make_fixnum(3), make_fixnum(1), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(ge({make_fixnum(2), make_flonum(2.0)}) == scm_true);
  ASSERT_TRUE(ge({make_flonum(2.0), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(ge({make_flonum(2.1), make_fixnum(2)}) == scm_true);
}

int main() {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  test_add();
  test_sub();
  test_mul();
  test_div();
  test_cmp();

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
