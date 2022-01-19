// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "arith.h"
#include "bit.h"
#include "heap.h"
#include "port.h"

#if USE_DIGIT32
  #define DIGIT_BIT             32
  #define DIGIT_BIT_MASK        UINT32_MAX
  #define DIGIT_BIT_SHIFT_COUNT 5
typedef uint64_t digit2x_t;
typedef int32_t signed_digit_t;
typedef int64_t signed_digit2x_t;
#else
  #define DIGIT_BIT             64
  #define DIGIT_BIT_MASK        UINT64_MAX
  #define DIGIT_BIT_SHIFT_COUNT 6
typedef uint128_t digit2x_t;
typedef int64_t signed_digit_t;
typedef int128_t signed_digit2x_t;
#endif

#define BN_QUANTUM     32
#define BN_STACK_LIMIT 1024

#define P_DIGITS       308  // (floor (log (* (- 2 (expt 2 -52)) (expt 2 1023)) 10))
#define P_EXP10        22   // (floor (* 53 (log 2 5)))

#ifndef NDEBUG
  #include "printer.h"
  #include "vm.h"
  #define DBGPRT printer_t(current_vm(), current_vm()->m_current_output).format
#endif

/*
    BN_TEMPORARY may not aligned and BIGNUMP() can not use to check it.
    Do not pass BN_TEMPORARY to the function which argument type is scm_obj_t
*/

#define BN_TEMPORARY(NAME) scm_bignum_rec_t NAME

#define BN_ALLOC(VAR, COUNT)                                    \
  do {                                                          \
    if (sizeof(digit_t) * (COUNT) < BN_STACK_LIMIT) {           \
      (VAR).hdr = scm_hdr_bignum;                               \
      (VAR).elts = (digit_t*)alloca(sizeof(digit_t) * (COUNT)); \
      bn_set_count(&(VAR), (COUNT));                            \
    } else {                                                    \
      (VAR) = *(make_bignum(heap, COUNT));                      \
    }                                                           \
  } while (0);

#define BN_ALLOC_2SC(NEW, ORG)                                            \
  do {                                                                    \
    BN_ALLOC((NEW), bn_get_count(ORG) + 1);                               \
    memcpy((NEW).elts, (ORG)->elts, sizeof(digit_t) * bn_get_count(ORG)); \
    (NEW).elts[bn_get_count(ORG)] = 0;                                    \
    bn_flip2sc(&(NEW));                                                   \
  } while (0);

#define BN_ALLOC_FIXNUM(VAR) BN_ALLOC(VAR, 1)

static const int64_t iexpt_2n52 = 0x10000000000000LL;  // 2^(53-1)
static const int64_t iexpt_2n53 = 0x20000000000000LL;  // 2^53

struct bn_div_ans_t {
  scm_obj_t quotient;   // #t: alloc new and set, #f: no need, scm_bignum_t: overwrite
  scm_obj_t remainder;  // #t: alloc new and set, #f: no need, scm_bignum_t: overwrite
};

static bool bn_norm_pred(scm_bignum_t bn) {
  int bn_count = bn_get_count(bn);
  return (bn_count == 0) || (bn->elts[bn_count - 1] != 0);
}

static scm_bignum_t bn_dup(object_heap_t* heap, scm_bignum_t bn) { return make_bignum(heap, bn); }

static void bn_subsection(scm_bignum_t subsec, scm_bignum_t original, int offset) {
  *subsec = *original;
  subsec->elts += offset;
  bn_set_count(subsec, bn_get_count(original) - offset);
}

static void bn_flip2sc(scm_bignum_t bn)  // set 2s complement in elts
{
  int bn_count = bn_get_count(bn);
  digit2x_t acc = 1;
  for (int i = 0; i < bn_count; i++) {
    acc = (digit2x_t)(~bn->elts[i]) + acc;
    bn->elts[i] = (digit_t)acc;
    acc >>= DIGIT_BIT;
  }
}

static int bn_norm(scm_bignum_t bn) {
  int bn_count = bn_get_count(bn);
  if (bn_count) {
    int index = bn_count - 1;
    while (bn->elts[index] == 0) {
      if (--index < 0) {
        bn_set_zero(bn);
        return 0;
      }
    }
    bn_set_count(bn, index + 1);
    return index + 1;
  }
  bn_set_zero(bn);
  return 0;
}

static scm_obj_t bn_to_integer(object_heap_t* heap, scm_bignum_t bn) {
  assert(bn_norm_pred(bn));
  if (bn_get_count(bn) == 0) return MAKEFIXNUM(0);
  assert(bn_get_sign(bn) != 0);
  if (bn_get_count(bn) == 1) {
    signed_digit2x_t n = bn->elts[0];
    if (bn_get_sign(bn) < 0) n = -n;
    if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
  }
  return bn_dup(heap, bn);
}

static scm_obj_t bn_demote(scm_bignum_t bn) {
  assert(bn_get_sign(bn) != 0);
  if (bn_get_count(bn) == 0) return MAKEFIXNUM(0);
  if (bn_get_count(bn) == 1) {
    signed_digit2x_t n = bn->elts[0];
    if (bn_get_sign(bn) < 0) n = -n;
    if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
  }
  return bn;
}

static void bn_lognot(scm_bignum_t ans, scm_bignum_t obj) {
  int ans_count = bn_get_count(ans);
  int obj_count = bn_get_count(obj);
  assert(ans_count >= obj_count);
  for (int i = 0; i < ans_count; i++) {
    if (i < obj_count)
      ans->elts[i] = ~obj->elts[i];
    else
      ans->elts[i] = (digit_t)-1;
  }
  bn_norm(ans);
}

static void bn_logand(scm_bignum_t ans, scm_bignum_t lhs, scm_bignum_t rhs, bool lhs2sc, bool rhs2sc) {
  int ans_count = bn_get_count(ans);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  assert(ans_count >= lhs_count);
  for (int i = 0; i < ans_count; i++) {
    digit_t bit1 = (i < lhs_count) ? lhs->elts[i] : (lhs2sc ? DIGIT_BIT_MASK : 0);
    digit_t bit2 = (i < rhs_count) ? rhs->elts[i] : (rhs2sc ? DIGIT_BIT_MASK : 0);
    ans->elts[i] = bit1 & bit2;
  }
  bn_norm(ans);
}

static void bn_logior(scm_bignum_t ans, scm_bignum_t lhs, scm_bignum_t rhs, bool lhs2sc, bool rhs2sc) {
  int ans_count = bn_get_count(ans);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  assert(ans_count >= lhs_count);
  for (int i = 0; i < ans_count; i++) {
    digit_t bit1 = (i < lhs_count) ? lhs->elts[i] : (lhs2sc ? DIGIT_BIT_MASK : 0);
    digit_t bit2 = (i < rhs_count) ? rhs->elts[i] : (rhs2sc ? DIGIT_BIT_MASK : 0);
    ans->elts[i] = bit1 | bit2;
  }
  bn_norm(ans);
}

static void bn_logxor(scm_bignum_t ans, scm_bignum_t lhs, scm_bignum_t rhs, bool lhs2sc, bool rhs2sc) {
  int ans_count = bn_get_count(ans);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  assert(ans_count >= lhs_count);
  for (int i = 0; i < ans_count; i++) {
    digit_t bit1 = (i < lhs_count) ? lhs->elts[i] : (lhs2sc ? DIGIT_BIT_MASK : 0);
    digit_t bit2 = (i < rhs_count) ? rhs->elts[i] : (rhs2sc ? DIGIT_BIT_MASK : 0);
    ans->elts[i] = bit1 ^ bit2;
  }
  bn_norm(ans);
}

static bool bn_add(scm_bignum_t ans, scm_bignum_t lhs, scm_bignum_t rhs) {
  int ans_count = bn_get_count(ans);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  assert(ans_count >= lhs_count);
  digit2x_t acc = 0;
  for (int i = 0; i < ans_count; i++) {
    if (i < lhs_count) acc = acc + (digit2x_t)lhs->elts[i];
    if (i < rhs_count) acc = acc + (digit2x_t)rhs->elts[i];
    ans->elts[i] = (digit_t)acc;
    acc >>= DIGIT_BIT;
  }
  bn_norm(ans);
  return acc != 0;
}

static bool bn_sub(scm_bignum_t ans, scm_bignum_t lhs, scm_bignum_t rhs) {
  int ans_count = bn_get_count(ans);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  assert(ans_count);
  signed_digit2x_t acc = 0;
  for (int i = 0; i < ans_count; i++) {
    if (i < lhs_count) acc = acc + (digit2x_t)lhs->elts[i];
    if (i < rhs_count) acc = acc - (digit2x_t)rhs->elts[i];
    ans->elts[i] = (digit_t)acc;
    acc >>= DIGIT_BIT;
  }
  if (acc >= 0) {
    bn_norm(ans);
    return false;
  }
  return true;  // underflow
}

static void bn_mul(scm_bignum_t ans, scm_bignum_t lhs, scm_bignum_t rhs) {
  assert(ans->elts != lhs->elts);
  assert(ans->elts != rhs->elts);
  int ans_count = bn_get_count(ans);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  assert(ans_count >= lhs_count + rhs_count);
  memset(ans->elts, 0, sizeof(digit_t) * ans_count);
  for (int i = 0; i < lhs_count; i++) {
    digit2x_t acc = 0;
    for (int j = 0; j < rhs_count; j++) {
      acc = (digit2x_t)lhs->elts[i] * (digit2x_t)rhs->elts[j] + acc + (digit2x_t)ans->elts[i + j];
      ans->elts[i + j] = (digit_t)acc;
      acc >>= DIGIT_BIT;
    }
    ans->elts[i + rhs_count] = (digit_t)acc;
  }
  bn_set_count(ans, rhs_count + lhs_count);
  bn_norm(ans);
}

static void bn_mul_add_digit(scm_bignum_t ans, scm_bignum_t lhs, digit_t rhs, digit_t addend) {
  int lhs_count = bn_get_count(lhs);
  assert(bn_get_count(ans) > lhs_count);
  digit2x_t acc = addend;
  for (int i = 0; i < lhs_count; i++) {
    acc = (digit2x_t)lhs->elts[i] * (digit2x_t)rhs + acc;
    ans->elts[i] = (digit_t)acc;
    acc >>= DIGIT_BIT;
  }
  ans->elts[lhs_count] = acc;
  bn_set_count(ans, lhs_count + 1);
  bn_norm(ans);
}

static digit_t bn_div_digit(scm_bignum_t quotient, scm_bignum_t numerator, digit_t denominator) {
  int numerator_count = bn_get_count(numerator);
#ifndef NDEBUG
  int quotient_count = bn_get_count(quotient);
  assert(quotient_count >= numerator_count);
#endif
  digit2x_t remainder = 0;
  for (int i = numerator_count - 1; i >= 0; i--) {
    remainder = (remainder << DIGIT_BIT) + numerator->elts[i];
    quotient->elts[i] = remainder / denominator;
    remainder = remainder % denominator;
  }
  bn_norm(quotient);
  return remainder;
}

static digit_t bn_remainder_digit(scm_bignum_t numerator, digit_t denominator) {
  int numerator_count = bn_get_count(numerator);
  digit2x_t remainder = 0;
  for (int i = numerator_count - 1; i >= 0; i--) {
    remainder = ((remainder << DIGIT_BIT) + numerator->elts[i]) % denominator;
  }
  return remainder;
}

static int bn_cmp(scm_bignum_t lhs, scm_bignum_t rhs) {
  assert(bn_norm_pred(lhs));
  assert(bn_norm_pred(rhs));
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  if (lhs_count > rhs_count) return 1;
  if (lhs_count < rhs_count) return -1;
  for (int i = lhs_count - 1; i >= 0; i--) {
    if (lhs->elts[i] > rhs->elts[i]) return 1;
    if (lhs->elts[i] < rhs->elts[i]) return -1;
  }
  return 0;
}

static void bn_shift_right_digit(scm_bignum_t bn, int unit) {
  assert(unit >= 0);
  int bn_count = bn_get_count(bn);
  if (bn_count > unit) {
    for (int i = 0; i < bn_count - unit; i++) bn->elts[i] = bn->elts[i + unit];
  } else {
    unit = bn_count;
  }
  for (int i = bn_count - unit; i < bn_count; i++) bn->elts[i] = 0;
}

static void bn_shift_right(scm_bignum_t bn, unsigned int shift) {
  int bit_shift = shift & (DIGIT_BIT - 1);
  shift >>= DIGIT_BIT_SHIFT_COUNT;
  if (shift) bn_shift_right_digit(bn, shift);
  if (bit_shift) {
    int count = bn_get_count(bn) - shift;
    digit_t bits = 0;
    for (int i = count - 1; i >= 0; i--) {
      digit_t bits2 = bn->elts[i] << (DIGIT_BIT - bit_shift);
      bn->elts[i] = (bn->elts[i] >> bit_shift) | bits;
      bits = bits2;
    }
  }
}

static void bn_shift_right_digit_2sc(scm_bignum_t bn, int unit) {
  assert(unit >= 0);
  int bn_count = bn_get_count(bn);
  if (bn_count > unit) {
    for (int i = 0; i < bn_count - unit; i++) bn->elts[i] = bn->elts[i + unit];
  } else {
    unit = bn_count;
  }
  for (int i = bn_count - unit; i < bn_count; i++) bn->elts[i] = DIGIT_BIT_MASK;
}

static void bn_shift_right_2sc(scm_bignum_t bn, unsigned int shift) {
  int bit_shift = shift & (DIGIT_BIT - 1);
  shift >>= DIGIT_BIT_SHIFT_COUNT;
  if (shift) bn_shift_right_digit_2sc(bn, shift);
  if (bit_shift) {
    int count = bn_get_count(bn) - shift;
    digit_t bits = ~(DIGIT_BIT_MASK >> bit_shift);
    for (int i = count - 1; i >= 0; i--) {
      digit_t bits2 = bn->elts[i] << (DIGIT_BIT - bit_shift);
      bn->elts[i] = (bn->elts[i] >> bit_shift) | bits;
      bits = bits2;
    }
  }
}

static void bn_shift_left_digit(scm_bignum_t bn, int unit) {
  assert(unit >= 0);
  int bn_count = bn_get_count(bn);
  if (bn_count > unit) {
    for (int i = bn_count - 1; i >= unit; i--) bn->elts[i] = bn->elts[i - unit];
  } else {
    unit = bn_count;
  }
  for (int i = 0; i < unit; i++) bn->elts[i] = 0;
}

static void bn_shift_left(scm_bignum_t bn, int shift) {
  assert(shift >= 0);
  int bn_count = bn_get_count(bn);
  int bit_shift = shift & (DIGIT_BIT - 1);
  if (bit_shift) {
    digit_t bits = 0;
    for (int i = 0; i < bn_count; i++) {
      digit_t bits2 = bn->elts[i] >> (DIGIT_BIT - bit_shift);
      bn->elts[i] = (bn->elts[i] << bit_shift) | bits;
      bits = bits2;
    }
  }
  if (shift == bit_shift) return;
  bn_shift_left_digit(bn, shift >> DIGIT_BIT_SHIFT_COUNT);
}

static void bn_copy(scm_bignum_t dst, scm_bignum_t src) {
  assert(bn_get_count(dst) >= bn_get_count(src));
  int count = bn_get_count(src);
  bn_set_count(dst, count);
  bn_set_sign(dst, bn_get_sign(src));
  memcpy(dst->elts, src->elts, sizeof(digit_t) * count);
}

static void bn_let(scm_bignum_t dst, scm_fixnum_t src) {
  assert(bn_get_count(dst) >= 1);
  intptr_t value = FIXNUM(src);
  if (value) {
    bn_set_count(dst, 1);
    if (value > 0) {
      dst->elts[0] = value;
      bn_set_sign(dst, 1);
    } else {
      dst->elts[0] = -value;
      bn_set_sign(dst, -1);
    }
  } else {
    bn_set_count(dst, 0);
    bn_set_sign(dst, 0);
  }
}

static void bn_div(object_heap_t* heap, bn_div_ans_t* answer, scm_bignum_t numerator, scm_bignum_t denominator) {
  assert(bn_norm_pred(numerator));
  assert(bn_norm_pred(denominator));
  assert(bn_get_count(numerator));
  assert(bn_get_count(denominator));
  int denominator_count = bn_get_count(denominator);
  int numerator_count = bn_get_count(numerator);
  digit_t bits = denominator->elts[denominator_count - 1];
  int shift = 0;
  while ((signed_digit_t)bits > 0) {
    bits <<= 1;
    shift++;
  }
  bn_shift_left(denominator, shift);
  int remainder_count = numerator_count + 1;
  BN_TEMPORARY(remainder);
  BN_ALLOC(remainder, remainder_count);
  memcpy(remainder.elts, numerator->elts, sizeof(digit_t) * numerator_count);
  remainder.elts[remainder_count - 1] = 0;
  bn_shift_left(&remainder, shift);
  remainder_count = bn_norm(&remainder);
  int quotient_count = remainder_count - denominator_count + 1;
  BN_TEMPORARY(quotient);
  BN_ALLOC(quotient, quotient_count);
  memset(quotient.elts, 0, sizeof(digit_t) * quotient_count);
  int workpad_count = denominator_count + 1;
  BN_TEMPORARY(workpad);
  BN_ALLOC(workpad, workpad_count);
  int qt_index = quotient_count - 1;
  int rd_index = remainder_count - 1;
  int de_index = denominator_count - 1;
  while (qt_index >= 0) {
    assert(rd_index >= 0);
    if (remainder.elts[rd_index] >= denominator->elts[de_index]) {
      BN_TEMPORARY(subsec);
      bn_subsection(&subsec, &remainder, rd_index - de_index);
      if (bn_sub(&subsec, &subsec, denominator)) {
        bn_add(&subsec, &subsec, denominator);  // ignore overflow
      } else {
        quotient.elts[qt_index] = 1;
      }
    }
    if (qt_index > 0) {
      assert(rd_index > 0);
      digit2x_t n = ((digit2x_t)remainder.elts[rd_index] << DIGIT_BIT) + remainder.elts[rd_index - 1];
      if (n > denominator->elts[de_index]) {
        digit_t qt = 0;
        if (remainder.elts[rd_index] < denominator->elts[de_index]) {
          qt = n / denominator->elts[de_index];
          bn_set_count(&workpad, workpad_count);
          bn_mul_add_digit(&workpad, denominator, qt, 0);
        } else {
          assert(remainder.elts[rd_index] == denominator->elts[de_index]);
          bn_set_count(&workpad, workpad_count);
          memcpy(workpad.elts, denominator->elts, sizeof(digit_t) * denominator_count);
          bn_shift_left(&workpad, DIGIT_BIT);
        }
        BN_TEMPORARY(subsec);
        bn_subsection(&subsec, &remainder, rd_index - (workpad_count - 1));
        bn_norm(&subsec);
        while (bn_cmp(&subsec, &workpad) < 0) {
          bn_sub(&workpad, &workpad, denominator);
          qt--;
        }
        assert(qt >= 0);
        bn_sub(&subsec, &subsec, &workpad);
        quotient.elts[qt_index - 1] = qt;
      } else {
        quotient.elts[qt_index - 1] = 0;
      }
    }
    qt_index--;
    rd_index--;
  }
  bn_shift_right(denominator, shift);
  if (answer->quotient != scm_false) {
    bn_norm(&quotient);
    if (answer->quotient == scm_true)
      answer->quotient = bn_dup(heap, &quotient);
    else
      bn_copy((scm_bignum_t)answer->quotient, &quotient);
  }
  if (answer->remainder != scm_false) {
    bn_shift_right(&remainder, shift);
    bn_norm(&remainder);
    if (answer->remainder == scm_true)
      answer->remainder = bn_dup(heap, &remainder);
    else
      bn_copy((scm_bignum_t)answer->remainder, &remainder);
  }
}

static void bn_quotient(object_heap_t* heap, scm_bignum_t quotient, scm_bignum_t lhs, scm_bignum_t rhs) {
  bn_div_ans_t ans;
  ans.quotient = quotient;
  ans.remainder = scm_false;
  bn_div(heap, &ans, lhs, rhs);
}

static int bn_bitsize(scm_bignum_t obj) {
  int last = bn_get_count(obj) - 1;
  assert(last >= 0);
  assert(obj->elts[last]);
  int bitsize = DIGIT_BIT * last;
  return bitsize + DIGIT_BIT - nlz(obj->elts[last]);
}

static void bn_sqrt(object_heap_t* heap, scm_bignum_t obj) {
  if (bn_get_sign(obj) == 0) return;
  int count = bn_get_count(obj);
  BN_TEMPORARY(s);
  BN_ALLOC(s, count);
  bn_set_sign(&s, 1);
  memcpy(s.elts, obj->elts, sizeof(digit_t) * count);
  int workpad_count = count + 1;
  BN_TEMPORARY(workpad);
  BN_ALLOC(workpad, workpad_count);
  bn_set_sign(&workpad, 1);
  int bitsize = bn_bitsize(obj);
  bn_shift_right(&s, (bitsize - 1) / 2);
  bn_norm(&s);
  while (true) {
    memset(workpad.elts, 0, sizeof(digit_t) * workpad_count);
    bn_set_count(&workpad, workpad_count);
    bn_quotient(heap, &workpad, obj, &s);
    bn_set_count(&workpad, bn_get_count(&workpad) + 1);
    bool overflow = bn_add(&workpad, &workpad, &s);
    assert(overflow == false);
    bn_shift_right(&workpad, 1);
    bn_norm(&workpad);
    if (bn_cmp(&workpad, &s) >= 0) {
      bn_copy(obj, &s);
      bn_set_sign(obj, 1);
      bn_norm(obj);
      return;
    }
    count = bn_get_count(&workpad);
    bn_set_count(&s, count);
    memcpy(s.elts, workpad.elts, sizeof(digit_t) * count);
  }
}

static double bignum_to_double(scm_bignum_t obj) {
  int count = bn_get_count(obj);
  double ans = 0.0;
#if USE_DIGIT32
  // to double conversion use last 3 elts (96bits)
  if (count == 0)
    return 0.0;
  else if (count == 1)
    ans = obj->elts[0];
  else if (count == 2)
    ans = obj->elts[1] * (double)4294967296.0 + obj->elts[0];
  else
    for (int i = count - 1; i >= count - 3; i--) ans += ldexp((double)obj->elts[i], DIGIT_BIT * i);
  return bn_get_sign(obj) > 0 ? ans : -ans;
#else
  // to double conversion use last 2 elts (128bits)
  if (count == 0)
    return 0.0;
  else if (count == 1)
    ans = obj->elts[0];
  else
    for (int i = count - 1; i >= count - 2; i--) ans += ldexp((double)obj->elts[i], DIGIT_BIT * i);
  return bn_get_sign(obj) > 0 ? ans : -ans;
#endif
}

static double bignum_shift_right_to_double(scm_bignum_t obj, int shift) {
  int count = bn_get_count(obj);
  if (count == 0) return 0.0;
  if (count == 1) return (double)(obj->elts[0] >> shift);
  if (bn_bitsize(obj) <= shift) return 0.0;
  double ans = 0.0;
#if USE_DIGIT32
  // to double conversion use last 3 elts (96bits)
  for (int i = count - 1; i >= 0 && i >= count - 3; i--) {
    int n = DIGIT_BIT * i - shift;
    ans = ans + ldexp((double)obj->elts[i], n);
  }
#else
  // to double conversion use last 2 elts (128bits)
  for (int i = count - 1; i >= 0 && i >= count - 2; i--) {
    int n = DIGIT_BIT * i - shift;
    ans = ans + ldexp((double)obj->elts[i], n);
  }
#endif
  return bn_get_sign(obj) > 0 ? ans : -ans;
}

static double rational_to_double(scm_rational_t obj) {
  const int BITSIZE_TH = 96;
  double nume = FIXNUMP(obj->nume) ? FIXNUM(obj->nume) : bignum_to_double((scm_bignum_t)obj->nume);
  double deno = FIXNUMP(obj->deno) ? FIXNUM(obj->deno) : bignum_to_double((scm_bignum_t)obj->deno);
  if (isinf(nume) || isinf(deno)) {
    if (isinf(nume) && isinf(deno)) {
      int nume_bitsize = bn_bitsize((scm_bignum_t)obj->nume);
      int deno_bitsize = bn_bitsize((scm_bignum_t)obj->deno);
      int shift = (nume_bitsize > deno_bitsize) ? nume_bitsize - BITSIZE_TH : deno_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = bignum_shift_right_to_double((scm_bignum_t)obj->nume, shift);
      deno = bignum_shift_right_to_double((scm_bignum_t)obj->deno, shift);
    } else if (isinf(deno)) {
      int deno_bitsize = bn_bitsize((scm_bignum_t)obj->deno);
      int shift = deno_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = ldexp(nume, -shift);
      deno = bignum_shift_right_to_double((scm_bignum_t)obj->deno, shift);
    } else {
      int nume_bitsize = bn_bitsize((scm_bignum_t)obj->nume);
      int shift = nume_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = bignum_shift_right_to_double((scm_bignum_t)obj->nume, shift);
      deno = ldexp(deno, -shift);
    }
  }
  return nume / deno;
}

bool bignum_to_int32(scm_bignum_t bn, int32_t* ans) {
  assert(bn_norm_pred(bn));
  if (bn_get_count(bn) == 1) {
    if (bn_get_sign(bn) > 0) {
      if (bn->elts[0] <= INT32_MAX) {
        *ans = bn->elts[0];
        return true;
      }
    } else {
      if (bn->elts[0] <= (uint32_t)INT32_MAX + 1) {
        *ans = -bn->elts[0];
        return true;
      }
    }
  }
  return false;
}

bool bignum_to_uint32(scm_bignum_t bn, uint32_t* ans) {
  assert(bn_norm_pred(bn));
#if USE_DIGIT32
  if ((bn_get_sign(bn) < 0) || (bn_get_count(bn) > 1)) return false;
  *ans = bn->elts[0];
  return true;
#else
  if ((bn_get_sign(bn) < 0) || (bn_get_count(bn) > 1)) return false;
  if (bn->elts[0] <= UINT32_MAX) {
    *ans = bn->elts[0];
    return true;
  }
  return false;
#endif
}

bool bignum_to_int64(scm_bignum_t bn, int64_t* ans) {
  assert(bn_norm_pred(bn));
#if USE_DIGIT32
  if (bn_get_count(bn) == 1) {
    if (bn_get_sign(bn) > 0) {
      *ans = (signed_digit2x_t)bn->elts[0];
      return true;
    } else {
      *ans = -((signed_digit2x_t)bn->elts[0]);
      return true;
    }
  }
  if (bn_get_count(bn) == 2) {
    digit2x_t value = ((digit2x_t)bn->elts[1] << DIGIT_BIT) + (digit2x_t)bn->elts[0];
    if (bn_get_sign(bn) > 0) {
      if (value <= INT64_MAX) {
        *ans = value;
        return true;
      }
    } else {
      if (value <= (digit2x_t)INT64_MAX + 1) {
        *ans = -value;
        return true;
      }
    }
  }
  return false;
#else
  if (bn_get_count(bn) == 1) {
    if (bn_get_sign(bn) > 0) {
      if (bn->elts[0] <= INT64_MAX) {
        *ans = bn->elts[0];
        return true;
      }
    } else {
      if (bn->elts[0] <= (digit_t)INT64_MAX + 1) {
        *ans = -bn->elts[0];
        return true;
      }
    }
  }
  return false;
#endif
}

bool bignum_to_uint64(scm_bignum_t bn, uint64_t* ans) {
  assert(bn_norm_pred(bn));
#if USE_DIGIT32
  if (bn_get_sign(bn) < 0) return false;
  if (bn_get_count(bn) == 1) {
    *ans = (digit2x_t)bn->elts[0];
    return true;
  }
  if (bn_get_count(bn) == 2) {
    *ans = ((digit2x_t)bn->elts[1] << DIGIT_BIT) + (digit2x_t)bn->elts[0];
    return true;
  }
  return false;
#else
  if (bn_get_sign(bn) < 0) return false;
  if (bn_get_count(bn) == 1) {
    *ans = bn->elts[0];
    return true;
  }
  return false;
#endif
}

bool exact_integer_to_int32(scm_obj_t obj, int32_t* ans) {
  if (FIXNUMP(obj)) {
#if USE_DIGIT32
    *ans = FIXNUM(obj);
    return true;
#else
    intptr_t value = FIXNUM(obj);
    if (value >= INT32_MIN && value <= INT32_MAX) {
      *ans = value;
      return true;
    }
    return false;
#endif
  }
  if (BIGNUMP(obj)) return bignum_to_int32((scm_bignum_t)obj, ans);
  return false;
}

bool exact_integer_to_uint32(scm_obj_t obj, uint32_t* ans) {
  if (FIXNUMP(obj)) {
#if USE_DIGIT32
    intptr_t value = FIXNUM(obj);
    if (value >= 0) {
      *ans = value;
      return true;
    }
    return false;
#else
    intptr_t value = FIXNUM(obj);
    if (value >= 0 && value <= UINT32_MAX) {
      *ans = value;
      return true;
    }
    return false;
#endif
  }
  if (BIGNUMP(obj)) return bignum_to_uint32((scm_bignum_t)obj, ans);
  return false;
}

bool exact_integer_to_int64(scm_obj_t obj, int64_t* ans) {
  if (FIXNUMP(obj)) {
    *ans = FIXNUM(obj);
    return true;
  }
  if (BIGNUMP(obj)) {
    return bignum_to_int64((scm_bignum_t)obj, ans);
  }
  return false;
}

bool exact_integer_to_uint64(scm_obj_t obj, uint64_t* ans) {
  if (FIXNUMP(obj)) {
    intptr_t value = FIXNUM(obj);
    if (value >= 0) {
      *ans = FIXNUM(obj);
      return true;
    }
    return false;
  }
  if (BIGNUMP(obj)) {
    return bignum_to_uint64((scm_bignum_t)obj, ans);
  }
  return false;
}

bool exact_integer_to_int16(scm_obj_t obj, int16_t* ans) {
  assert(FIXNUMP(obj));
  intptr_t val = FIXNUM(obj);
  if (val >= INT16_MIN && val <= INT16_MAX) {
    *ans = val;
    return true;
  }
  return false;
}

bool exact_integer_to_uint16(scm_obj_t obj, uint16_t* ans) {
  assert(FIXNUMP(obj));
  intptr_t val = FIXNUM(obj);
  if (val >= 0 && val <= UINT16_MAX) {
    *ans = val;
    return true;
  }
  return false;
}

intptr_t coerce_exact_integer_to_intptr(scm_obj_t obj) {
  if (FIXNUMP(obj)) return FIXNUM(obj);
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
#if USE_DIGIT32
    assert(sizeof(intptr_t) == 4);
#else
    assert(sizeof(intptr_t) == 8);
#endif
    if (bn_get_sign(bn) > 0) return bn->elts[0];
    intptr_t n = bn->elts[0];
    return (-n);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

int64_t coerce_exact_integer_to_int64(scm_obj_t obj) {
#if USE_DIGIT32
  if (FIXNUMP(obj)) return FIXNUM(obj);
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    if (bn_get_count(bn) == 1) {
      if (bn_get_sign(bn) > 0) return bn->elts[0];
      int64_t n = bn->elts[0];
      return (-n);
    }
    if (bn_get_sign(bn) > 0) return bn->elts[0] + ((int64_t)bn->elts[1] << 32);
    int64_t n = bn->elts[0] + ((int64_t)bn->elts[1] << 32);
    return (-n);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
#else
  return coerce_exact_integer_to_intptr(obj);
#endif
}

scm_obj_t int32_to_bignum(object_heap_t* heap, int32_t value) {
  if (value) {
    scm_bignum_t ans = make_bignum(heap, 1);
    if (value > 0) {
      bn_set_sign(ans, 1);
      ans->elts[0] = value;
    } else {
      bn_set_sign(ans, -1);
      ans->elts[0] = -value;
    }
    return ans;
  }
  return make_bignum(heap, 0);
}

scm_obj_t uint32_to_bignum(object_heap_t* heap, uint32_t value) {
  if (value) {
    scm_bignum_t ans = make_bignum(heap, 1);
    bn_set_sign(ans, 1);
    ans->elts[0] = value;
    return ans;
  }
  return make_bignum(heap, 0);
}

scm_obj_t int64_to_bignum(object_heap_t* heap, int64_t value) {
#if USE_DIGIT32
  if (value) {
    int sign;
    if (value > 0) {
      sign = 1;
    } else {
      sign = -1;
      value = -value;
    }
    scm_bignum_t ans;
    if ((value >> DIGIT_BIT) != 0) {
      ans = make_bignum(heap, 2);
      ans->elts[0] = value & DIGIT_BIT_MASK;
      ans->elts[1] = value >> DIGIT_BIT;
    } else {
      ans = make_bignum(heap, 1);
      ans->elts[0] = value;
    }
    bn_set_sign(ans, sign);
    return ans;
  }
  return make_bignum(heap, 0);
#else
  if (value) {
    scm_bignum_t ans = make_bignum(heap, 1);
    if (value > 0) {
      bn_set_sign(ans, 1);
      ans->elts[0] = value;
    } else {
      bn_set_sign(ans, -1);
      ans->elts[0] = -value;
    }
    return ans;
  }
  return make_bignum(heap, 0);
#endif
}

scm_obj_t uint64_to_bignum(object_heap_t* heap, uint64_t value) {
#if USE_DIGIT32
  if (value) {
    scm_bignum_t ans;
    if ((value >> DIGIT_BIT) != 0) {
      ans = make_bignum(heap, 2);
      ans->elts[0] = value & DIGIT_BIT_MASK;
      ans->elts[1] = value >> DIGIT_BIT;
    } else {
      ans = make_bignum(heap, 1);
      ans->elts[0] = value;
    }
    bn_set_sign(ans, 1);
    return ans;
  }
  return make_bignum(heap, 0);
#else
  if (value) {
    scm_bignum_t ans = make_bignum(heap, 1);
    bn_set_sign(ans, 1);
    ans->elts[0] = value;
    return ans;
  }
  return make_bignum(heap, 0);
#endif
}

#if USE_DIGIT64
scm_obj_t int128_to_bignum(object_heap_t* heap, int128_t value) {
  if (value) {
    int sign;
    if (value > 0) {
      sign = 1;
    } else {
      sign = -1;
      value = -value;
    }
    scm_bignum_t ans;
    if ((value >> DIGIT_BIT) != 0) {
      ans = make_bignum(heap, 2);
      ans->elts[0] = value & DIGIT_BIT_MASK;
      ans->elts[1] = value >> DIGIT_BIT;
      bn_set_sign(ans, sign);
      return ans;
    }
    ans = make_bignum(heap, 1);
    ans->elts[0] = value;
    bn_set_sign(ans, sign);
    return ans;
  }
  return make_bignum(heap, 0);
}
#endif

static scm_obj_t signed_digit2x_to_bignum(object_heap_t* heap, signed_digit2x_t value) {
#if USE_DIGIT32
  return int64_to_bignum(heap, value);
#else
  return int128_to_bignum(heap, value);
#endif
}

scm_obj_t intptr_to_bignum(object_heap_t* heap, intptr_t value) {
  if (sizeof(intptr_t) == sizeof(int32_t)) return int32_to_bignum(heap, value);
  return int64_to_bignum(heap, value);
}

scm_obj_t double_to_inexact(object_heap_t* heap, double value) { return make_flonum(heap, value); }

double real_to_double(scm_obj_t obj) {
  if (FIXNUMP(obj)) return (double)FIXNUM(obj);
  if (FLONUMP(obj)) return ((scm_flonum_t)obj)->value;
  if (BIGNUMP(obj)) return bignum_to_double((scm_bignum_t)obj);
  if (RATIONALP(obj)) return rational_to_double((scm_rational_t)obj);
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    if (n_zero_pred(cn->imag)) return real_to_double(cn->real);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

static int64_t decode_double(double n, int* exp, int* sign) {
  union {
    double f64;
    uint64_t u64;
  } datum;
  datum.f64 = n;
  uint64_t bits = datum.u64;
  uint64_t mant_bits = bits & (iexpt_2n52 - 1);
  uint32_t sign_bits = bits >> 63;
  uint32_t exp_bits = (bits >> 52) & 0x7ff;
  if (n == 0.0) {
    *exp = 0;
    *sign = sign_bits ? -1 : 1;
    return 0;
  }
  if (isnan(n)) {
    *exp = 972;
    *sign = 1;
    return 0x18000000000000LL;  // (uint64_t)0x180000 << 32;
  }
  if (isinf(n)) {
    *exp = 972;
    *sign = sign_bits ? -1 : 1;
    return 0x10000000000000LL;  // (uint64_t)0x100000 << 32;
  }
  assert(exp_bits != 0x7ff);
  *exp = (exp_bits ? (int)exp_bits - 1023 : -1022) - 52;
  *sign = sign_bits ? -1 : 1;
  if (exp_bits) mant_bits |= iexpt_2n52;
  return mant_bits;
}

static double pow10n(double value, int n) {
  assert(value != 0.0);
  assert(DBL_MAX_10_EXP <= 511);
  assert(DBL_MIN_10_EXP >= -511);
  static const double bigtens[] = {1.0e+16, 1.0e+32, 1.0e+64, 1.0e+128, 1.0e+256};
  static const double tens[] = {1.0,     1.0e+1,  1.0e+2,  1.0e+3,  1.0e+4,  1.0e+5,  1.0e+6,  1.0e+7,  1.0e+8,  1.0e+9,  1.0e+10, 1.0e+11,
                                1.0e+12, 1.0e+13, 1.0e+14, 1.0e+15, 1.0e+16, 1.0e+17, 1.0e+18, 1.0e+19, 1.0e+20, 1.0e+21, 1.0e+22};
  assert(array_sizeof(tens) == P_EXP10 + 1);
  bool inflate = true;
  if (n < 0) {
    inflate = false;
    n = -n;
  }
  if (n <= P_EXP10) return inflate ? value * tens[n] : value / tens[n];
  if (n > 511) n = 511;
  if (n & 0x0f) {
    if (inflate)
      value *= tens[n & 0x0f];
    else
      value /= tens[n & 0x0f];
  }
  n >>= 4;
  for (int i = 0; i <= 4; i++) {
    if (n & 1) {
      if (inflate)
        value *= bigtens[i];
      else
        value /= bigtens[i];
    }
    n >>= 1;
  }
  if (value == 0.0) return ldexp(1.0, -1074);  // underflow
  return value;
}

static scm_obj_t oprtr_inexact_negate(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) return make_flonum(heap, -FIXNUM(obj));
  if (FLONUMP(obj)) return make_flonum(heap, -((scm_flonum_t)obj)->value);
  if (BIGNUMP(obj)) return make_flonum(heap, -bignum_to_double((scm_bignum_t)obj));
  if (RATIONALP(obj)) return make_flonum(heap, -rational_to_double((scm_rational_t)obj));
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    return make_complex(heap, cnvt_to_inexact(heap, arith_negate(heap, cn->real)), cnvt_to_inexact(heap, arith_negate(heap, cn->imag)));
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

static scm_obj_t oprtr_norm_integer(object_heap_t* heap, scm_obj_t obj) {
  assert(FIXNUMP(obj) || BIGNUMP(obj));
  if (BIGNUMP(obj)) return bn_to_integer(heap, (scm_bignum_t)obj);
  return obj;
}

static scm_obj_t oprtr_norm_complex(object_heap_t* heap, scm_obj_t real, scm_obj_t imag) {
  assert(!COMPLEXP(real));
  assert(!COMPLEXP(imag));
  if (FIXNUMP(imag) && FIXNUM(imag) == 0) return real;
  if (BIGNUMP(imag) && bn_get_sign((scm_bignum_t)imag) == 0) return real;
  if (FLONUMP(real) + FLONUMP(imag) == 1) {
    return make_complex(heap, cnvt_to_inexact(heap, real), cnvt_to_inexact(heap, imag));
  }
  return make_complex(heap, real, imag);
}

static scm_obj_t oprtr_reduce_fixnum_fixnum(object_heap_t* heap, scm_fixnum_t numerator, scm_fixnum_t denominator) {
  intptr_t nume = FIXNUM(numerator);
  intptr_t deno = FIXNUM(denominator);
  assert(deno != 0);
  if (deno == 1) return numerator;
  if (deno == -1) return arith_negate(heap, numerator);
  if (nume == 0) return MAKEFIXNUM(0);
  if (nume == 1) {
    if (deno < 0) return make_rational(heap, MAKEFIXNUM(-1), intptr_to_integer(heap, -deno));
    return make_rational(heap, numerator, denominator);
  }
  if (nume == -1) {
    if (deno < 0) return make_rational(heap, MAKEFIXNUM(1), intptr_to_integer(heap, -deno));
    return make_rational(heap, numerator, denominator);
  }
  int ans_sign = 1;
  if (nume < 0) {
    ans_sign = -ans_sign;
    nume = -nume;
  }
  if (deno < 0) {
    ans_sign = -ans_sign;
    deno = -deno;
  }
  intptr_t n1 = nume;
  intptr_t n2 = deno;
  while (n2) {
    intptr_t t = n2;
    n2 = n1 % n2;
    n1 = t;
  }
  intptr_t gcd = n1;
  if (deno == gcd) return intptr_to_integer(heap, nume * ans_sign / gcd);
  return make_rational(heap, intptr_to_integer(heap, nume * ans_sign / gcd), MAKEFIXNUM(deno / gcd));
}

static scm_obj_t oprtr_reduce_fixnum_bignum(object_heap_t* heap, scm_fixnum_t numerator, scm_bignum_t denominator) {
  if (numerator == MAKEFIXNUM(0)) return MAKEFIXNUM(0);
  if (numerator == MAKEFIXNUM(1)) {
    if (bn_get_sign(denominator) < 0) return make_rational(heap, MAKEFIXNUM(-1), arith_negate(heap, denominator));
    return make_rational(heap, numerator, denominator);
  }
  if (numerator == MAKEFIXNUM(-1)) {
    if (bn_get_sign(denominator) < 0) return make_rational(heap, MAKEFIXNUM(1), arith_negate(heap, denominator));
    return make_rational(heap, numerator, denominator);
  }
  int ans_sign = 1;
  signed_digit_t nume = FIXNUM(numerator);
  if (nume < 0) {
    ans_sign = -ans_sign;
    nume = -nume;
  }
  if (bn_get_sign(denominator) < 0) {
    ans_sign = -ans_sign;
  }
  signed_digit_t n1 = bn_remainder_digit(denominator, nume);
  signed_digit_t n2 = nume;
  while (n2) {
    signed_digit_t t = n2;
    n2 = n1 % n2;
    n1 = t;
  }
  signed_digit_t gcd = n1;
  nume = nume / gcd;
  if (ans_sign < 0) nume = -nume;
  BN_TEMPORARY(quo);
  int count = bn_get_count(denominator);
  BN_ALLOC(quo, count);
  memset(quo.elts, 0, sizeof(digit_t) * count);
  bn_div_digit(&quo, denominator, gcd);
  bn_set_sign(&quo, 1);
  scm_obj_t ans_nume = intptr_to_integer(heap, nume);
  scm_obj_t ans_deno = bn_to_integer(heap, &quo);
  if (ans_deno == MAKEFIXNUM(1)) return ans_nume;
  return make_rational(heap, ans_nume, ans_deno);
}

static scm_obj_t oprtr_reduce_bignum_fixnum(object_heap_t* heap, scm_bignum_t numerator, scm_fixnum_t denominator) {
  if (denominator == MAKEFIXNUM(1)) return numerator;
  if (denominator == MAKEFIXNUM(-1)) return arith_negate(heap, numerator);
  int ans_sign = 1;
  intptr_t deno = FIXNUM(denominator);
  if (bn_get_sign(numerator) < 0) {
    ans_sign = -ans_sign;
  }
  if (deno < 0) {
    ans_sign = -ans_sign;
    deno = -deno;
  }
  intptr_t n1 = bn_remainder_digit(numerator, deno);
  intptr_t n2 = deno;
  while (n2) {
    intptr_t t = n2;
    n2 = n1 % n2;
    n1 = t;
  }
  intptr_t gcd = n1;
  deno = deno / gcd;
  BN_TEMPORARY(quo);
  int count = bn_get_count(numerator);
  BN_ALLOC(quo, count);
  memset(quo.elts, 0, sizeof(digit_t) * count);
  bn_div_digit(&quo, numerator, gcd);
  bn_set_sign(&quo, ans_sign);
  if (deno == 1) return bn_to_integer(heap, &quo);
  return make_rational(heap, bn_to_integer(heap, &quo), intptr_to_integer(heap, deno));
}

static scm_obj_t oprtr_reduce(object_heap_t* heap, scm_obj_t numerator, scm_obj_t denominator) {
  assert(FIXNUMP(numerator) || BIGNUMP(numerator));
  assert(FIXNUMP(denominator) || BIGNUMP(denominator));
  assert(n_zero_pred(denominator) == false);
  if (FIXNUMP(numerator)) {
    if (FIXNUMP(denominator)) return oprtr_reduce_fixnum_fixnum(heap, (scm_fixnum_t)numerator, (scm_fixnum_t)denominator);
    return oprtr_reduce_fixnum_bignum(heap, (scm_fixnum_t)numerator, (scm_bignum_t)denominator);
  }
  if (FIXNUMP(denominator)) {
    return oprtr_reduce_bignum_fixnum(heap, (scm_bignum_t)numerator, (scm_fixnum_t)denominator);
  }
  if (denominator == MAKEFIXNUM(1)) return numerator;
  if (denominator == MAKEFIXNUM(-1)) return arith_negate(heap, numerator);
  if (numerator == MAKEFIXNUM(0)) return MAKEFIXNUM(0);
  if (numerator == MAKEFIXNUM(1)) {
    if (n_negative_pred(denominator)) return make_rational(heap, MAKEFIXNUM(-1), arith_negate(heap, denominator));
    return make_rational(heap, MAKEFIXNUM(1), denominator);
  }
  if (numerator == MAKEFIXNUM(-1)) {
    if (n_negative_pred(denominator)) return make_rational(heap, MAKEFIXNUM(1), arith_negate(heap, denominator));
    return make_rational(heap, MAKEFIXNUM(-1), denominator);
  }
  BN_TEMPORARY(n1);
  BN_TEMPORARY(n2);
  int ans_sign;
  int n1_count;
  int n2_count;
  if (BIGNUMP(numerator)) {
    assert(bn_norm_pred((scm_bignum_t)numerator));
    assert(bn_get_count((scm_bignum_t)numerator) != 0);
    n1_count = bn_get_count((scm_bignum_t)numerator);
    assert(n1_count);
    BN_ALLOC(n1, n1_count);
    memcpy(n1.elts, ((scm_bignum_t)numerator)->elts, sizeof(digit_t) * n1_count);
    ans_sign = bn_get_sign((scm_bignum_t)numerator);
  } else {
    BN_ALLOC_FIXNUM(n1);
    n1_count = 1;
    bn_let(&n1, (scm_fixnum_t)numerator);
    ans_sign = bn_get_sign(&n1);
  }
  if (BIGNUMP(denominator)) {
    assert(bn_norm_pred((scm_bignum_t)denominator));
    if (bn_get_sign((scm_bignum_t)denominator) < 0) ans_sign = -ans_sign;
    n2_count = bn_get_count((scm_bignum_t)denominator);
    assert(n2_count);
    BN_ALLOC(n2, n2_count);
    memcpy(n2.elts, ((scm_bignum_t)denominator)->elts, sizeof(digit_t) * n2_count);
  } else {
    BN_ALLOC_FIXNUM(n2);
    n2_count = 1;
    bn_let(&n2, (scm_fixnum_t)denominator);
    if (bn_get_sign(&n2) == -1) ans_sign = -ans_sign;
  }
  BN_TEMPORARY(divisor);
  int divisor_count = (n1_count > n2_count) ? n1_count : n2_count;
  BN_ALLOC(divisor, divisor_count);
  int shift = 0;
  while (((n1.elts[0] | n2.elts[0]) & 1) == 0) {
    bn_shift_right(&n1, 1);
    bn_shift_right(&n2, 1);
    shift++;
  }
  bn_norm(&n1);
  bn_norm(&n2);
  while (bn_get_count(&n1)) {
    if ((n1.elts[0] & 1) == 0) {
      bn_shift_right(&n1, 1);
      bn_norm(&n1);
    } else if ((n2.elts[0] & 1) == 0) {
      bn_shift_right(&n2, 1);
      bn_norm(&n2);
    } else {
      if (bn_cmp(&n1, &n2) < 0) {
        // n2 = (n2 - n1) >> 1;
        bn_sub(&n2, &n2, &n1);
        bn_shift_right(&n2, 1);
        bn_norm(&n2);
      } else {
        // n1 = (n1 - n2) >> 1;
        bn_sub(&n1, &n1, &n2);
        bn_shift_right(&n1, 1);
        bn_norm(&n1);
      }
    }
  }
  if ((bn_get_count(&n2) == 1) & (n2.elts[0] == 1) & (shift == 0)) {
    if (n_negative_pred(denominator)) {
      return make_rational(heap, arith_negate(heap, numerator), arith_negate(heap, denominator));
    }
    return make_rational(heap, numerator, denominator);
  }
  // n2 -> divisor
  bn_copy(&divisor, &n2);
  // numerator -> n1
  bn_set_count(&n1, n1_count);
  if (BIGNUMP(numerator)) {
    memcpy(n1.elts, ((scm_bignum_t)numerator)->elts, sizeof(digit_t) * n1_count);
  } else {
    intptr_t value = FIXNUM(numerator);
    n1.elts[0] = (value > 0) ? value : -value;
  }
  // denominator-> n2
  bn_set_count(&n2, n2_count);
  if (BIGNUMP(denominator)) {
    memcpy(n2.elts, ((scm_bignum_t)denominator)->elts, sizeof(digit_t) * n2_count);
  } else {
    intptr_t value = FIXNUM(denominator);
    n2.elts[0] = (value > 0) ? value : -value;
  }
  bn_shift_right(&n1, shift);
  bn_norm(&n1);
  bn_quotient(heap, &n1, &n1, &divisor);
  bn_set_sign(&n1, ans_sign);
  bn_shift_right(&n2, shift);
  bn_norm(&n2);
  bn_quotient(heap, &n2, &n2, &divisor);
  bn_set_sign(&n2, 1);
  scm_obj_t ans_numerator = bn_to_integer(heap, &n1);
  scm_obj_t ans_denominator = bn_to_integer(heap, &n2);
  if (ans_denominator == MAKEFIXNUM(1)) return ans_numerator;
  return make_rational(heap, ans_numerator, ans_denominator);
}

static scm_obj_t oprtr_add(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  int base_count = (lhs_count > rhs_count) ? lhs_count : rhs_count;
  if (lhs_sign == rhs_sign) {  // (+,+) or (-,-) or (0,0)
    if (lhs_sign) {
      BN_TEMPORARY(ans);
      BN_ALLOC(ans, base_count + 1);
      bool overflow = bn_add(&ans, lhs, rhs);
      assert(overflow == false);
      bn_set_sign(&ans, lhs_sign);
      return bn_to_integer(heap, &ans);
    }
    return MAKEFIXNUM(0);
  }
  if (lhs_sign & rhs_sign) {  // (+,-) or (-,+)
    BN_TEMPORARY(ans);
    BN_ALLOC(ans, base_count);
    bool underflow = (lhs_sign > 0) ? bn_sub(&ans, lhs, rhs) : bn_sub(&ans, rhs, lhs);
    if (!underflow) {
      bn_set_sign(&ans, bn_get_count(&ans) != 0);
      return bn_to_integer(heap, &ans);
    }
    bn_flip2sc(&ans);
    bn_set_sign(&ans, -1);
    bn_norm(&ans);
    return bn_to_integer(heap, &ans);
  }
  return lhs_sign == 0 ? oprtr_norm_integer(heap, rhs) : oprtr_norm_integer(heap, lhs);  // (0,-) or (0,+) or (-,0) or (+,0)
}

static scm_obj_t oprtr_lognot(object_heap_t* heap, scm_bignum_t obj) {
  int obj_sign = bn_get_sign(obj);
  int obj_count = bn_get_count(obj);
  if (obj_sign) {
    BN_TEMPORARY(ans);
    BN_ALLOC(ans, obj_count + 1);
    if (obj_sign > 0) {  // (+)
      bn_lognot(&ans, obj);
      bn_flip2sc(&ans);
      bn_set_sign(&ans, -1);
      bn_norm(&ans);
      return bn_to_integer(heap, &ans);
    }
    BN_TEMPORARY(obj2sc);  // (-)
    BN_ALLOC_2SC(obj2sc, obj);
    bn_lognot(&ans, &obj2sc);
    bn_set_sign(&ans, 1);
    bn_norm(&ans);
    return bn_to_integer(heap, &ans);
  }
  return MAKEFIXNUM(-1);
}

static scm_obj_t oprtr_logand(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  int base_count = (lhs_count > rhs_count) ? lhs_count : rhs_count;
  BN_TEMPORARY(ans);
  BN_ALLOC(ans, base_count + 1);
  if (lhs_sign == rhs_sign) {  // (+,+) or (-,-) or (0,0)
    if (lhs_sign) {
      if (lhs_sign > 0) {  // (+,+)
        bn_logand(&ans, lhs, rhs, false, false);
        bn_set_sign(&ans, 1);
        bn_norm(&ans);
        return bn_to_integer(heap, &ans);
      }
      BN_TEMPORARY(lhs2sc);  // (-,-)
      BN_ALLOC_2SC(lhs2sc, lhs);
      BN_TEMPORARY(rhs2sc);
      BN_ALLOC_2SC(rhs2sc, rhs);
      bn_logand(&ans, &lhs2sc, &rhs2sc, true, true);
      bn_flip2sc(&ans);
      bn_set_sign(&ans, -1);
      bn_norm(&ans);
      return bn_to_integer(heap, &ans);
    }
    return MAKEFIXNUM(0);  // (0,0)
  }
  if (lhs_sign & rhs_sign) {  // (+,-) or (-,+)
    if (lhs_sign < 0) {
      BN_TEMPORARY(lhs2sc);  // (-,+)
      BN_ALLOC_2SC(lhs2sc, lhs);
      bn_logand(&ans, &lhs2sc, rhs, true, false);
    } else {  // (+,-)
      BN_TEMPORARY(rhs2sc);
      BN_ALLOC_2SC(rhs2sc, rhs);
      bn_logand(&ans, lhs, &rhs2sc, false, true);
    }
    bn_set_sign(&ans, 1);
    bn_norm(&ans);
    return bn_to_integer(heap, &ans);
  }
  return MAKEFIXNUM(0);  // (0,-) or (0,+) or (-,0) or (+,0)
}

static scm_obj_t oprtr_logior(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  int base_count = (lhs_count > rhs_count) ? lhs_count : rhs_count;
  BN_TEMPORARY(ans);
  BN_ALLOC(ans, base_count + 1);
  if (lhs_sign == rhs_sign) {  // (+,+) or (-,-) or (0,0)
    if (lhs_sign) {
      if (lhs_sign > 0) {  // (+,+)
        bn_logior(&ans, lhs, rhs, false, false);
        bn_set_sign(&ans, 1);
        bn_norm(&ans);
        return bn_to_integer(heap, &ans);
      }
      BN_TEMPORARY(lhs2sc);  // (-,-)
      BN_ALLOC_2SC(lhs2sc, lhs);
      BN_TEMPORARY(rhs2sc);
      BN_ALLOC_2SC(rhs2sc, rhs);
      bn_logior(&ans, &lhs2sc, &rhs2sc, true, true);
      bn_flip2sc(&ans);
      bn_set_sign(&ans, -1);
      bn_norm(&ans);
      return bn_to_integer(heap, &ans);
    }
    return MAKEFIXNUM(0);  // (0,0)
  }
  if (lhs_sign & rhs_sign) {  // (+,-) or (-,+)
    if (lhs_sign < 0) {
      BN_TEMPORARY(lhs2sc);  // (-,+)
      BN_ALLOC_2SC(lhs2sc, lhs);
      bn_logior(&ans, &lhs2sc, rhs, true, false);
    } else {  // (+,-)
      BN_TEMPORARY(rhs2sc);
      BN_ALLOC_2SC(rhs2sc, rhs);
      bn_logior(&ans, lhs, &rhs2sc, false, true);
    }
    bn_flip2sc(&ans);
    bn_set_sign(&ans, -1);
    bn_norm(&ans);
    return bn_to_integer(heap, &ans);
  }
  return (lhs_sign == 0) ? oprtr_norm_integer(heap, rhs) : oprtr_norm_integer(heap, lhs);  // (0,-) or (0,+) or (-,0) or (+,0)
}

static scm_obj_t oprtr_logxor(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  int base_count = (lhs_count > rhs_count) ? lhs_count : rhs_count;
  BN_TEMPORARY(ans);
  BN_ALLOC(ans, base_count + 1);
  if (lhs_sign == rhs_sign) {  // (+,+) or (-,-) or (0,0)
    if (lhs_sign) {
      if (lhs_sign > 0) {  // (+,+)
        bn_logxor(&ans, lhs, rhs, false, false);
        bn_set_sign(&ans, 1);
        bn_norm(&ans);
        return bn_to_integer(heap, &ans);
      }
      BN_TEMPORARY(lhs2sc);  // (-,-)
      BN_ALLOC_2SC(lhs2sc, lhs);
      BN_TEMPORARY(rhs2sc);
      BN_ALLOC_2SC(rhs2sc, rhs);
      bn_logxor(&ans, &lhs2sc, &rhs2sc, true, true);
      bn_set_sign(&ans, 1);
      bn_norm(&ans);
      return bn_to_integer(heap, &ans);
    }
    return MAKEFIXNUM(0);  // (0,0)
  }
  if (lhs_sign & rhs_sign) {  // (+,-) or (-,+)
    if (lhs_sign < 0) {
      BN_TEMPORARY(lhs2sc);  // (-,+)
      BN_ALLOC_2SC(lhs2sc, lhs);
      bn_logxor(&ans, &lhs2sc, rhs, true, false);
    } else {  // (+,-)
      BN_TEMPORARY(rhs2sc);
      BN_ALLOC_2SC(rhs2sc, rhs);
      bn_logxor(&ans, lhs, &rhs2sc, false, true);
    }
    bn_flip2sc(&ans);
    bn_set_sign(&ans, -1);
    bn_norm(&ans);
    return bn_to_integer(heap, &ans);
  }
  return (lhs_sign == 0) ? oprtr_norm_integer(heap, rhs) : oprtr_norm_integer(heap, lhs);  // (0,-) or (0,+) or (-,0) or (+,0)
}

static scm_obj_t oprtr_logash(object_heap_t* heap, scm_bignum_t lhs, int shift) {
  if (shift == 0) return lhs;
  if (shift > 0) {
    int lhs_count = bn_get_count(lhs);
    int ans_count = lhs_count + (shift + (DIGIT_BIT - 1)) / DIGIT_BIT;
    BN_TEMPORARY(ans);
    BN_ALLOC(ans, ans_count);
    memset(ans.elts, 0, sizeof(digit_t) * ans_count);
    memcpy(ans.elts, lhs->elts, sizeof(digit_t) * lhs_count);
    bn_set_sign(&ans, bn_get_sign(lhs));
    bn_shift_left(&ans, shift);
    bn_norm(&ans);
    return bn_to_integer(heap, &ans);
  } else {
    int lhs_sign = bn_get_sign(lhs);
    if (lhs_sign > 0) {
      int lhs_count = bn_get_count(lhs);
      BN_TEMPORARY(ans);
      BN_ALLOC(ans, lhs_count);
      memcpy(ans.elts, lhs->elts, sizeof(digit_t) * lhs_count);
      bn_set_sign(&ans, 1);
      bn_shift_right(&ans, -shift);
      bn_norm(&ans);
      return bn_to_integer(heap, &ans);
    } else {
      BN_TEMPORARY(ans);
      BN_ALLOC_2SC(ans, lhs);
      bn_shift_right_2sc(&ans, -shift);
      bn_flip2sc(&ans);
      bn_set_sign(&ans, -1);
      bn_norm(&ans);
      return bn_to_integer(heap, &ans);
    }
  }
}

static scm_obj_t oprtr_add(object_heap_t* heap, scm_rational_t lhs, scm_rational_t rhs) {
  scm_obj_t deno = arith_mul(heap, lhs->deno, rhs->deno);
  scm_obj_t nume = arith_add(heap, arith_mul(heap, lhs->nume, rhs->deno), arith_mul(heap, lhs->deno, rhs->nume));
  return oprtr_reduce(heap, nume, deno);
}

static scm_obj_t oprtr_sub(object_heap_t* heap, scm_rational_t lhs, scm_rational_t rhs) {
  scm_obj_t deno = arith_mul(heap, lhs->deno, rhs->deno);
  scm_obj_t nume = arith_sub(heap, arith_mul(heap, lhs->nume, rhs->deno), arith_mul(heap, lhs->deno, rhs->nume));
  return oprtr_reduce(heap, nume, deno);
}

static scm_obj_t oprtr_mul(object_heap_t* heap, scm_rational_t lhs, scm_rational_t rhs) {
  scm_obj_t nume = arith_mul(heap, lhs->nume, rhs->nume);
  scm_obj_t deno = arith_mul(heap, lhs->deno, rhs->deno);
  return oprtr_reduce(heap, nume, deno);
}

static scm_obj_t oprtr_div(object_heap_t* heap, scm_rational_t lhs, scm_rational_t rhs) {
  scm_obj_t nume = arith_mul(heap, lhs->nume, rhs->deno);
  scm_obj_t deno = arith_mul(heap, lhs->deno, rhs->nume);
  return oprtr_reduce(heap, nume, deno);
}

static scm_obj_t oprtr_sub(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int lhs_count = bn_get_count(lhs);
  int rhs_count = bn_get_count(rhs);
  int base_count = (lhs_count > rhs_count) ? lhs_count : rhs_count;
  if (lhs_sign == rhs_sign) {  // (+,+) or (-,-) or (0,0)
    if (lhs_sign) {
      BN_TEMPORARY(ans);
      BN_ALLOC(ans, base_count);
      bool underflow = (lhs_sign > 0) ? bn_sub(&ans, lhs, rhs) : bn_sub(&ans, rhs, lhs);
      if (!underflow) {
        bn_set_sign(&ans, bn_get_count(&ans) != 0);
        return bn_to_integer(heap, &ans);
      }
      bn_flip2sc(&ans);
      bn_set_sign(&ans, -1);
      bn_norm(&ans);
      return bn_to_integer(heap, &ans);
    }  // (0,0)
    return MAKEFIXNUM(0);
  }
  if (lhs_sign & rhs_sign) {  // (+,-) or (-,+)
    BN_TEMPORARY(ans);
    BN_ALLOC(ans, base_count + 1);
    bool overflow = bn_add(&ans, lhs, rhs);
    assert(overflow == false);
    bn_set_sign(&ans, lhs_sign);
    return bn_to_integer(heap, &ans);
  } else {                // (0,-) or (0,+) or (-,0) or (+,0)
    if (lhs_sign == 0) {  // (0,-) or (0,+)
      BN_TEMPORARY(ans);
      BN_ALLOC(ans, rhs_count);
      bn_set_sign(&ans, -rhs_sign);
      memcpy(ans.elts, rhs->elts, sizeof(digit_t) * rhs_count);
      return bn_to_integer(heap, &ans);
    }  // (-,0) or (+,0)
    return oprtr_norm_integer(heap, lhs);
  }
}

static scm_obj_t oprtr_mul(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int sign = 0;
  if (lhs_sign == rhs_sign) {  // (+,+) (-,-) (0,0)
    if (lhs_sign)
      sign = 1;
    else
      return MAKEFIXNUM(0);
  } else if (lhs_sign & rhs_sign) {  // (+,-) (-,+)
    sign = -1;
  } else {  // (0,-) (0,+) (-,0) (+,0)
    return MAKEFIXNUM(0);
  }
  BN_TEMPORARY(ans);
  int ans_count = bn_get_count(lhs) + bn_get_count(rhs);
  BN_ALLOC(ans, ans_count);
  bn_mul(&ans, lhs, rhs);
  bn_set_sign(&ans, sign);
  return bn_to_integer(heap, &ans);
}

static scm_obj_t oprtr_quotient(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int sign = 0;
  if (lhs_sign == rhs_sign) {  // (+,+) (-,-) (0,0)
    if (lhs_sign)
      sign = 1;
    else
      assert(false);                 // division by zero
  } else if (lhs_sign & rhs_sign) {  // (+,-) (-,+)
    sign = -1;
  } else {  // (0,-) (0,+) (-,0) (+,0)
    if (rhs_sign)
      return MAKEFIXNUM(0);
    else
      assert(false);  // division by zero
  }
  int test = bn_cmp(lhs, rhs);
  if (test < 0) return MAKEFIXNUM(0);
  if (test == 0) {
    return sign > 0 ? MAKEFIXNUM(1) : MAKEFIXNUM(-1);
  }
  BN_TEMPORARY(quotient);
  int count = bn_get_count(lhs);
  BN_ALLOC(quotient, count);
  bn_div_ans_t ans;
  ans.quotient = &quotient;
  ans.remainder = scm_false;
  bn_div(heap, &ans, lhs, rhs);
  bn_set_sign(&quotient, sign);
  bn_norm(&quotient);
  return bn_to_integer(heap, &quotient);
}

static scm_obj_t oprtr_remainder(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  int lhs_sign = bn_get_sign(lhs);
  int rhs_sign = bn_get_sign(rhs);
  int sign = 0;
  if (lhs_sign == rhs_sign) {  // (+,+) (-,-) (0,0)
    if (lhs_sign)
      sign = lhs_sign;
    else
      assert(false);                 // division by zero
  } else if (lhs_sign & rhs_sign) {  // (+,-) (-,+)
    sign = lhs_sign;
  } else {  // (0,-) (0,+) (-,0) (+,0)
    if (rhs_sign)
      return MAKEFIXNUM(0);
    else
      assert(false);  // division by zero
  }
  int test = bn_cmp(lhs, rhs);
  if (test < 0) return oprtr_norm_integer(heap, lhs);
  if (test == 0) return MAKEFIXNUM(0);
  BN_TEMPORARY(remainder);
  int count = bn_get_count(lhs);
  BN_ALLOC(remainder, count);
  bn_div_ans_t ans;
  ans.quotient = scm_false;
  ans.remainder = &remainder;
  bn_div(heap, &ans, lhs, rhs);
  bn_set_sign(&remainder, sign);
  bn_norm(&remainder);
  return bn_to_integer(heap, &remainder);
}

static scm_obj_t oprtr_modulo(object_heap_t* heap, scm_bignum_t lhs, scm_bignum_t rhs) {
  scm_obj_t modulo = oprtr_remainder(heap, lhs, rhs);
  if (modulo == MAKEFIXNUM(0)) return modulo;
  if (n_negative_pred(rhs) + n_negative_pred(modulo) == 1) return arith_add(heap, modulo, rhs);
  return modulo;
}

static scm_obj_t oprtr_expt(object_heap_t* heap, scm_obj_t lhs, scm_fixnum_t rhs) {
  intptr_t n = FIXNUM(rhs);
  if (n == 0) return MAKEFIXNUM(1);
  if (n == 1) return lhs;
  if (n < 0) return arith_inverse(heap, oprtr_expt(heap, lhs, MAKEFIXNUM(-n)));
  if (!COMPLEXP(lhs) && n_negative_pred(lhs)) {
    scm_obj_t ans = oprtr_expt(heap, arith_negate(heap, lhs), rhs);
    if (n & 1) return arith_negate(heap, ans);
    return ans;
  }
  if (lhs == MAKEFIXNUM(0)) return lhs;
  if (lhs == MAKEFIXNUM(1)) return lhs;
  if (lhs == MAKEFIXNUM(2)) {
    if (n + 1 <= FIXNUM_BITS - 1) return MAKEFIXNUM((uintptr_t)1 << n);
    int count = ((n + 1) + (DIGIT_BIT - 1)) / DIGIT_BIT;
    scm_bignum_t ans = make_bignum(heap, count);
    memset(ans->elts, 0, sizeof(digit_t) * count);
    bn_set_sign(ans, 1);
    ans->elts[count - 1] = (digit_t)1 << (n & (DIGIT_BIT - 1));
    return ans;
  }
  if (RATIONALP(lhs)) {
    return oprtr_reduce(heap, oprtr_expt(heap, ((scm_rational_t)lhs)->nume, rhs), oprtr_expt(heap, ((scm_rational_t)lhs)->deno, rhs));
  }
  scm_obj_t ans = MAKEFIXNUM(1);
  while (true) {
    if (n & 1) {
      if (ans == MAKEFIXNUM(1))
        ans = lhs;
      else
        ans = arith_mul(heap, ans, lhs);
      if (n == 1) return ans;
    }
    lhs = arith_mul(heap, lhs, lhs);
    n >>= 1;
  }
}

bool number_pred(scm_obj_t obj) {
  if (FIXNUMP(obj)) return true;
  if (CELLP(obj)) {
    int type = HDR(obj) & HDR_TYPE_MASKBITS;
    return (type == scm_hdr_flonum) | (type == scm_hdr_bignum) | (type == scm_hdr_rational) | (type == scm_hdr_complex);
  }
  return false;
}

bool integer_pred(scm_obj_t obj) {
  if (FIXNUMP(obj) || BIGNUMP(obj)) return true;
  if (RATIONALP(obj)) return false;
  if (FLONUMP(obj)) {
    scm_flonum_t flonum = (scm_flonum_t)obj;
    if (isinf(flonum->value)) return false;
    if (isnan(flonum->value)) return false;
    return flonum->value == round(flonum->value);
  }
  return false;
}

bool integer_valued_pred(scm_obj_t obj) {
  if (integer_pred(obj)) return true;
  if (COMPLEXP(obj)) {
    return n_zero_pred(((scm_complex_t)obj)->imag) && integer_valued_pred(((scm_complex_t)obj)->real);
  }
  return false;
}

bool rational_pred(scm_obj_t obj) {
  if (FIXNUMP(obj) || BIGNUMP(obj) || RATIONALP(obj)) return true;
  if (FLONUMP(obj)) {
    scm_flonum_t flonum = (scm_flonum_t)obj;
    if (isinf(flonum->value)) return false;
    if (isnan(flonum->value)) return false;
    return true;
  }
  return false;
}

bool rational_valued_pred(scm_obj_t obj) {
  if (rational_pred(obj)) return true;
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    return n_zero_pred(cn->imag) && rational_pred(cn->real);
  }
  return false;
}

bool real_pred(scm_obj_t obj) {
  if (FIXNUMP(obj) || FLONUMP(obj) || BIGNUMP(obj) || RATIONALP(obj)) return true;
  return false;
}

bool n_finite_pred(scm_obj_t obj) {
  assert(real_pred(obj));
  if (FLONUMP(obj)) {
    scm_flonum_t flonum = (scm_flonum_t)obj;
    if (isinf(flonum->value)) return false;
    if (isnan(flonum->value)) return false;
  }
  return true;
}

bool real_valued_pred(scm_obj_t obj) {
  if (real_pred(obj)) return true;
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    return n_zero_pred(cn->imag);
  }
  return false;
}

bool exact_integer_pred(scm_obj_t obj) {
  if (FIXNUMP(obj) || BIGNUMP(obj)) return true;
  return false;
}

bool exact_non_negative_integer_pred(scm_obj_t obj) {
  if (FIXNUMP(obj)) return FIXNUM(obj) >= 0;
  if (BIGNUMP(obj)) return bn_get_sign((scm_bignum_t)obj) >= 0;
  return false;
}

bool exact_positive_integer_pred(scm_obj_t obj) {
  if (FIXNUMP(obj)) return FIXNUM(obj) > 0;
  if (BIGNUMP(obj)) return bn_get_sign((scm_bignum_t)obj) > 0;
  return false;
}

bool n_exact_pred(scm_obj_t obj) {
  if (FIXNUMP(obj) || BIGNUMP(obj) || RATIONALP(obj)) return true;
  if (FLONUMP(obj)) return false;
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    return n_exact_pred(cn->real) && n_exact_pred(cn->imag);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

bool n_zero_pred(scm_obj_t obj) {
  if (FIXNUMP(obj)) return obj == MAKEFIXNUM(0);
  if (FLONUMP(obj)) return ((scm_flonum_t)obj)->value == 0.0;
  if (BIGNUMP(obj)) {
    assert(bn_get_sign((scm_bignum_t)obj) != 0);
    return false;
  }
  if (RATIONALP(obj)) {
    assert(n_zero_pred(((scm_rational_t)obj)->nume) == false);
    return false;
  }
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    return n_zero_pred(cn->real) && n_zero_pred(cn->imag);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

bool n_negative_pred(scm_obj_t obj) {
  assert(real_valued_pred(obj));
  if (FIXNUMP(obj)) return FIXNUM(obj) < 0;
  if (BIGNUMP(obj)) return bn_get_sign((scm_bignum_t)obj) < 0;
  if (FLONUMP(obj)) return ((scm_flonum_t)obj)->value < 0.0;
  if (RATIONALP(obj)) return n_negative_pred(((scm_rational_t)obj)->nume);
  if (COMPLEXP(obj)) return n_negative_pred(((scm_complex_t)obj)->real);
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

bool n_positive_pred(scm_obj_t obj) {
  assert(real_valued_pred(obj));
  if (FIXNUMP(obj)) return FIXNUM(obj) > 0;
  if (BIGNUMP(obj)) return bn_get_sign((scm_bignum_t)obj) > 0;
  if (FLONUMP(obj)) return ((scm_flonum_t)obj)->value > 0.0;
  if (RATIONALP(obj)) return n_positive_pred(((scm_rational_t)obj)->nume);
  if (COMPLEXP(obj)) return n_positive_pred(((scm_complex_t)obj)->real);
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

bool n_even_pred(scm_obj_t obj) {
  assert(integer_valued_pred(obj));
  if (FIXNUMP(obj)) return (FIXNUM(obj) & 1) == 0;
  if (BIGNUMP(obj)) return (((scm_bignum_t)obj)->elts[0] & 1) == 0;
  if (FLONUMP(obj)) {
    double value = ((scm_flonum_t)obj)->value;
    return (value * 0.5 == floor(value * 0.5));
  }
  if (COMPLEXP(obj)) return n_even_pred(((scm_complex_t)obj)->real);
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

bool n_equal_pred(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
start_again:
  if (FIXNUMP(lhs)) {
  fixnum_again:
    if (FIXNUMP(rhs)) {  // fixnum | fixnum
      return FIXNUM(lhs) == FIXNUM(rhs);
    }
    if (FLONUMP(rhs)) {  // fixnum | flonum
      return (double)FIXNUM(lhs) == ((scm_flonum_t)rhs)->value;
    }
    if (BIGNUMP(rhs)) {  // fixnum | bignum
      return false;
    }
    if (RATIONALP(rhs)) {  // fixnum | rational
      return false;
    }
    if (COMPLEXP(rhs)) {  // fixnum | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto fixnum_again;
      }
      return false;
    }
  }
  if (FLONUMP(lhs)) {
  flonum_again:
    if (FIXNUMP(rhs)) {  // flonum | fixnum
      return ((scm_flonum_t)lhs)->value == (double)FIXNUM(rhs);
    }
    if (FLONUMP(rhs)) {  // flonum | flonum
      return ((scm_flonum_t)lhs)->value == ((scm_flonum_t)rhs)->value;
    }
    if (BIGNUMP(rhs)) {  // flonum | bignum
      if (((scm_flonum_t)lhs)->value == bignum_to_double((scm_bignum_t)rhs)) {
        return n_compare(heap, lhs, cnvt_to_exact(heap, rhs)) == 0;
      }
      return false;
    }
    if (RATIONALP(rhs)) {  // flonum | rational
      return ((scm_flonum_t)lhs)->value == rational_to_double((scm_rational_t)rhs);
    }
    if (COMPLEXP(rhs)) {  // flonum | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto flonum_again;
      }
      return false;
    }
  }
  if (BIGNUMP(lhs)) {
  bignum_again:
    if (FIXNUMP(rhs)) {  // bignum | fixnum
      return false;
    }
    if (FLONUMP(rhs)) {  // bignum | flonum
      if (bignum_to_double((scm_bignum_t)lhs) == ((scm_flonum_t)rhs)->value) {
        return n_compare(heap, cnvt_to_exact(heap, lhs), rhs) == 0;
      }
      return false;
    }
    if (BIGNUMP(rhs)) {  // bignum | bignum
      return bn_cmp((scm_bignum_t)lhs, (scm_bignum_t)rhs) == 0;
    }
    if (RATIONALP(rhs)) {  // bignum | rational
      return false;
    }
    if (COMPLEXP(rhs)) {  // bignum | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto bignum_again;
      }
      return false;
    }
  }
  if (RATIONALP(lhs)) {
  rational_again:
    if (FIXNUMP(rhs)) {  // rational | fixnum
      return false;
    }
    if (FLONUMP(rhs)) {  // rational | flonum
      return rational_to_double((scm_rational_t)lhs) == ((scm_flonum_t)rhs)->value;
    }
    if (BIGNUMP(rhs)) {  // rational | bignum
      return false;
    }
    if (RATIONALP(rhs)) {  // rational | rational
      if (n_equal_pred(heap, ((scm_rational_t)lhs)->nume, ((scm_rational_t)rhs)->nume)) {
        if (n_equal_pred(heap, ((scm_rational_t)lhs)->deno, ((scm_rational_t)rhs)->deno)) {
          return true;
        }
      }
      return false;
    }
    if (COMPLEXP(rhs)) {  // rational | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto rational_again;
      }
      return false;
    }
  }
  if (COMPLEXP(lhs)) {
    if (n_zero_pred(((scm_complex_t)lhs)->imag)) {
      lhs = ((scm_complex_t)lhs)->real;
      goto start_again;
    }
    if (FIXNUMP(rhs)) {  // complex | fixnum
      return false;
    }
    if (FLONUMP(rhs)) {  // complex | flonum
      return false;
    }
    if (BIGNUMP(rhs)) {  // complex | bignum
      return false;
    }
    if (RATIONALP(rhs)) {  // complex | rational
      return false;
    }
    if (COMPLEXP(rhs)) {  // complex | complex
      if (n_equal_pred(heap, ((scm_complex_t)lhs)->real, ((scm_complex_t)rhs)->real)) {
        if (n_equal_pred(heap, ((scm_complex_t)lhs)->imag, ((scm_complex_t)rhs)->imag)) {
          return true;
        }
      }
      return false;
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

bool n_exact_equal_pred(scm_obj_t lhs, scm_obj_t rhs) {
  if (FIXNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // fixnum | fixnum
      return FIXNUM(lhs) == FIXNUM(rhs);
    }
    if (BIGNUMP(rhs)) {  // fixnum | bignum
      return false;
    }
    if (RATIONALP(rhs)) {  // fixnum | rational
      return false;
    }
    if (COMPLEXP(rhs)) {  // fixnum | complex
      assert(n_exact_pred(rhs));
      return false;
    }
    if (FLONUMP(rhs)) {  // fixnum | flonum
      assert(false);
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum | fixnum
      return false;
    }
    if (BIGNUMP(rhs)) {  // bignum | bignum
      return bn_cmp((scm_bignum_t)lhs, (scm_bignum_t)rhs) == 0;
    }
    if (RATIONALP(rhs)) {  // bignum | rational
      return false;
    }
    if (COMPLEXP(rhs)) {  // bignum | complex
      assert(n_exact_pred(rhs));
      return false;
    }
    if (FLONUMP(rhs)) {  // bignum | flonum
      assert(false);
    }
  }
  if (RATIONALP(lhs)) {
    if (FIXNUMP(rhs)) {  // rational | fixnum
      return false;
    }
    if (BIGNUMP(rhs)) {  // rational | bignum
      return false;
    }
    if (RATIONALP(rhs)) {  // rational | rational
      if (n_exact_equal_pred(((scm_rational_t)lhs)->nume, ((scm_rational_t)rhs)->nume)) {
        if (n_exact_equal_pred(((scm_rational_t)lhs)->deno, ((scm_rational_t)rhs)->deno)) {
          return true;
        }
      }
      return false;
    }
    if (COMPLEXP(rhs)) {  // rational | complex
      assert(n_exact_pred(rhs));
      return false;
    }
    if (FLONUMP(rhs)) {  // rational | flonum
      assert(false);
    }
  }
  if (COMPLEXP(lhs)) {
    assert(n_exact_pred(lhs));
    if (FIXNUMP(rhs)) {  // complex | fixnum
      return false;
    }
    if (BIGNUMP(rhs)) {  // complex | bignum
      return false;
    }
    if (RATIONALP(rhs)) {  // complex | rational
      return false;
    }
    if (COMPLEXP(rhs)) {  // complex | complex
      assert(n_exact_pred(rhs));
      if (n_exact_equal_pred(((scm_complex_t)lhs)->real, ((scm_complex_t)rhs)->real)) {
        if (n_exact_equal_pred(((scm_complex_t)lhs)->imag, ((scm_complex_t)rhs)->imag)) {
          return true;
        }
      }
      return false;
    }
    if (FLONUMP(rhs)) {  // complex | flonum
      assert(false);
      return false;
    }
  }
  if (FLONUMP(lhs)) {
    assert(false);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

bool n_inexact_equal_pred(scm_obj_t lhs, scm_obj_t rhs) {
  if (FLONUMP(lhs)) {
  flonum_again:
    if (FLONUMP(rhs)) {  // flonum | flonum
      return ((scm_flonum_t)lhs)->value == ((scm_flonum_t)rhs)->value;
    }
    if (COMPLEXP(rhs)) {  // flonum | complex
      assert(!n_exact_pred(rhs));
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto flonum_again;
      }
      return false;
    }
    if (FIXNUMP(rhs)) {  // flonum | fixnum
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // flonum | bignum
      assert(false);
    }
    if (RATIONALP(rhs)) {  // flonum | rational
      assert(false);
    }
    assert(false);
  }
  if (COMPLEXP(lhs)) {
    assert(!n_exact_pred(lhs));
    if (n_zero_pred(((scm_complex_t)lhs)->imag)) {
      lhs = ((scm_complex_t)lhs)->real;
      goto flonum_again;
    }
    if (FLONUMP(rhs)) {  // complex | flonum
      return false;
    }
    if (COMPLEXP(rhs)) {  // complex | complex
      assert(!n_exact_pred(rhs));
      if (n_inexact_equal_pred(((scm_complex_t)lhs)->real, ((scm_complex_t)rhs)->real)) {
        if (n_inexact_equal_pred(((scm_complex_t)lhs)->imag, ((scm_complex_t)rhs)->imag)) {
          return true;
        }
      }
      return false;
    }
    if (FIXNUMP(rhs)) {  // complex | fixnum
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // complex | bignum
      assert(false);
    }
    if (RATIONALP(rhs)) {  // complex | rational
      assert(false);
    }
    assert(false);
  }
  if (FIXNUMP(lhs)) {
    assert(false);
  }
  if (BIGNUMP(lhs)) {
    assert(false);
  }
  if (RATIONALP(lhs)) {
    assert(false);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

int n_compare(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
start_again:
  if (FIXNUMP(lhs)) {
  fixnum_again:
    if (FIXNUMP(rhs)) {  // fixnum | fixnum
      intptr_t n = FIXNUM(lhs) - FIXNUM(rhs);
      if (n == 0) return 0;
      return n > 0 ? 1 : -1;
    }
    if (FLONUMP(rhs)) {  // fixnum | flonum
      double d = (double)FIXNUM(lhs) - ((scm_flonum_t)rhs)->value;
      if (d == 0.0) return 0;
      return d > 0.0 ? 1 : -1;
    }
    if (BIGNUMP(rhs)) {  // fixnum | bignum
      return -bn_get_sign((scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // fixnum | rational
      scm_obj_t nume = ((scm_rational_t)rhs)->nume;
      scm_obj_t deno = ((scm_rational_t)rhs)->deno;
      return n_compare(heap, arith_mul(heap, lhs, deno), nume);
    }
    if (COMPLEXP(rhs)) {  // fixnum | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto fixnum_again;
      }
    }
    assert(false);
    return 0;
  }
  if (FLONUMP(lhs)) {
  flonum_again:
    if (FIXNUMP(rhs)) {  // flonum | fixnum
      double d = ((scm_flonum_t)lhs)->value - (double)FIXNUM(rhs);
      if (d == 0.0) return 0;
      return d > 0.0 ? 1 : -1;
    }
    if (FLONUMP(rhs)) {  // flonum | flonum
      double d = ((scm_flonum_t)lhs)->value - ((scm_flonum_t)rhs)->value;
      if (d == 0.0) return 0;
      return d > 0.0 ? 1 : -1;
    }
    if (BIGNUMP(rhs)) {  // flonum | bignum
      if (integer_pred(lhs)) return n_compare(heap, cnvt_to_exact(heap, lhs), rhs);
      return (((scm_flonum_t)lhs)->value > bignum_to_double((scm_bignum_t)rhs)) ? 1 : -1;
    }
    if (RATIONALP(rhs)) {  // flonum | rational
      double d = ((scm_flonum_t)lhs)->value - rational_to_double((scm_rational_t)rhs);
      if (d == 0.0) return 0;
      return d > 0.0 ? 1 : -1;
    }
    if (COMPLEXP(rhs)) {  // flonum | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto flonum_again;
      }
    }
    assert(false);
    return 0;
  }
  if (BIGNUMP(lhs)) {
  bignum_again:
    if (FIXNUMP(rhs)) {  // bignum | fixnum
      return bn_get_sign((scm_bignum_t)lhs);
    }
    if (FLONUMP(rhs)) {  // bignum | flonum
      if (integer_pred(rhs)) return n_compare(heap, lhs, cnvt_to_exact(heap, rhs));
      return (bignum_to_double((scm_bignum_t)lhs) > ((scm_flonum_t)rhs)->value) ? 1 : -1;
    }
    if (BIGNUMP(rhs)) {  // bignum | bignum
      int lhs_sign = bn_get_sign((scm_bignum_t)lhs);
      int rhs_sign = bn_get_sign((scm_bignum_t)rhs);
      if (lhs_sign == rhs_sign) {
        int n = bn_cmp((scm_bignum_t)lhs, (scm_bignum_t)rhs);
        return (lhs_sign < 0) ? -n : n;
      }
      return lhs_sign > rhs_sign ? 1 : -1;
    }
    if (RATIONALP(rhs)) {  // bignum | rational
      scm_obj_t nume = ((scm_rational_t)rhs)->nume;
      scm_obj_t deno = ((scm_rational_t)rhs)->deno;
      return n_compare(heap, arith_mul(heap, lhs, deno), nume);
    }
    if (COMPLEXP(rhs)) {  // bignum | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto bignum_again;
      }
    }
    assert(false);
    return 0;
  }
  if (RATIONALP(lhs)) {
  rational_again:
    scm_obj_t nume = ((scm_rational_t)lhs)->nume;
    scm_obj_t deno = ((scm_rational_t)lhs)->deno;
    if (FIXNUMP(rhs)) {  // rational | fixnum
      return n_compare(heap, nume, arith_mul(heap, rhs, deno));
    }
    if (FLONUMP(rhs)) {  // rational | flonum
      double d = rational_to_double((scm_rational_t)lhs) - ((scm_flonum_t)rhs)->value;
      if (d == 0.0) return 0;
      return (d > 0.0) ? 1 : -1;
    }
    if (BIGNUMP(rhs)) {  // rational | bignum
      return n_compare(heap, nume, arith_mul(heap, rhs, deno));
    }
    if (RATIONALP(rhs)) {  // rational | rational
      scm_obj_t nume2 = ((scm_rational_t)rhs)->nume;
      scm_obj_t deno2 = ((scm_rational_t)rhs)->deno;
      return n_compare(heap, arith_mul(heap, nume, deno2), arith_mul(heap, nume2, deno));
    }
    if (COMPLEXP(rhs)) {  // rational | complex
      if (n_zero_pred(((scm_complex_t)rhs)->imag)) {
        rhs = ((scm_complex_t)rhs)->real;
        goto rational_again;
      }
    }
    assert(false);
    return 0;
  }
  if (COMPLEXP(lhs)) {
    if (n_zero_pred(((scm_complex_t)lhs)->imag)) {
      lhs = ((scm_complex_t)lhs)->real;
      goto start_again;
    }
    assert(false);
    return 0;
  }
  assert(false);
  return 0;
}

scm_obj_t arith_inverse(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    assert(FIXNUM(obj));
    if (FIXNUM(obj) > 0) {
      if (obj == MAKEFIXNUM(1)) return obj;
      return make_rational(heap, MAKEFIXNUM(1), obj);
    }
    if (obj == MAKEFIXNUM(-1)) return obj;
    return make_rational(heap, MAKEFIXNUM(-1), arith_negate(heap, obj));
  }
  if (FLONUMP(obj)) return make_flonum(heap, 1.0 / ((scm_flonum_t)obj)->value);
  if (BIGNUMP(obj)) {
    assert(bn_get_sign((scm_bignum_t)obj));
    if (bn_get_sign((scm_bignum_t)obj) > 0) {
      return make_rational(heap, MAKEFIXNUM(1), obj);
    }
    return make_rational(heap, MAKEFIXNUM(-1), arith_negate(heap, obj));
  }
  if (RATIONALP(obj)) {
    scm_rational_t rn = (scm_rational_t)obj;
    if (n_negative_pred(rn->nume) == false) {
      if (rn->nume == MAKEFIXNUM(1)) return oprtr_norm_integer(heap, rn->deno);
      return make_rational(heap, rn->deno, rn->nume);
    }
    if (rn->nume == MAKEFIXNUM(-1)) return arith_negate(heap, rn->deno);
    return make_rational(heap, arith_negate(heap, rn->deno), arith_negate(heap, rn->nume));
  }
  if (COMPLEXP(obj)) return arith_div(heap, MAKEFIXNUM(1), obj);
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_negate(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t n = FIXNUM(obj);
    if (n == FIXNUM_MIN) return intptr_to_integer(heap, -n);
    return MAKEFIXNUM(-n);
  }
  if (FLONUMP(obj)) return make_flonum(heap, -((scm_flonum_t)obj)->value);
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    bn = bn_dup(heap, bn);
    bn_set_sign(bn, -bn_get_sign(bn));
    return oprtr_norm_integer(heap, bn);
  }
  if (RATIONALP(obj)) {
    scm_rational_t rn = (scm_rational_t)obj;
    return make_rational(heap, arith_negate(heap, rn->nume), rn->deno);
  }
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    return make_complex(heap, arith_negate(heap, cn->real), arith_negate(heap, cn->imag));
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_bit_count(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t n = FIXNUM(obj);
    if (n > 0) {
      return MAKEFIXNUM(nbits(n));
    } else {
      return MAKEFIXNUM(~nbits(~n));
    }
  }
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    if (bn_get_sign(bn) > 0) {
      int n = 0;
      int len = bn_get_count(bn);
      for (int i = 0; i < len; i++) n += nbits(bn->elts[i]);
      return MAKEFIXNUM(n);
    } else {
      return MAKEFIXNUM(~FIXNUM(arith_bit_count(heap, oprtr_lognot(heap, bn))));
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_bit_length(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t n = FIXNUM(obj);
    if (n == 0) return MAKEFIXNUM(0);
    intptr_t n2 = (n < 0) ? ~n : n;
    return MAKEFIXNUM(DIGIT_BIT - nlz(n2));
  }
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    if (bn_get_sign(bn) > 0) return MAKEFIXNUM(bn_bitsize(bn));
    return arith_bit_length(heap, oprtr_lognot(heap, bn));
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_first_bit_set(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t n = FIXNUM(obj);
    if (n == 0) return MAKEFIXNUM(-1);
    int bit = 0;
    bit += ntz(n);
    return MAKEFIXNUM(bit);
  }
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    if (bn_get_sign(bn) > 0) {
      int bit = 0;
      for (int i = 0; i < bn_get_count(bn); i++) {
        digit_t n = bn->elts[i];
        if (n == 0) {
          bit += DIGIT_BIT;
          continue;
        }
        bit += ntz(n);
        return MAKEFIXNUM(bit);
      }
    } else {
      BN_TEMPORARY(obj2sc);
      BN_ALLOC_2SC(obj2sc, bn);
      int bit = 0;
      for (int i = 0; i < bn_get_count(&obj2sc); i++) {
        digit_t n = obj2sc.elts[i];
        if (n == 0) {
          bit += DIGIT_BIT;
          continue;
        }
        bit += ntz(n);
        return MAKEFIXNUM(bit);
      }
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_lognot(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t n = ~FIXNUM(obj);
    if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
    return intptr_to_bignum(heap, n);
  }
  if (BIGNUMP(obj)) {
    return oprtr_lognot(heap, (scm_bignum_t)obj);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_logand(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  if (FIXNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // fixnum & fixnum
      return MAKEFIXNUM(FIXNUM(lhs) & FIXNUM(rhs));
    }
    if (BIGNUMP(rhs)) {  // fixnum & bignum
      BN_TEMPORARY(bn);
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_logand(heap, (scm_bignum_t)rhs, &bn);
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum & fixnum
      BN_TEMPORARY(bn);
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_logand(heap, &bn, (scm_bignum_t)lhs);
    }
    if (BIGNUMP(rhs)) {  // bignum & bignum
      return oprtr_logand(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_logior(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  if (FIXNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // fixnum | fixnum
      return MAKEFIXNUM(FIXNUM(lhs) | FIXNUM(rhs));
    }
    if (BIGNUMP(rhs)) {  // fixnum | bignum
      BN_TEMPORARY(bn);
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_logior(heap, (scm_bignum_t)rhs, &bn);
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum | fixnum
      BN_TEMPORARY(bn);
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_logior(heap, &bn, (scm_bignum_t)lhs);
    }
    if (BIGNUMP(rhs)) {  // bignum | bignum
      return oprtr_logior(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_logxor(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  if (FIXNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // fixnum ^ fixnum
      return MAKEFIXNUM(FIXNUM(lhs) ^ FIXNUM(rhs));
    }
    if (BIGNUMP(rhs)) {  // fixnum ^ bignum
      BN_TEMPORARY(bn);
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_logxor(heap, (scm_bignum_t)rhs, &bn);
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum ^ fixnum
      BN_TEMPORARY(bn);
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_logxor(heap, &bn, (scm_bignum_t)lhs);
    }
    if (BIGNUMP(rhs)) {  // bignum ^ bignum
      return oprtr_logxor(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_logash(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  assert(FIXNUMP(rhs));
  intptr_t shift = FIXNUM(rhs);
  if (FIXNUMP(lhs)) {
    if (shift <= DIGIT_BIT) {
      signed_digit2x_t n = FIXNUM(lhs);
      if (shift > 0)
        n = n << shift;
      else
        n = n >> (-shift);
      if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
    }
    return oprtr_logash(heap, (scm_bignum_t)intptr_to_bignum(heap, FIXNUM(lhs)), shift);
  }
  if (BIGNUMP(lhs)) {
    scm_bignum_t bn = (scm_bignum_t)lhs;
    return oprtr_logash(heap, bn, shift);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_add(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  BN_TEMPORARY(bn);
  scm_obj_t real;
  scm_obj_t imag;

  if (FIXNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // fixnum + fixnum
      intptr_t n = FIXNUM(lhs) + FIXNUM(rhs);
      if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
      return intptr_to_bignum(heap, n);
    }
    if (FLONUMP(rhs)) {  // fixnum + flonum
      return make_flonum(heap, (double)FIXNUM(lhs) + ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // fixnum + bignum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_add(heap, (scm_bignum_t)rhs, &bn);
    }
    if (RATIONALP(rhs)) {  // fixnum + rational
      scm_rational_t rn = (scm_rational_t)rhs;
      return oprtr_reduce(heap, arith_add(heap, rn->nume, arith_mul(heap, rn->deno, lhs)), rn->deno);
    }
    if (COMPLEXP(rhs)) {  // fixnum + complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_add(heap, real, lhs), imag);
    }
  }
  if (FLONUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // flonum + fixnum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value + FIXNUM(rhs));
    }
    if (FLONUMP(rhs)) {  // flonum + flonum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value + ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // flonum + bignum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value + bignum_to_double((scm_bignum_t)rhs));
    }
    if (RATIONALP(rhs)) {  // flonum + rational
      return make_flonum(heap, ((scm_flonum_t)lhs)->value + rational_to_double((scm_rational_t)rhs));
    }
    if (COMPLEXP(rhs)) {  // flonum + complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_add(heap, real, lhs), cnvt_to_inexact(heap, imag));
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum + fixnum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_add(heap, &bn, (scm_bignum_t)lhs);
    }
    if (FLONUMP(rhs)) {  // bignum + flonum
      return make_flonum(heap, bignum_to_double((scm_bignum_t)lhs) + ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // bignum + bignum
      return oprtr_add(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // bignum + rational
      scm_rational_t rn = (scm_rational_t)rhs;
      return oprtr_reduce(heap, arith_add(heap, rn->nume, arith_mul(heap, rn->deno, lhs)), rn->deno);
    }
    if (COMPLEXP(rhs)) {  // bignum + complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_add(heap, real, lhs), imag);
    }
  }
  if (RATIONALP(lhs)) {
    if (FIXNUMP(rhs)) {  // rational + fixnum
      scm_rational_t rn = (scm_rational_t)lhs;
      return oprtr_reduce(heap, arith_add(heap, rn->nume, arith_mul(heap, rn->deno, rhs)), rn->deno);
    }
    if (FLONUMP(rhs)) {  // rational + flonum
      return make_flonum(heap, rational_to_double((scm_rational_t)lhs) + ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // rational + bignum
      scm_rational_t rn = (scm_rational_t)lhs;
      return oprtr_reduce(heap, arith_add(heap, rn->nume, arith_mul(heap, rn->deno, rhs)), rn->deno);
    }
    if (RATIONALP(rhs)) {  // rational + rational
      return oprtr_add(heap, (scm_rational_t)lhs, (scm_rational_t)rhs);
    }
    if (COMPLEXP(rhs)) {  // rational + complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_add(heap, real, lhs), imag);
    }
  }
  if (COMPLEXP(lhs)) {
    real = ((scm_complex_t)lhs)->real;
    imag = ((scm_complex_t)lhs)->imag;
    if (FIXNUMP(rhs)) {  // complex + fixnum
      return make_complex(heap, arith_add(heap, real, rhs), imag);
    }
    if (FLONUMP(rhs)) {  // complex + flonum
      return make_complex(heap, arith_add(heap, real, rhs), cnvt_to_inexact(heap, imag));
    }
    if (BIGNUMP(rhs)) {  // complex + bignum
      return make_complex(heap, arith_add(heap, real, rhs), imag);
    }
    if (RATIONALP(rhs)) {  // complex + rational
      return make_complex(heap, arith_add(heap, real, rhs), imag);
    }
    if (COMPLEXP(rhs)) {  // complex + complex
      real = arith_add(heap, real, ((scm_complex_t)rhs)->real);
      imag = arith_add(heap, imag, ((scm_complex_t)rhs)->imag);
      return oprtr_norm_complex(heap, real, imag);
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_sub(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  BN_TEMPORARY(bn);
  scm_obj_t real;
  scm_obj_t imag;
  if (FIXNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // fixnum - fixnum
      intptr_t n = FIXNUM(lhs) - FIXNUM(rhs);
      if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
      return intptr_to_bignum(heap, n);
    }
    if (FLONUMP(rhs)) {  // fixnum - flonum
      return make_flonum(heap, (double)FIXNUM(lhs) - ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // fixnum - bignum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_sub(heap, &bn, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // fixnum - rational
      scm_rational_t rn = (scm_rational_t)rhs;
      return oprtr_reduce(heap, arith_sub(heap, arith_mul(heap, rn->deno, lhs), rn->nume), rn->deno);
    }
    if (COMPLEXP(rhs)) {  // fixnum - complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_sub(heap, lhs, real), arith_negate(heap, imag));
    }
  }
  if (FLONUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // flonum - fixnum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value - (double)FIXNUM(rhs));
    }
    if (FLONUMP(rhs)) {  // flonum - flonum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value - ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // flonum - bignum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value - bignum_to_double((scm_bignum_t)rhs));
    }
    if (RATIONALP(rhs)) {  // flonum - rational
      return make_flonum(heap, ((scm_flonum_t)lhs)->value - rational_to_double((scm_rational_t)rhs));
    }
    if (COMPLEXP(rhs)) {  // flonum - complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_sub(heap, lhs, real), oprtr_inexact_negate(heap, imag));
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum - fixnum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_sub(heap, (scm_bignum_t)lhs, &bn);
    }
    if (FLONUMP(rhs)) {  // bignum - flonum
      return make_flonum(heap, bignum_to_double((scm_bignum_t)lhs) - ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // bignum - bignum
      return oprtr_sub(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // bignum - rational
      scm_rational_t rn = (scm_rational_t)rhs;
      return oprtr_reduce(heap, arith_sub(heap, arith_mul(heap, rn->deno, lhs), rn->nume), rn->deno);
    }
    if (COMPLEXP(rhs)) {  // bignum - complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_sub(heap, lhs, real), arith_negate(heap, imag));
    }
  }
  if (RATIONALP(lhs)) {
    if (FIXNUMP(rhs)) {  // rational - fixnum
      scm_rational_t rn = (scm_rational_t)lhs;
      return oprtr_reduce(heap, arith_sub(heap, rn->nume, arith_mul(heap, rn->deno, rhs)), rn->deno);
    }
    if (FLONUMP(rhs)) {  // rational - flonum
      return make_flonum(heap, rational_to_double((scm_rational_t)lhs) - ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // rational - bignum
      scm_rational_t rn = (scm_rational_t)lhs;
      return oprtr_reduce(heap, arith_sub(heap, rn->nume, arith_mul(heap, rn->deno, rhs)), rn->deno);
    }
    if (RATIONALP(rhs)) {  // rational - rational
      return oprtr_sub(heap, (scm_rational_t)lhs, (scm_rational_t)rhs);
    }
    if (COMPLEXP(rhs)) {  // rational - complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_sub(heap, lhs, real), arith_negate(heap, imag));
    }
  }
  if (COMPLEXP(lhs)) {
    real = ((scm_complex_t)lhs)->real;
    imag = ((scm_complex_t)lhs)->imag;
    if (FIXNUMP(rhs)) {  // complex - fixnum
      return make_complex(heap, arith_sub(heap, real, rhs), imag);
    }
    if (FLONUMP(rhs)) {  // complex - flonum
      return make_complex(heap, arith_sub(heap, real, rhs), cnvt_to_inexact(heap, imag));
    }
    if (BIGNUMP(rhs)) {  // complex - bignum
      return make_complex(heap, arith_sub(heap, real, rhs), imag);
    }
    if (RATIONALP(rhs)) {  // complex - rational
      return make_complex(heap, arith_sub(heap, real, rhs), imag);
    }
    if (COMPLEXP(rhs)) {  // complex - complex
      real = arith_sub(heap, real, ((scm_complex_t)rhs)->real);
      imag = arith_sub(heap, imag, ((scm_complex_t)rhs)->imag);
      return oprtr_norm_complex(heap, real, imag);
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_mul(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  BN_TEMPORARY(bn);
  scm_obj_t real;
  scm_obj_t imag;

  if (FIXNUMP(lhs)) {
    if (FIXNUM(lhs) == 0) return MAKEFIXNUM(0);
    if (FIXNUMP(rhs)) {  // fixnum * fixnum
      signed_digit2x_t n = (signed_digit2x_t)FIXNUM(lhs) * FIXNUM(rhs);
      if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM((signed_digit_t)n);
      return signed_digit2x_to_bignum(heap, n);
    }
    if (FLONUMP(rhs)) {  // fixnum * flonum
      return make_flonum(heap, (double)FIXNUM(lhs) * ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // fixnum * bignum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_mul(heap, (scm_bignum_t)rhs, &bn);
    }
    if (RATIONALP(rhs)) {  // fixnum * rational
      scm_rational_t rn = (scm_rational_t)rhs;
      if (rn->nume == MAKEFIXNUM(1)) return oprtr_reduce(heap, lhs, rn->deno);
      if (rn->nume == MAKEFIXNUM(-1)) return arith_negate(heap, oprtr_reduce(heap, lhs, rn->deno));
      return oprtr_reduce(heap, arith_mul(heap, rn->nume, lhs), rn->deno);
    }
    if (COMPLEXP(rhs)) {  // fixnum * complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_mul(heap, real, lhs), arith_mul(heap, imag, lhs));
    }
  }
  if (FLONUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // flonum * fixnum
      if (FIXNUM(rhs) == 0) return MAKEFIXNUM(0);
      return make_flonum(heap, ((scm_flonum_t)lhs)->value * FIXNUM(rhs));
    }
    if (FLONUMP(rhs)) {  // flonum * flonum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value * ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // flonum * bignum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value * bignum_to_double((scm_bignum_t)rhs));
    }
    if (RATIONALP(rhs)) {  // flonum * rational
      return make_flonum(heap, ((scm_flonum_t)lhs)->value * rational_to_double((scm_rational_t)rhs));
    }
    if (COMPLEXP(rhs)) {  // flonum * complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_mul(heap, real, lhs), arith_mul(heap, imag, lhs));
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum * fixnum
      if (FIXNUM(rhs) == 0) return MAKEFIXNUM(0);
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_mul(heap, &bn, (scm_bignum_t)lhs);
    }
    if (FLONUMP(rhs)) {  // bignum * flonum
      return make_flonum(heap, bignum_to_double((scm_bignum_t)lhs) * ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // bignum * bignum
      return oprtr_mul(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // bignum * rational
      scm_rational_t rn = (scm_rational_t)rhs;
      if (rn->nume == MAKEFIXNUM(1)) return oprtr_reduce(heap, lhs, rn->deno);
      if (rn->nume == MAKEFIXNUM(-1)) return arith_negate(heap, oprtr_reduce(heap, lhs, rn->deno));
      return oprtr_reduce(heap, arith_mul(heap, rn->nume, lhs), rn->deno);
    }
    if (COMPLEXP(rhs)) {  // bignum * complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_mul(heap, real, lhs), arith_mul(heap, imag, lhs));
    }
  }
  if (RATIONALP(lhs)) {
    if (FIXNUMP(rhs)) {  // rational * fixnum
      if (FIXNUM(rhs) == 0) return MAKEFIXNUM(0);
      scm_rational_t rn = (scm_rational_t)lhs;
      if (rn->nume == MAKEFIXNUM(1)) return oprtr_reduce(heap, rhs, rn->deno);
      if (rn->nume == MAKEFIXNUM(-1)) return arith_negate(heap, oprtr_reduce(heap, rhs, rn->deno));
      return oprtr_reduce(heap, arith_mul(heap, rn->nume, rhs), rn->deno);
    }
    if (FLONUMP(rhs)) {  // rational * flonum
      return make_flonum(heap, rational_to_double((scm_rational_t)lhs) * ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // rational * bignum
      scm_rational_t rn = (scm_rational_t)lhs;
      if (rn->nume == MAKEFIXNUM(1)) return oprtr_reduce(heap, rhs, rn->deno);
      if (rn->nume == MAKEFIXNUM(-1)) return arith_negate(heap, oprtr_reduce(heap, rhs, rn->deno));
      return oprtr_reduce(heap, arith_mul(heap, rn->nume, rhs), rn->deno);
    }
    if (RATIONALP(rhs)) {  // rational * rational
      return oprtr_mul(heap, (scm_rational_t)lhs, (scm_rational_t)rhs);
    }
    if (COMPLEXP(rhs)) {  // rational * complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      return make_complex(heap, arith_mul(heap, real, lhs), arith_mul(heap, imag, lhs));
    }
  }
  if (COMPLEXP(lhs)) {
    real = ((scm_complex_t)lhs)->real;
    imag = ((scm_complex_t)lhs)->imag;
    if (FIXNUMP(rhs)) {  // complex * fixnum
      if (FIXNUM(rhs) == 0) return MAKEFIXNUM(0);
      return make_complex(heap, arith_mul(heap, real, rhs), arith_mul(heap, imag, rhs));
    }
    if (FLONUMP(rhs)) {  // complex * flonum
      return make_complex(heap, arith_mul(heap, real, rhs), arith_mul(heap, imag, rhs));
    }
    if (BIGNUMP(rhs)) {  // complex * bignum
      return make_complex(heap, arith_mul(heap, real, rhs), arith_mul(heap, imag, rhs));
    }
    if (RATIONALP(rhs)) {  // complex * rational
      return make_complex(heap, arith_mul(heap, real, rhs), arith_mul(heap, imag, rhs));
    }
    if (COMPLEXP(rhs)) {  // complex * complex
      scm_obj_t real2 = arith_sub(heap, arith_mul(heap, real, ((scm_complex_t)rhs)->real), arith_mul(heap, imag, ((scm_complex_t)rhs)->imag));
      scm_obj_t imag2 = arith_add(heap, arith_mul(heap, imag, ((scm_complex_t)rhs)->real), arith_mul(heap, real, ((scm_complex_t)rhs)->imag));
      return oprtr_norm_complex(heap, real2, imag2);
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_div(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  scm_obj_t real;
  scm_obj_t imag;

  if (FIXNUMP(lhs)) {
    if (lhs == MAKEFIXNUM(0)) {
      if (n_exact_pred(rhs)) return MAKEFIXNUM(0);
      lhs = make_flonum(heap, 0.0);
      goto flonum_again;
    }
    if (FIXNUMP(rhs)) {  // fixnum / fixnum
      return oprtr_reduce_fixnum_fixnum(heap, lhs, rhs);
    }
    if (FLONUMP(rhs)) {  // fixnum / flonum
      return make_flonum(heap, (double)FIXNUM(lhs) / ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // fixnum / bignum
      return oprtr_reduce_fixnum_bignum(heap, (scm_fixnum_t)lhs, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // fixnum / rational
      scm_rational_t rn = (scm_rational_t)rhs;
      return oprtr_reduce(heap, arith_mul(heap, rn->deno, lhs), rn->nume);
    }
    if (COMPLEXP(rhs)) {  // fixnum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      scm_obj_t r2 = arith_add(heap, arith_mul(heap, real, real), arith_mul(heap, imag, imag));
      return make_complex(heap, arith_div(heap, arith_mul(heap, real, lhs), r2),
                          arith_negate(heap, arith_div(heap, arith_mul(heap, lhs, imag), r2)));
    }
  }
  if (FLONUMP(lhs)) {
  flonum_again:
    if (FIXNUMP(rhs)) {  // flonum / fixnum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value / FIXNUM(rhs));
    }
    if (FLONUMP(rhs)) {  // flonum / flonum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value / ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // flonum / bignum
      return make_flonum(heap, ((scm_flonum_t)lhs)->value / bignum_to_double((scm_bignum_t)rhs));
    }
    if (RATIONALP(rhs)) {  // flonum / rational
      return make_flonum(heap, ((scm_flonum_t)lhs)->value / rational_to_double((scm_rational_t)rhs));
    }
    if (COMPLEXP(rhs)) {  // flonum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      scm_obj_t r2 = arith_add(heap, arith_mul(heap, real, real), arith_mul(heap, imag, imag));
      return make_complex(heap, arith_div(heap, arith_mul(heap, real, lhs), r2),
                          arith_negate(heap, arith_div(heap, arith_mul(heap, lhs, imag), r2)));
    }
  }
  if (BIGNUMP(lhs)) {
    if (FIXNUMP(rhs)) {  // bignum / fixnum
      return oprtr_reduce_bignum_fixnum(heap, (scm_bignum_t)lhs, (scm_fixnum_t)rhs);
    }
    if (FLONUMP(rhs)) {  // bignum / flonum
      return make_flonum(heap, bignum_to_double((scm_bignum_t)lhs) / ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // bignum / bignum
      return oprtr_reduce(heap, lhs, rhs);
    }
    if (RATIONALP(rhs)) {  // bignum / rational
      scm_rational_t rn = (scm_rational_t)rhs;
      return oprtr_reduce(heap, arith_mul(heap, rn->deno, lhs), rn->nume);
    }
    if (COMPLEXP(rhs)) {  // bignum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      scm_obj_t r2 = arith_add(heap, arith_mul(heap, real, real), arith_mul(heap, imag, imag));
      return make_complex(heap, arith_div(heap, arith_mul(heap, real, lhs), r2),
                          arith_negate(heap, arith_div(heap, arith_mul(heap, lhs, imag), r2)));
    }
  }
  if (RATIONALP(lhs)) {
    if (FIXNUMP(rhs)) {  // rational / fixnum
      scm_rational_t rn = (scm_rational_t)lhs;
      return oprtr_reduce(heap, rn->nume, arith_mul(heap, rn->deno, rhs));
    }
    if (FLONUMP(rhs)) {  // rational / flonum
      return make_flonum(heap, rational_to_double((scm_rational_t)lhs) / ((scm_flonum_t)rhs)->value);
    }
    if (BIGNUMP(rhs)) {  // rational / bignum
      scm_rational_t rn = (scm_rational_t)lhs;
      return oprtr_reduce(heap, rn->nume, arith_mul(heap, rn->deno, rhs));
    }
    if (RATIONALP(rhs)) {  // rational / rational
      return oprtr_div(heap, (scm_rational_t)lhs, (scm_rational_t)rhs);
    }
    if (COMPLEXP(rhs)) {  // rational / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      scm_obj_t r2 = arith_add(heap, arith_mul(heap, real, real), arith_mul(heap, imag, imag));
      return make_complex(heap, arith_div(heap, arith_mul(heap, real, lhs), r2),
                          arith_negate(heap, arith_div(heap, arith_mul(heap, lhs, imag), r2)));
    }
  }
  if (COMPLEXP(lhs)) {
    real = ((scm_complex_t)lhs)->real;
    imag = ((scm_complex_t)lhs)->imag;
    if (FIXNUMP(rhs)) {  // complex / fixnum
      return make_complex(heap, arith_div(heap, real, rhs), arith_div(heap, imag, rhs));
    }
    if (FLONUMP(rhs)) {  // complex / flonum
      return make_complex(heap, arith_div(heap, real, rhs), arith_div(heap, imag, rhs));
    }
    if (BIGNUMP(rhs)) {  // complex / bignum
      return make_complex(heap, arith_div(heap, real, rhs), arith_div(heap, imag, rhs));
    }
    if (RATIONALP(rhs)) {  // complex / rational
      return make_complex(heap, arith_div(heap, real, rhs), arith_div(heap, imag, rhs));
    }
    if (COMPLEXP(rhs)) {  // complex / complex
      scm_obj_t real2 = ((scm_complex_t)rhs)->real;
      scm_obj_t imag2 = ((scm_complex_t)rhs)->imag;
      scm_obj_t r2 = arith_add(heap, arith_mul(heap, real2, real2), arith_mul(heap, imag2, imag2));
      scm_obj_t real3 = arith_div(heap, arith_add(heap, arith_mul(heap, real, real2), arith_mul(heap, imag, imag2)), r2);
      scm_obj_t imag3 = arith_div(heap, arith_sub(heap, arith_mul(heap, imag, real2), arith_mul(heap, real, imag2)), r2);
      return oprtr_norm_complex(heap, real3, imag3);
    }
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_quotient(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  BN_TEMPORARY(bn);
  scm_obj_t real;
  scm_obj_t imag;
start_again:
  if (FIXNUMP(lhs)) {
    if (FIXNUM(lhs) == 0) return MAKEFIXNUM(0);
  fixnum_again:
    if (FIXNUMP(rhs)) {  // fixnum / fixnum
      if (FIXNUM(rhs) == 0) {
        fatal("%s:%u undefined for 0", __FILE__, __LINE__);
      }
      return MAKEFIXNUM(FIXNUM(lhs) / FIXNUM(rhs));
    }
    if (FLONUMP(rhs)) {  // fixnum / flonum
      double value = ((scm_flonum_t)rhs)->value;
      if (value == round(value)) {
        if (value == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        return make_flonum(heap, trunc(FIXNUM(lhs) / value));
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // fixnum / bignum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_quotient(heap, &bn, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // fixnum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // fixnum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto fixnum_again;
      }
      assert(false);
    }
  }
  if (FLONUMP(lhs)) {
    double value = ((scm_flonum_t)lhs)->value;
    if (value == round(value)) {
      if (value == 0.0) return make_flonum(heap, 0.0);
    } else {
      assert(false);
    }
  flonum_again:
    if (FIXNUMP(rhs)) {  // flonum / fixnum
      return make_flonum(heap, trunc(value / FIXNUM(rhs)));
    }
    if (FLONUMP(rhs)) {  // flonum / flonum
      double value2 = ((scm_flonum_t)rhs)->value;
      if (value2 == round(value2)) {
        if (value2 == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        return make_flonum(heap, trunc(value / value2));
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // flonum / bignum
      return make_flonum(heap, trunc(value / bignum_to_double((scm_bignum_t)rhs)));
    }
    if (RATIONALP(rhs)) {  // flonum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // flonum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto flonum_again;
      }
      assert(false);
    }
  }
  if (BIGNUMP(lhs)) {
  bignum_again:
    if (FIXNUMP(rhs)) {  // bignum / fixnum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_quotient(heap, (scm_bignum_t)lhs, &bn);
    }
    if (FLONUMP(rhs)) {  // bignum / flonum
      double value = ((scm_flonum_t)rhs)->value;
      if (value == round(value)) {
        if (value == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        return make_flonum(heap, trunc(bignum_to_double((scm_bignum_t)lhs) / value));
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // bignum / bignum
      return oprtr_quotient(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // bignum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // bignum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto bignum_again;
      }
      assert(false);
    }
  }
  if (RATIONALP(lhs)) {
    assert(false);
  }
  if (COMPLEXP(lhs)) {
    real = ((scm_complex_t)lhs)->real;
    imag = ((scm_complex_t)lhs)->imag;
    if (n_zero_pred(imag)) {
      lhs = real;
      goto start_again;
    }
    assert(false);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_remainder(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  BN_TEMPORARY(bn);
  scm_obj_t real;
  scm_obj_t imag;
start_again:
  if (FIXNUMP(lhs)) {
    if (FIXNUM(lhs) == 0) return MAKEFIXNUM(0);
  fixnum_again:
    if (FIXNUMP(rhs)) {  // fixnum / fixnum
      if (FIXNUM(rhs) == 0) {
        fatal("%s:%u undefined for 0", __FILE__, __LINE__);
      }
      return MAKEFIXNUM(FIXNUM(lhs) % FIXNUM(rhs));
    }
    if (FLONUMP(rhs)) {  // fixnum / flonum
      double value = ((scm_flonum_t)rhs)->value;
      if (value == round(value)) {
        if (value == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        return make_flonum(heap, fmod((double)FIXNUM(lhs), value));
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // fixnum / bignum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_remainder(heap, &bn, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // fixnum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // fixnum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto fixnum_again;
      }
      assert(false);
    }
  }
  if (FLONUMP(lhs)) {
    double value = ((scm_flonum_t)lhs)->value;
    if (value == round(value)) {
      if (value == 0.0) return make_flonum(heap, 0.0);
    } else {
      assert(false);
    }
  flonum_again:
    if (FIXNUMP(rhs)) {  // flonum / fixnum
      return make_flonum(heap, fmod(value, (double)FIXNUM(rhs)));
    }
    if (FLONUMP(rhs)) {  // flonum / flonum
      double value2 = ((scm_flonum_t)rhs)->value;
      if (value2 == round(value2)) {
        if (value2 == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        return make_flonum(heap, fmod(value, value2));
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // flonum / bignum
      return make_flonum(heap, fmod(value, bignum_to_double((scm_bignum_t)rhs)));
    }
    if (RATIONALP(rhs)) {  // flonum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // flonum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto flonum_again;
      }
      assert(false);
    }
  }
  if (BIGNUMP(lhs)) {
  bignum_again:
    if (FIXNUMP(rhs)) {  // bignum / fixnum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_remainder(heap, (scm_bignum_t)lhs, &bn);
    }
    if (FLONUMP(rhs)) {  // bignum / flonum
      double value = ((scm_flonum_t)rhs)->value;
      if (value == round(value)) {
        if (value == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        return make_flonum(heap, fmod(bignum_to_double((scm_bignum_t)lhs), value));
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // bignum / bignum
      return oprtr_remainder(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // bignum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // bignum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto bignum_again;
      }
      assert(false);
    }
  }
  if (RATIONALP(lhs)) {
    assert(false);
  }
  if (COMPLEXP(lhs)) {
    real = ((scm_complex_t)lhs)->real;
    imag = ((scm_complex_t)lhs)->imag;
    if (n_zero_pred(imag)) {
      lhs = real;
      goto start_again;
    }
    assert(false);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_modulo(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  BN_TEMPORARY(bn);
  scm_obj_t real;
  scm_obj_t imag;
start_again:
  if (FIXNUMP(lhs)) {
    if (FIXNUM(lhs) == 0) return MAKEFIXNUM(0);
  fixnum_again:
    if (FIXNUMP(rhs)) {  // fixnum / fixnum
      if (FIXNUM(rhs) == 0) return scm_unspecified;
      intptr_t rem = FIXNUM(lhs) % FIXNUM(rhs);
      if (rem == 0) return MAKEFIXNUM(0);
      if ((FIXNUM(rhs) > 0) + (rem > 0) == 1) rem = rem + FIXNUM(rhs);
      assert(rem >= FIXNUM_MIN && rem <= FIXNUM_MAX);
      return MAKEFIXNUM(rem);
    }
    if (FLONUMP(rhs)) {  // fixnum / flonum
      double value = ((scm_flonum_t)rhs)->value;
      if (value == round(value)) {
        if (value == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
          return scm_unspecified;
        }
        double rem = fmod((double)FIXNUM(lhs), value);
        if (rem == 0.0) return make_flonum(heap, 0.0);
        if ((value > 0.0) + (rem > 0.0) == 1) rem = rem + value;
        return make_flonum(heap, rem);
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // fixnum / bignum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)lhs);
      return oprtr_modulo(heap, &bn, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // fixnum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // fixnum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto fixnum_again;
      }
      assert(false);
    }
  }
  if (FLONUMP(lhs)) {
    double value = ((scm_flonum_t)lhs)->value;
    if (value == round(value)) {
      if (value == 0.0) return make_flonum(heap, 0.0);
    } else {
      assert(false);
    }
  flonum_again:
    if (FIXNUMP(rhs)) {  // flonum / fixnum
      double rem = fmod(value, (double)FIXNUM(rhs));
      if ((FIXNUM(rhs) > 0) + (rem > 0.0) == 1) rem = rem + FIXNUM(rhs);
      if (rem == 0) return MAKEFIXNUM(0);
      return make_flonum(heap, rem);
    }
    if (FLONUMP(rhs)) {  // flonum / flonum
      double value2 = ((scm_flonum_t)rhs)->value;
      if (value2 == round(value2)) {
        if (value2 == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        double rem = fmod(value, value2);
        if (rem == 0.0) return make_flonum(heap, 0.0);
        if ((value2 > 0.0) + (rem > 0.0) == 1) rem = rem + value2;
        return make_flonum(heap, rem);
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // flonum / bignum
      double value2 = bignum_to_double((scm_bignum_t)rhs);
      double rem = fmod(value, value2);
      if (rem == 0.0) return make_flonum(heap, 0.0);
      if ((value2 > 0.0) + (rem > 0.0) == 1) rem = rem + value2;
      return make_flonum(heap, rem);
    }
    if (RATIONALP(rhs)) {  // flonum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // flonum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto flonum_again;
      }
      assert(false);
    }
  }
  if (BIGNUMP(lhs)) {
  bignum_again:
    if (FIXNUMP(rhs)) {  // bignum / fixnum
      BN_ALLOC_FIXNUM(bn);
      bn_let(&bn, (scm_fixnum_t)rhs);
      return oprtr_modulo(heap, (scm_bignum_t)lhs, &bn);
    }
    if (FLONUMP(rhs)) {  // bignum / flonum
      double value = ((scm_flonum_t)rhs)->value;
      if (value == round(value)) {
        if (value == 0.0) {
          fatal("%s:%u undefined for 0", __FILE__, __LINE__);
        }
        double value2 = bignum_to_double((scm_bignum_t)rhs);
        double rem = fmod(value2, value);
        if (rem == 0.0) return make_flonum(heap, 0.0);
        if ((value > 0.0) + (rem > 0.0) == 1) rem = rem + value;
        return make_flonum(heap, rem);
      }
      assert(false);
    }
    if (BIGNUMP(rhs)) {  // bignum / bignum
      return oprtr_modulo(heap, (scm_bignum_t)lhs, (scm_bignum_t)rhs);
    }
    if (RATIONALP(rhs)) {  // bignum / rational
      assert(false);
    }
    if (COMPLEXP(rhs)) {  // bignum / complex
      real = ((scm_complex_t)rhs)->real;
      imag = ((scm_complex_t)rhs)->imag;
      if (n_zero_pred(imag)) {
        rhs = real;
        goto bignum_again;
      }
      assert(false);
    }
  }
  if (RATIONALP(lhs)) {
    assert(false);
  }
  if (COMPLEXP(lhs)) {
    real = ((scm_complex_t)lhs)->real;
    imag = ((scm_complex_t)lhs)->imag;
    if (n_zero_pred(imag)) {
      lhs = real;
      goto start_again;
    }
    assert(false);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_expt(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  if (FLONUMP(lhs) && FLONUM(lhs) == 0.0) {
    if (COMPLEXP(rhs)) {
      if (n_positive_pred(((scm_complex_t)rhs)->real)) return make_flonum(heap, 0.0);
    } else {
      if (n_positive_pred(rhs)) return make_flonum(heap, 0.0);
    }
  }
  if (n_exact_pred(rhs)) {
    if (FIXNUMP(rhs)) {
      if (FIXNUM(rhs) == 0) return MAKEFIXNUM(1);
      if (FLONUMP(lhs)) return make_flonum(heap, pow(((scm_flonum_t)lhs)->value, (double)FIXNUM(rhs)));
      return oprtr_expt(heap, lhs, (scm_fixnum_t)rhs);
    }
    if (BIGNUMP(rhs)) {
      assert(!n_exact_pred(lhs));
      if (real_valued_pred(lhs)) {
        double n = bignum_to_double((scm_bignum_t)rhs);
        return make_flonum(heap, pow(real_to_double(lhs), n));
      }
      return arith_exp(heap, arith_mul(heap, rhs, arith_log(heap, lhs)));
    }
    if (RATIONALP(rhs)) {
      double n = rational_to_double((scm_rational_t)rhs);
      if (real_valued_pred(lhs) && !n_negative_pred(lhs)) return make_flonum(heap, pow(real_to_double(lhs), n));
      return arith_exp(heap, arith_mul(heap, rhs, arith_log(heap, lhs)));
    }
    if (COMPLEXP(rhs)) {
      return arith_exp(heap, arith_mul(heap, rhs, arith_log(heap, lhs)));
    }
    fatal("%s:%u wrong datum type", __FILE__, __LINE__);
  } else {
    if (FLONUMP(rhs)) {
      if (real_valued_pred(lhs) && !n_negative_pred(lhs)) {
        double n = ((scm_flonum_t)rhs)->value;
        return make_flonum(heap, pow(real_to_double(lhs), n));
      }
      return arith_exp(heap, arith_mul(heap, rhs, arith_log(heap, lhs)));
    }
    return arith_exp(heap, arith_mul(heap, rhs, arith_log(heap, lhs)));
  }
}

scm_obj_t arith_exp(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    if (FIXNUM(obj) == 0) return MAKEFIXNUM(1);
    return make_flonum(heap, exp((double)FIXNUM(obj)));
  }
  if (COMPLEXP(obj)) {
    double real = real_to_double(((scm_complex_t)obj)->real);
    double imag = real_to_double(((scm_complex_t)obj)->imag);
    double a = exp(real);
    return make_complex(heap, a * cos(imag), a * sin(imag));
  }
  if (real_valued_pred(obj)) return make_flonum(heap, exp(real_to_double(obj)));
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_log(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t value FIXNUM(obj);
    if (value > 0) {
      if (value == 1) return MAKEFIXNUM(0);
      return make_flonum(heap, log((double)value));
    }
    double real = value;
    return make_complex(heap, 0.5 * log(real * real), atan2(0.0, real));
  }
  if (COMPLEXP(obj)) {
    double real = real_to_double(((scm_complex_t)obj)->real);
    double imag = real_to_double(((scm_complex_t)obj)->imag);
    return make_complex(heap, 0.5 * log(real * real + imag * imag), atan2(imag, real));
  }
  if (real_valued_pred(obj)) {
    double real = real_to_double(obj);
    if (isinf(real) && exact_positive_integer_pred(obj)) {
      exact_integer_sqrt_ans_t ans = arith_exact_integer_sqrt(heap, obj);
      return arith_add(heap, arith_log(heap, ans.s), arith_log(heap, ans.s));
    }
    if (real > 0) return make_flonum(heap, log(real));
    double imag = atan2(0.0, real);
    if (imag == 0.0) return make_flonum(heap, 0.5 * log(real * real));
    return make_complex(heap, 0.5 * log(real * real), imag);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_sin(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    if (FIXNUM(obj) == 0) return MAKEFIXNUM(0);
    return make_flonum(heap, sin((double)FIXNUM(obj)));
  }
  if (COMPLEXP(obj)) {
    double real = real_to_double(((scm_complex_t)obj)->real);
    double imag = real_to_double(((scm_complex_t)obj)->imag);
    double e = exp(imag);
    double f = 1.0 / e;
    return make_complex(heap, 0.5 * sin(real) * (e + f), 0.5 * cos(real) * (e - f));
  }
  if (real_valued_pred(obj)) return make_flonum(heap, sin(real_to_double(obj)));
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_cos(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    if (FIXNUM(obj) == 0) return MAKEFIXNUM(1);
    return make_flonum(heap, cos((double)FIXNUM(obj)));
  }
  if (COMPLEXP(obj)) {
    double real = real_to_double(((scm_complex_t)obj)->real);
    double imag = real_to_double(((scm_complex_t)obj)->imag);
    double e = exp(imag);
    double f = 1.0 / e;
    return make_complex(heap, 0.5 * cos(real) * (f + e), 0.5 * sin(real) * (f - e));
  }
  if (real_valued_pred(obj)) return make_flonum(heap, cos(real_to_double(obj)));
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_tan(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    if (FIXNUM(obj) == 0) return MAKEFIXNUM(0);
    return make_flonum(heap, tan((double)FIXNUM(obj)));
  }
  if (COMPLEXP(obj)) {
    double real = real_to_double(((scm_complex_t)obj)->real);
    double imag = real_to_double(((scm_complex_t)obj)->imag);
    double e = exp(2.0 * imag);
    double f = 1.0 / e;
    double d = cos(2.0 * real) + 0.5 * (e + f);
    return make_complex(heap, sin(2.0 * real) / d, 0.5 * (e - f) / d);
  }
  if (real_valued_pred(obj)) return make_flonum(heap, tan(real_to_double(obj)));
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_sqrt(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t value = FIXNUM(obj);
    if (value == 0) return MAKEFIXNUM(0);
    if (value > 0) {
      intptr_t iroot = (intptr_t)floor(sqrt((double)value));
      if (iroot * iroot == value) return MAKEFIXNUM(iroot);
      return make_flonum(heap, sqrt((double)value));
    } else {
      value = -value;
      intptr_t iroot = (intptr_t)floor(sqrt((double)value));
      if (iroot * iroot == value) return make_complex(heap, MAKEFIXNUM(0), MAKEFIXNUM(iroot));
      return make_complex(heap, make_flonum(heap, 0.0), make_flonum(heap, sqrt((double)value)));
    }
  }
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    int count = bn_get_count(bn);
    BN_TEMPORARY(workpad);
    BN_ALLOC(workpad, count);
    bn_set_sign(&workpad, 1);
    memcpy(workpad.elts, bn->elts, sizeof(digit_t) * count);
    bn_sqrt(heap, &workpad);
    if (bn->elts[0] == workpad.elts[0] * workpad.elts[0]) {
      BN_TEMPORARY(s2);
      BN_ALLOC(s2, bn_get_count(bn));
      bn_set_sign(&s2, 1);
      bn_mul(&s2, &workpad, &workpad);
      if (bn_cmp(bn, &s2) == 0) {
        if (bn_get_sign(bn) == 1) return bn_to_integer(heap, &workpad);
        return make_complex(heap, MAKEFIXNUM(0), bn_to_integer(heap, &workpad));
      }
    }
    double s = bignum_to_double(bn);
    s = sqrt(s < 0.0 ? -s : s);
    if (bn_get_sign(bn) == 1) return make_flonum(heap, s);
    return make_complex(heap, make_flonum(heap, 0.0), make_flonum(heap, s));
  }
  if (RATIONALP(obj)) {
    scm_rational_t rn = (scm_rational_t)obj;
    scm_obj_t numerator;
    scm_obj_t denominator;
    bool complex = false;
    if (n_negative_pred(rn->nume)) {
      numerator = arith_sqrt(heap, arith_negate(heap, rn->nume));
      complex = true;
    } else {
      numerator = arith_sqrt(heap, rn->nume);
    }
    denominator = arith_sqrt(heap, rn->deno);
    if (FIXNUMP(numerator) || BIGNUMP(numerator)) {
      if (FIXNUMP(denominator) || BIGNUMP(denominator)) {
        if (complex) return make_complex(heap, MAKEFIXNUM(0), oprtr_reduce(heap, numerator, denominator));
        return oprtr_reduce(heap, numerator, denominator);
      }
    }
    if (complex) return make_complex(heap, make_flonum(heap, 0.0), arith_div(heap, numerator, denominator));
    return arith_div(heap, numerator, denominator);
  }
  if (FLONUMP(obj)) {
    scm_flonum_t flonum = (scm_flonum_t)obj;
    double s = flonum->value;
    if (s < 0.0) return make_complex(heap, make_flonum(heap, 0.0), make_flonum(heap, sqrt(-s)));
    return make_flonum(heap, sqrt(s));
  }
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    if (n_exact_pred(cn)) {
      scm_obj_t m = arith_magnitude(heap, obj);
      scm_obj_t x = arith_div(heap, arith_add(heap, cn->real, m), MAKEFIXNUM(2));
      scm_obj_t y = arith_div(heap, cn->imag, MAKEFIXNUM(2));
      scm_obj_t s = arith_sqrt(heap, arith_div(heap, m, arith_add(heap, arith_mul(heap, x, x), arith_mul(heap, y, y))));
      return oprtr_norm_complex(heap, arith_mul(heap, x, s), arith_mul(heap, y, s));
    }
    double real = real_to_double(cn->real);
    double imag = real_to_double(cn->imag);
    double m = sqrt(real * real + imag * imag);
    double x = (real + m) / 2;
    double y = imag / 2;
    double s = sqrt(m / (x * x + y * y));
    return make_complex(heap, make_flonum(heap, x * s), make_flonum(heap, y * s));
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

exact_integer_sqrt_ans_t arith_exact_integer_sqrt(object_heap_t* heap, scm_obj_t obj) {
  exact_integer_sqrt_ans_t ans = {scm_undef, scm_undef};
  if (FIXNUMP(obj)) {
    intptr_t value = FIXNUM(obj);
    assert(value >= 0);
    if (value == 0) {
      ans.s = ans.r = MAKEFIXNUM(0);
      return ans;
    }
#if ARCH_LP64
    if (value < iexpt_2n53) {
      intptr_t iroot = (intptr_t)floor(sqrt((double)value));
      ans.s = MAKEFIXNUM(iroot);
      ans.r = MAKEFIXNUM(value - iroot * iroot);
      return ans;
    }
    BN_TEMPORARY(workpad);
    BN_ALLOC_FIXNUM(workpad);
    bn_let(&workpad, (scm_fixnum_t)obj);
    bn_set_sign(&workpad, 1);
    bn_sqrt(heap, &workpad);
    ans.s = bn_to_integer(heap, &workpad);
    ans.r = arith_sub(heap, obj, arith_mul(heap, ans.s, ans.s));
    return ans;
#else
    intptr_t iroot = (intptr_t)floor(sqrt((double)value));
    ans.s = MAKEFIXNUM(iroot);
    ans.r = MAKEFIXNUM(value - iroot * iroot);
    return ans;
#endif
  }
  if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    assert(bn_get_sign(bn) == 1);
    int count = bn_get_count(bn);
    BN_TEMPORARY(workpad);
    BN_ALLOC(workpad, count);
    bn_set_sign(&workpad, 1);
    memcpy(workpad.elts, bn->elts, sizeof(digit_t) * count);
    bn_sqrt(heap, &workpad);
    ans.s = bn_to_integer(heap, &workpad);
    ans.r = arith_sub(heap, obj, arith_mul(heap, ans.s, ans.s));
    return ans;
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_asin(object_heap_t* heap, scm_obj_t obj) {
  scm_complex_t cn;
  if (real_valued_pred(obj)) {
    double x = real_to_double(obj);
    if (x >= -1.0 && x <= 1.0) return make_flonum(heap, asin(real_to_double(obj)));
    if (x < 0.0) return arith_negate(heap, arith_asin(heap, make_flonum(heap, -x)));
    cn = make_complex(heap, make_flonum(heap, 0.0), make_flonum(heap, x));
  } else {
    assert(COMPLEXP(obj));
    if (n_positive_pred(((scm_complex_t)obj)->imag)) return arith_negate(heap, arith_asin(heap, arith_negate(heap, obj)));
    cn = make_complex(heap, arith_negate(heap, ((scm_complex_t)obj)->imag), ((scm_complex_t)obj)->real);
  }
  scm_obj_t ans = arith_log(heap, arith_add(heap, arith_sqrt(heap, arith_sub(heap, MAKEFIXNUM(1), arith_mul(heap, obj, obj))), cn));
  if (COMPLEXP(ans)) {
    return make_complex(heap, real_to_double(((scm_complex_t)ans)->imag), -real_to_double(((scm_complex_t)ans)->real));
  }
  return make_complex(heap, 0.0, -real_to_double(ans));
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_acos(object_heap_t* heap, scm_obj_t obj) {
  if (real_valued_pred(obj)) {
    double x = real_to_double(obj);
    if (x >= -1.0 && x <= 1.0) return make_flonum(heap, acos(real_to_double(obj)));
  }
  return arith_sub(heap, make_flonum(heap, 3.141592653589793 / 2.0), arith_asin(heap, obj));
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_atan(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    if (FIXNUM(obj) == 0) return MAKEFIXNUM(0);
    return make_flonum(heap, atan((double)FIXNUM(obj)));
  }
  if (COMPLEXP(obj)) {
    scm_complex_t cn = make_complex(heap, arith_negate(heap, ((scm_complex_t)obj)->imag), ((scm_complex_t)obj)->real);
    scm_obj_t ans = arith_log(heap, arith_div(heap, arith_add(heap, MAKEFIXNUM(1), cn), arith_sub(heap, MAKEFIXNUM(1), cn)));
    if (COMPLEXP(ans)) {
      return make_complex(heap, 0.5 * real_to_double(((scm_complex_t)ans)->imag), -0.5 * real_to_double(((scm_complex_t)ans)->real));
    }
    return make_complex(heap, 0.0, -0.5 * real_to_double(ans));
  }
  if (real_valued_pred(obj)) return make_flonum(heap, atan(real_to_double(obj)));
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_atan2(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  assert(real_valued_pred(lhs));
  assert(real_valued_pred(rhs));
  if (lhs == MAKEFIXNUM(0)) return MAKEFIXNUM(0);
  return make_flonum(heap, atan2(real_to_double(lhs), real_to_double(rhs)));
}

scm_obj_t arith_magnitude(object_heap_t* heap, scm_obj_t obj) {
  if (COMPLEXP(obj)) {
    // (magnitude 3/22+2/11i) => 5/22
    scm_complex_t cn = (scm_complex_t)obj;
    if (n_exact_pred(cn)) {
      if (n_zero_pred(cn->real)) return arith_magnitude(heap, cn->imag);
      if (n_zero_pred(cn->imag)) return arith_magnitude(heap, cn->real);
      return arith_sqrt(heap, arith_add(heap, arith_mul(heap, cn->real, cn->real), arith_mul(heap, cn->imag, cn->imag)));
    } else {
      double real = real_to_double(((scm_complex_t)obj)->real);
      double imag = real_to_double(((scm_complex_t)obj)->imag);
      if (isinf(real) || isinf(imag)) return make_flonum(heap, VALUE_INF);
      double m = sqrt(real * real + imag * imag);
      if (m < DBL_EPSILON || isinf(m)) return make_flonum(heap, imag / sin(atan2(imag, real)));
      return make_flonum(heap, m);
    }
  }
  if (real_valued_pred(obj)) {
    if (n_negative_pred(obj)) return arith_negate(heap, obj);
    return obj;
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_angle(object_heap_t* heap, scm_obj_t obj) {
  if (COMPLEXP(obj)) {
    double real = real_to_double(((scm_complex_t)obj)->real);
    double imag = real_to_double(((scm_complex_t)obj)->imag);
    return make_flonum(heap, atan2(imag, real));
  }
  if (real_valued_pred(obj)) {
    if (n_negative_pred(obj)) return make_flonum(heap, atan2(0.0, -1.0));  // pi
    if (FLONUMP(obj)) return make_flonum(heap, 0.0);
    return MAKEFIXNUM(0);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_rectangular(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) { return oprtr_norm_complex(heap, lhs, rhs); }

scm_obj_t arith_polar(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  if (rhs == MAKEFIXNUM(0)) return lhs;
  double r = real_to_double(lhs);
  double a = real_to_double(rhs);
  return make_complex(heap, r * cos(a), r * sin(a));
}

scm_obj_t arith_floor(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) return obj;
  if (BIGNUMP(obj)) return obj;
  if (FLONUMP(obj)) {
    double value = ((scm_flonum_t)obj)->value;
    return make_flonum(heap, floor(value));
  }
  if (RATIONALP(obj)) {
    scm_rational_t rn = (scm_rational_t)obj;
    if (n_negative_pred(rn->nume)) return arith_sub(heap, arith_quotient(heap, rn->nume, rn->deno), MAKEFIXNUM(1));
    return arith_quotient(heap, rn->nume, rn->deno);
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t arith_integer_div(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  if (FIXNUMP(lhs)) {
    if (FIXNUMP(rhs)) {
      intptr_t x = FIXNUM(lhs);
      intptr_t y = FIXNUM(rhs);
      intptr_t div;
      if (x == 0) {
        div = 0;
      } else if (x > 0) {
        div = x / y;
      } else if (y > 0) {
        div = (x - y + 1) / y;
      } else {
        div = (x + y + 1) / y;
      }
      return intptr_to_integer(heap, div);
    }
  }
  if (FLONUMP(lhs) || FLONUMP(rhs)) {
    double x = real_to_double(lhs);
    double y = real_to_double(rhs);
    return make_flonum(heap, (y > 0.0) ? floor(x / y) : -floor(x / -y));
  }
  if (n_positive_pred(rhs)) return arith_floor(heap, arith_div(heap, lhs, rhs));
  return arith_negate(heap, arith_floor(heap, arith_div(heap, lhs, arith_negate(heap, rhs))));
}

scm_obj_t arith_integer_div0(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs) {
  scm_obj_t div = arith_integer_div(heap, lhs, rhs);
  scm_obj_t mod = arith_sub(heap, lhs, arith_mul(heap, div, rhs));
  if (n_compare(heap, mod, arith_magnitude(heap, arith_div(heap, rhs, MAKEFIXNUM(2)))) < 0) return div;
  if (n_positive_pred(rhs)) return arith_add(heap, div, MAKEFIXNUM(1));
  return arith_sub(heap, div, MAKEFIXNUM(1));
}

scm_obj_t cnvt_to_inexact(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) return make_flonum(heap, FIXNUM(obj));
  if (FLONUMP(obj)) return obj;
  if (BIGNUMP(obj)) return make_flonum(heap, bignum_to_double((scm_bignum_t)obj));
  if (RATIONALP(obj)) return make_flonum(heap, rational_to_double((scm_rational_t)obj));
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    if (FLONUMP(cn->real) & FLONUMP(cn->imag)) return obj;
    return make_complex(heap, cnvt_to_inexact(heap, cn->real), cnvt_to_inexact(heap, cn->imag));
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

scm_obj_t cnvt_to_exact(object_heap_t* heap, scm_obj_t obj) {
  if (FLONUMP(obj)) {
    if (((scm_flonum_t)obj)->value == 0.0) return MAKEFIXNUM(0);
    int sign;
    int exp;
    int64_t mant = decode_double(((scm_flonum_t)obj)->value, &exp, &sign);
    if (mant > 0) {
      if (exp == 0) return int64_to_integer(heap, (sign > 0 ? mant : -mant));
      if (exp > 0) {
#if USE_DIGIT32
        int count = 2 + (exp / DIGIT_BIT) + 1;
        BN_TEMPORARY(bn);
        BN_ALLOC(bn, count);
        memset(bn.elts, 0, sizeof(digit_t) * count);
        bn.elts[0] = (digit_t)mant;
        bn.elts[1] = (digit_t)(mant >> DIGIT_BIT);
        bn_shift_left(&bn, exp);
        bn_set_sign(&bn, sign);
        bn_norm(&bn);
        return bn_dup(heap, &bn);
#else
        int count = 1 + (exp / DIGIT_BIT) + 1;
        BN_TEMPORARY(bn);
        BN_ALLOC(bn, count);
        memset(bn.elts, 0, sizeof(digit_t) * count);
        bn.elts[0] = (digit_t)mant;
        bn_shift_left(&bn, exp);
        bn_set_sign(&bn, sign);
        bn_norm(&bn);
        return bn_dup(heap, &bn);
#endif
      }
      int count = (-exp / DIGIT_BIT) + 1;
      BN_TEMPORARY(bn);
      BN_ALLOC(bn, count);
      memset(bn.elts, 0, sizeof(digit_t) * count);
      bn.elts[0] = 1;
      bn_shift_left(&bn, -exp);
      bn_set_sign(&bn, sign);
      bn_norm(&bn);
      return oprtr_reduce(heap, int64_to_integer(heap, mant), bn_to_integer(heap, &bn));
    }
    assert(false);
  }
  if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    if (FLONUMP(cn->real) || FLONUMP(cn->imag)) {
      return oprtr_norm_complex(heap, cnvt_to_exact(heap, cn->real), cnvt_to_exact(heap, cn->imag));
    }
    return obj;
  }
  if (FIXNUMP(obj) || BIGNUMP(obj) || RATIONALP(obj)) return obj;
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
}

static bool small_bignum_to_string(object_heap_t* heap, scm_bignum_t bn, int radix, char* buf, int buf_size) {
  int workpad_count = bn_get_count(bn);
  if (workpad_count) {
    BN_TEMPORARY(workpad);
    BN_ALLOC(workpad, workpad_count);
    memcpy(workpad.elts, bn->elts, sizeof(digit_t) * workpad_count);
    int i = buf_size - 1;
    buf[i--] = 0;
    if (radix == 10) {
      while (bn_get_count(&workpad)) {
        if (i < 0) return false;
        digit_t value = bn_div_digit(&workpad, &workpad, 1000000000U);
        for (int chunk = 0; chunk < 9; chunk++) {
          if (i < 0) return false;
          if (value) {
            int digit = value % 10;
            value /= 10;
            buf[i--] = digit + '0';
          } else {
            buf[i--] = '0';
          }
        }
      }
      while (buf[i + 1] == '0') i++;
    } else {
      while (bn_get_count(&workpad)) {
        if (i < 0) return false;
        digit_t digit = bn_div_digit(&workpad, &workpad, radix);
        if (digit < 10)
          buf[i--] = digit + '0';
        else
          buf[i--] = digit + 'a' - 10;
      }
    }
    if (i < 0) return false;
    if (bn_get_sign(bn) == -1) buf[i--] = '-';
    memmove(buf, &buf[i + 1], buf_size - i - 1);
    return true;
  }
  strncpy(buf, "0", buf_size);
  return true;
}

scm_string_t cnvt_bignum_to_string(object_heap_t* heap, scm_bignum_t bn, int radix) {
  int workpad_count = bn_get_count(bn);
  if (workpad_count) {
#if USE_DIGIT32
    if (workpad_count < 4) {
      char buf[128];
      if (small_bignum_to_string(heap, bn, radix, buf, sizeof(buf))) return make_string(heap, buf);
    }
#else
    if (workpad_count < 2) {
      char buf[128];
      if (small_bignum_to_string(heap, bn, radix, buf, sizeof(buf))) return make_string(heap, buf);
    }
#endif
    scm_port_t port = make_bytevector_port(heap, make_symbol(heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    BN_TEMPORARY(workpad);
    BN_ALLOC(workpad, workpad_count);
    memcpy(workpad.elts, bn->elts, sizeof(digit_t) * workpad_count);
    if (radix == 10) {
      while (bn_get_count(&workpad)) {
        digit_t value = bn_div_digit(&workpad, &workpad, 1000000000U);
        for (int chunk = 0; chunk < 9; chunk++) {
          if (value) {
            int digit = value % 10;
            value /= 10;
            port_put_byte(port, digit + '0');
          } else {
            port_put_byte(port, '0');
          }
        }
      }
    } else {
      while (bn_get_count(&workpad)) {
        digit_t digit = bn_div_digit(&workpad, &workpad, radix);
        if (digit < 10)
          port_put_byte(port, digit + '0');
        else
          port_put_byte(port, digit + 'a' - 10);
      }
    }
    bool negative = (bn_get_sign(bn) == -1);
    uint8_t* last = port->buf_tail - 1;
    while (*last == '0') last--;
    assert(last >= port->buf_head);
    int len = (last - port->buf_head) + 1;
    if (negative) len++;
    scm_string_t string = make_string(heap, len, 0);
    char* dst = string->name;
    if (negative) *dst++ = '-';
    while (last >= port->buf_head) *dst++ = *last--;
    assert(string->name + len == dst);
    assert(*dst == 0);
    port_close(port);
    return string;
  } else {
    return make_string(heap, "0");
  }
}

scm_string_t cnvt_fixnum_to_string(object_heap_t* heap, scm_fixnum_t fixnum, int radix) {
  char buf[64];
  intptr_t value = FIXNUM(fixnum);
  if (value) {
    bool negative = false;
    if (value < 0) {
      negative = true;
      value = -value;
    }
    int i = array_sizeof(buf) - 1;
    buf[i--] = 0;
    while (value) {
      assert(i >= 0);
      int digit = value % radix;
      value /= radix;
      if (digit < 10)
        buf[i--] = digit + '0';
      else
        buf[i--] = digit + 'a' - 10;
    }
    assert(i > 0);
    if (negative) buf[i--] = '-';
    return make_string(heap, &buf[i + 1]);
  }
  return make_string(heap, "0");
}

static scm_obj_t integer_mul10_may_inplace(object_heap_t* heap, scm_obj_t n) {
#if USE_DIGIT32
  #define OVERFLOW_TEST_BITS 0xf0000000
#else
  #define OVERFLOW_TEST_BITS 0xf000000000000000
#endif
  if (FIXNUMP(n)) {
    signed_digit2x_t n10 = (signed_digit2x_t)FIXNUM(n) * 10;
    if ((n10 >= FIXNUM_MIN) & (n10 <= FIXNUM_MAX)) return MAKEFIXNUM((signed_digit_t)n10);
    return signed_digit2x_to_bignum(heap, n10);
  }
  if (BIGNUMP(n)) {
    scm_bignum_t bn = (scm_bignum_t)n;
    int count = bn_get_count(bn);
    if (count == 0) return MAKEFIXNUM(0);
    if ((bn->elts[count - 1] & OVERFLOW_TEST_BITS) == 0) {
      digit2x_t acc = 0;
      for (int i = 0; i < count; i++) {
        acc = (digit2x_t)bn->elts[i] * 10 + acc;
        bn->elts[i] = (digit_t)acc;
        acc >>= DIGIT_BIT;
      }
      return bn;
    }
    scm_bignum_t ans = make_bignum(heap, count + 1);
    digit2x_t acc = 0;
    for (int i = 0; i < count; i++) {
      acc = (digit2x_t)bn->elts[i] * 10 + acc;
      ans->elts[i] = (digit_t)acc;
      acc >>= DIGIT_BIT;
    }
    if (acc == 0)
      bn_set_count(ans, count);
    else
      ans->elts[count] = acc;
    bn_set_sign(ans, 1);
    return ans;
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
#undef OVERFLOW_TEST_BITS
}

static digit_t integer_nth_digit(int n, scm_obj_t obj) {
  assert(FIXNUMP(obj) || BIGNUMP(obj));
  if (FIXNUMP(obj)) return n == 0 ? FIXNUM(obj) : 0;
  scm_bignum_t bn = (scm_bignum_t)obj;
  if (n >= bn_get_count(bn)) return 0;
  return bn->elts[n];
}

static int integer_ucmp3(scm_obj_t n1, scm_obj_t n2, scm_obj_t n3) {
  int count = 1;
  int n;
  if (BIGNUMP(n1)) {
    n = bn_get_count((scm_bignum_t)n1);
    if (n > count) count = n;
  }
  if (BIGNUMP(n2)) {
    n = bn_get_count((scm_bignum_t)n2);
    if (n > count) count = n;
  }
  if (BIGNUMP(n3)) {
    n = bn_get_count((scm_bignum_t)n3);
    if (n > count) count = n;
  }
  signed_digit2x_t acc = 0;
  for (n = count - 1; n >= 0; n--) {
    digit_t a = integer_nth_digit(n, n1);
    digit_t b = integer_nth_digit(n, n2);
    digit_t c = integer_nth_digit(n, n3);
    acc = acc + (signed_digit2x_t)a + (signed_digit2x_t)b - (signed_digit2x_t)c;
    if (acc > 0) return 1;
    if (acc < -1) return -1;
    acc <<= DIGIT_BIT;
  }
  return (int)acc;
}

static scm_obj_t integer_init_n_alloc(object_heap_t* heap, int64_t m, int shift_left) {
  assert(m >= 0);
  if (m == 0) return MAKEFIXNUM(0);
#if USE_DIGIT32
  int count = 2 + ((shift_left + (DIGIT_BIT - 1)) / DIGIT_BIT);
  scm_bignum_t bn = make_bignum(heap, count);
  bn->elts[0] = (digit_t)(m & 0xffffffff);
  bn->elts[1] = (digit_t)((digit2x_t)m >> DIGIT_BIT);
  memset(bn->elts + 2, 0, sizeof(digit_t) * (count - 2));
  bn_shift_left(bn, shift_left);
  bn_norm(bn);
  bn_set_sign(bn, 1);
  return bn_demote(bn);
#else
  int count = 1 + ((shift_left + (DIGIT_BIT - 1)) / DIGIT_BIT);
  scm_bignum_t bn = make_bignum(heap, count);
  bn->elts[0] = (digit_t)m;
  memset(bn->elts + 1, 0, sizeof(digit_t) * (count - 1));
  bn_shift_left(bn, shift_left);
  bn_norm(bn);
  bn_set_sign(bn, 1);
  return bn_demote(bn);
#endif
}

// Reference:
// Robert G. Burger and R. Kent Dybvig.
// Printing floatingpoint numbers quickly and accurately.
// In Proceedings of the ACM SIGPLAN '96 Conference on Programming Language Design and Implementation, pages 108--116.

scm_string_t cnvt_flonum_to_string(object_heap_t* heap, scm_flonum_t flonum, bool no_exponential) {
#if USE_SNPRINT_FOR_FLONUM
  char buf[32];
  double value = flonum->value;
  if (isnan(value)) return make_string(heap, (value > 0) ? "+nan.0" : "-nan.0");
  if (isinf(value)) return make_string(heap, (value > 0) ? "+inf.0" : "-inf.0");
  snprintf(buf, sizeof(buf), "%.20g", value);
  size_t n = strcspn(buf, ".eE");
  if (buf[n]) return make_string(heap, buf);
  strcat(buf, ".0");
  return make_string(heap, buf);
#else
  // note: can optimize by using (mp = mm * 2) rule, but its very rare
  double v = flonum->value;
  char digits[32];
  int digit_count = 0;
  int exponent;
  int e;
  int sign;
  int64_t f = decode_double(v, &e, &sign);
  if (v == 0.0) return make_string(heap, (sign > 0) ? "0.0" : "-0.0");
  if (isnan(v)) return make_string(heap, (sign > 0) ? "+nan.0" : "-nan.0");
  if (isinf(v)) return make_string(heap, (sign > 0) ? "+inf.0" : "-inf.0");
  if (sign == -1) v = -v;
  bool meven = ((f & 1) == 0);
  bool eq_mp_mm = true;
  int test;
  scm_obj_t r;
  scm_obj_t s;
  scm_obj_t mp;
  scm_obj_t mm;
  if (e >= 0) {
    scm_obj_t be = arith_expt(heap, MAKEFIXNUM(2), MAKEFIXNUM(e));
    if (f != iexpt_2n52) {
      r = integer_init_n_alloc(heap, f, e + 1);
      s = MAKEFIXNUM(2);
      mp = be;
      mm = be;
    } else {
      scm_obj_t be1 = arith_expt(heap, MAKEFIXNUM(2), MAKEFIXNUM(e + 1));
      r = integer_init_n_alloc(heap, f, e + 2);
      s = MAKEFIXNUM(4);
      mp = be1;
      mm = be;
      eq_mp_mm = false;
    }
  } else {
    if (e == -1023 || f != iexpt_2n52) {
      r = int64_to_integer(heap, f << 1);
      s = arith_expt(heap, MAKEFIXNUM(2), MAKEFIXNUM(1 - e));
      mp = MAKEFIXNUM(1);
      mm = MAKEFIXNUM(1);
    } else {
      r = int64_to_integer(heap, f << 2);
      s = arith_expt(heap, MAKEFIXNUM(2), MAKEFIXNUM(2 - e));
      mp = MAKEFIXNUM(2);
      mm = MAKEFIXNUM(1);
      eq_mp_mm = false;
    }
  }
  // scale
  int est = (int)(ceil(log10(v) - 0.1));
  if (est > 0) {
    s = arith_mul(heap, s, arith_expt(heap, MAKEFIXNUM(10), MAKEFIXNUM(est)));
  } else {
    scm_obj_t scale10 = arith_expt(heap, MAKEFIXNUM(10), MAKEFIXNUM(-est));
    r = arith_mul(heap, r, scale10);
    mp = arith_mul(heap, mp, scale10);
    mm = eq_mp_mm ? mp : arith_mul(heap, mm, scale10);
  }
  // fixup
  test = integer_ucmp3(r, mp, s);
  if (test > 0 || (meven && test == 0)) {
    s = integer_mul10_may_inplace(heap, s);
    exponent = est + 1;
  } else {
    exponent = est;
  }
  // generate
  BN_TEMPORARY(bn_q);
  BN_TEMPORARY(bn_s);
  BN_ALLOC_FIXNUM(bn_q);
  BN_ALLOC_FIXNUM(bn_s);
loop:
  mp = integer_mul10_may_inplace(heap, mp);
  mm = eq_mp_mm ? mp : integer_mul10_may_inplace(heap, mm);
  r = integer_mul10_may_inplace(heap, r);
  intptr_t dig = '0';
  test = n_compare(heap, r, s);
  if (test == 0) {
    dig += 1;
    r = MAKEFIXNUM(0);
  } else if (test > 0) {
    if (FIXNUMP(r)) {
      assert(FIXNUMP(s));
      intptr_t nq = FIXNUM(r) / FIXNUM(s);
      intptr_t nr = FIXNUM(r) - (nq * FIXNUM(s));
      dig += nq;
      r = MAKEFIXNUM(nr);
    } else {
      bn_div_ans_t ans;
      ans.quotient = &bn_q;
      ans.remainder = r;
      if (BIGNUMP(s)) {
        bn_div(heap, &ans, (scm_bignum_t)r, (scm_bignum_t)s);
      } else {
        bn_let(&bn_s, (scm_fixnum_t)s);
        bn_div(heap, &ans, (scm_bignum_t)r, &bn_s);
      }
      bn_set_sign((scm_bignum_t)ans.quotient, 1);
      bn_set_sign((scm_bignum_t)ans.remainder, 1);
      dig += FIXNUM(bn_demote((scm_bignum_t)ans.quotient));
      r = bn_demote((scm_bignum_t)ans.remainder);
    }
  }
  test = n_compare(heap, r, mm);
  bool tc1 = (test < 0 || (meven && test == 0));
  test = integer_ucmp3(r, mp, s);
  bool tc2 = (test > 0 || (meven && test == 0));
  if (!tc1) {
    if (!tc2) {
      digits[digit_count++] = dig;
      if (digit_count >= array_sizeof(digits)) {
        fatal("%s:%u something wrong", __FILE__, __LINE__);
      }
      goto loop;
    } else {
      digits[digit_count++] = dig + 1;
    }
  } else {
    if (!tc2) {
      digits[digit_count++] = dig;
    } else {
      if (integer_ucmp3(r, r, s) < 0) {
        digits[digit_count++] = dig;
      } else {
        digits[digit_count++] = dig + 1;
      }
    }
  }
  char out[512];
  int out_count = 0;
  digits[digit_count] = 0;
  if (no_exponential || (exponent >= -10 && exponent <= 10)) {
    if (sign == -1) out[out_count++] = '-';
    if (exponent <= 0) {
      out[out_count++] = '0';
      out[out_count++] = '.';
      while (++exponent <= 0) out[out_count++] = '0';
      for (int i = 0; digits[i] != 0; i++) out[out_count++] = digits[i];
    } else {
      for (int i = 0; digits[i] != 0; i++) {
        out[out_count++] = digits[i];
        if (--exponent == 0) out[out_count++] = '.';
      }
      if (exponent >= 0) {
        if (exponent == 0) {
          out[out_count++] = '0';
        } else {
          while (exponent > 0) {
            out[out_count++] = '0';
            exponent--;
          }
          out[out_count++] = '.';
          out[out_count++] = '0';
        }
      }
    }
    out[out_count] = 0;
  } else {
    if (sign == -1) out[out_count++] = '-';
    out[out_count++] = digits[0];
    if (digits[1]) out[out_count++] = '.';
    for (int i = 1; digits[i] != 0; i++) out[out_count++] = digits[i];
    out[out_count] = 0;
    snprintf(&out[out_count], sizeof(out) - out_count, "e%d", exponent - 1);
  }
  return make_string(heap, out);
}

scm_obj_t cnvt_number_to_string(object_heap_t* heap, scm_obj_t obj, int radix) {
  if (FIXNUMP(obj)) return cnvt_fixnum_to_string(heap, (scm_fixnum_t)obj, radix);
  if (BIGNUMP(obj)) return cnvt_bignum_to_string(heap, (scm_bignum_t)obj, radix);
  if (FLONUMP(obj)) {
    if (radix == 10) return cnvt_flonum_to_string(heap, (scm_flonum_t)obj, false);
    assert(false);
  }
  if (RATIONALP(obj)) {
    scm_obj_t nume = cnvt_number_to_string(heap, ((scm_rational_t)obj)->nume, radix);
    scm_obj_t deno = cnvt_number_to_string(heap, ((scm_rational_t)obj)->deno, radix);
    scm_string_t first = (scm_string_t)nume;
    scm_string_t second = (scm_string_t)deno;
    int size = first->size + second->size + 1;
    scm_string_t ans = make_string(heap, size, 0);
    char* dst = ans->name;
    strcpy(dst, first->name);
    dst += first->size;
    *dst++ = '/';
    strcpy(dst, second->name);
    dst += second->size;
    assert(ans->name + size == dst);
    assert(*dst == 0);
    return ans;
  }
  if (COMPLEXP(obj)) {
    scm_obj_t real = cnvt_number_to_string(heap, ((scm_complex_t)obj)->real, radix);
    scm_obj_t imag = cnvt_number_to_string(heap, ((scm_complex_t)obj)->imag, radix);
    scm_string_t first = (scm_string_t)real;
    scm_string_t second = (scm_string_t)imag;
    bool need_plus = (second->name[0] != '+' && second->name[0] != '-');
    int size = first->size + second->size + 1;
    if (need_plus) size++;
    scm_string_t ans = make_string(heap, size, 0);
    char* dst = ans->name;
    strcpy(dst, first->name);
    dst += first->size;
    if (need_plus) *dst++ = '+';
    strcpy(dst, second->name);
    dst += second->size;
    *dst++ = 'i';
    assert(ans->name + size == dst);
    assert(*dst == 0);
    return ans;
  }
  fatal("%s:%u wrong datum type", __FILE__, __LINE__);
#endif
}

static double nextfloat(double z) {
  int k;
  int sign;
  int64_t m = decode_double(z, &k, &sign);
  assert(sign >= 0);
  if (m == iexpt_2n53 - 1) return ldexp((double)iexpt_2n52, k + 1);
  return ldexp((double)(m + 1), k);
}

static double prevfloat(double z) {
  int k;
  int sign;
  int64_t m = decode_double(z, &k, &sign);
  assert(sign >= 0);
  if (m == iexpt_2n52) return ldexp((double)(iexpt_2n53 - 1), k - 1);
  return ldexp((double)(m - 1), k);
}

// Reference:
// William D. Clinger.
// How to read floating point numbers accurately
// Proceedings of the ACM SIGPLAN 1990 conference on Programming language design and implementation, p.92-101, June 1990

static double algorithmR(object_heap_t* heap, scm_obj_t f, const int e, const double z0) {
  double z = z0;
  scm_obj_t x0;
  scm_obj_t pow10e;
  if (e >= 0) {
    x0 = arith_mul(heap, f, arith_expt(heap, MAKEFIXNUM(10), MAKEFIXNUM(e)));
    pow10e = scm_unspecified;
  } else {
    x0 = scm_unspecified;
    pow10e = arith_expt(heap, MAKEFIXNUM(10), MAKEFIXNUM(-e));
  }
  while (1) {
    if (isinf(z)) return z;
    int k;
    int sign;
    int64_t m = decode_double(z, &k, &sign);
    assert(sign >= 0);
    scm_obj_t x;
    scm_obj_t y;
    if (e >= 0) {
      if (k >= 0) {
        x = x0;
        y = integer_init_n_alloc(heap, m, k);
      } else {
        x = arith_logash(heap, x0, MAKEFIXNUM(-k));
        y = int64_to_integer(heap, m);
      }
    } else {
      if (k >= 0) {
        x = f;
        y = arith_mul(heap, integer_init_n_alloc(heap, m, k), pow10e);
      } else {
        x = arith_logash(heap, f, MAKEFIXNUM(-k));
        y = arith_mul(heap, int64_to_integer(heap, m), pow10e);
      }
    }
    scm_obj_t D = arith_sub(heap, x, y);
    scm_obj_t D2 = arith_mul(heap, int64_to_integer(heap, m + m), D);
    bool negD = n_negative_pred(D);
    if (negD) {
      if (BIGNUMP(D2))
        bn_set_sign((scm_bignum_t)D2, -bn_get_sign((scm_bignum_t)D2));
      else
        D2 = MAKEFIXNUM(-FIXNUM(D2));
    }
    int test = n_compare(heap, D2, y);
    if (test < 0) {
      if (negD && m == iexpt_2n52 && integer_ucmp3(D2, D2, y) > 0) {
        z = prevfloat(z);
        continue;
      }
      return z;
    }
    if (test == 0) {
      if ((m & 1) == 0) {
        if (negD && m == iexpt_2n52) {
          z = prevfloat(z);
          continue;
        }
        return z;
      }
      return negD ? prevfloat(z) : nextfloat(z);
    }
    z = negD ? prevfloat(z) : nextfloat(z);
  }
}

static const char* parse_ubignum(object_heap_t* heap, const char* s, int radix, scm_obj_t* ans) {
  const char* p = s;
  int digit_count = 0;
  int workpad_count = BN_QUANTUM;
  BN_TEMPORARY(workpad);
  BN_ALLOC(workpad, workpad_count);
  bn_set_sign(&workpad, 0);
  memset(workpad.elts, 0, sizeof(digit_t) * workpad_count);
  if (*p) {
    char c;
    while ((c = *p++) != 0) {
      int digit;
      if (c == '#') return p - 1;
      if ((c >= '0') & (c <= '9'))
        digit = c - '0';
      else if (c >= 'a')
        digit = c - 'a' + 10;
      else if (c >= 'A')
        digit = c - 'A' + 10;
      else
        break;
      digit_count++;
      if (digit < radix) {
        BN_TEMPORARY(bn1);
        BN_TEMPORARY(bn2);
        bn1 = bn2 = workpad;
        bn_set_count(&bn1, workpad_count - 1);
        bn_mul_add_digit(&bn2, &bn1, radix, digit);
        if (workpad.elts[workpad_count - 1] == 0) continue;
        digit_t* save_elts = workpad.elts;
        int save_count = workpad_count;
        workpad_count += BN_QUANTUM;
        BN_ALLOC(workpad, workpad_count);
        memcpy(workpad.elts, save_elts, sizeof(digit_t) * save_count);
        memset(&workpad.elts[save_count], 0, sizeof(digit_t) * (workpad_count - save_count));
        continue;
      }
      break;
    }
    if (--p != s) {
      bn_set_sign(&workpad, 1);
      bn_norm(&workpad);
      *ans = bn_dup(heap, &workpad);
    }
    return p;
  }
  return p;
}

static const char* parse_uinteger(object_heap_t* heap, const char* s, int radix, scm_obj_t* ans) {
  int digit_count = 0;
  const char* p = s;
  if (*p) {
#if ARCH_LP64
    int128_t value = 0;
#else
    int64_t value = 0;
#endif
    char c;
    while ((c = *p++) != 0) {
      int digit;
      if (c == '#') return p - 1;
      if ((c >= '0') & (c <= '9'))
        digit = c - '0';
      else if (c >= 'a')
        digit = c - 'a' + 10;
      else if (c >= 'A')
        digit = c - 'A' + 10;
      else
        break;
      digit_count++;
      if (digit < radix) {
        value *= radix;
        value += digit;
        if (value <= FIXNUM_MAX) continue;
        return parse_ubignum(heap, s, radix, ans);
      }
      break;
    }
    if (--p != s) *ans = MAKEFIXNUM((intptr_t)value);
    return p;
  }
  return p;
}

static const char* parse_udecimal(object_heap_t* heap, const char* s, int radix, scm_obj_t* ans) {
  if (tolower(s[0]) == 'n' && strncasecmp(s + 1, "an.0", 4) == 0) {
    *ans = make_flonum(heap, VALUE_NAN);
    return s + 5;
  }
  if (tolower(s[0]) == 'i' && strncasecmp(s + 1, "nf.0", 4) == 0) {
    *ans = make_flonum(heap, VALUE_INF);
    return s + 5;
  }

  if ((*s == 0) | (radix != 10)) return s;
  const char* p = s;
  int digit_count = 0;
  int fraction_count = 0;
  int exponent = 0;
  bool exponent_negative = false;
  char c;
  int digit;
  scm_obj_t value = MAKEFIXNUM(0);
  while ((c = *p++) != 0) {
    if (c == '0') {
      digit_count++;
      continue;
    }
    p--;
    goto parse_integral;
  }
  if (--p != s) {
    *ans = make_flonum(heap, 0.0);
  }
  return p;
parse_integral:
  while ((c = *p++) != 0) {
    if (c == '#') return p - 1;
    if ((c >= '0') & (c <= '9')) {
      digit_count++;
      digit = c - '0';
      value = arith_add(heap, arith_mul(heap, value, MAKEFIXNUM(10)), MAKEFIXNUM(digit));
      continue;
    }
    if (c == '.') goto parse_fraction;
    if (strchr("esfdlESFDL", c)) goto parse_exponent;
    p--;
    goto parse_precision;  // hit none decimal char
  }
  goto parse_done;
parse_fraction:
  while ((c = *p++) != 0) {
    if (c == '#') return p - 1;
    if ((c >= '0') & (c <= '9')) {
      if (digit_count < P_DIGITS) {
        digit_count++;
        digit = c - '0';
        value = arith_add(heap, arith_mul(heap, value, MAKEFIXNUM(10)), MAKEFIXNUM(digit));
        fraction_count++;
      }
      continue;
    }
    if (strchr("esfdlESFDL", c)) goto parse_exponent;
    p--;
    goto parse_precision;  // hit none decimal char
  }
  p--;
  goto parse_done;
parse_exponent:
  if (*p == '|') {  // `10.2e|43`
    return p;
  }
  if (*p == '-') {
    exponent_negative = true;
    p++;
  } else if (*p == '+') {
    p++;
  }
  if (*p) {
    while ((c = *p++) != 0) {
      if ((c >= '0') & (c <= '9')) {
        digit = c - '0';
        exponent *= 10;
        exponent += digit;
        if (exponent < 0) fatal("%s:%u flonum exponent overflow.", __FILE__, __LINE__);
        continue;
      }
      p--;
      goto parse_precision;  // hit none decimal char
    }
  } else {
    return p;  // string end with `e`, `e+`, `e-` like `100.4e`, `100.4e-`
  }
  p--;
  goto parse_done;
parse_precision:
  if (c != '|') goto parse_done;
  scm_obj_t precision;
  p = parse_uinteger(heap, p + 1, 10, &precision);
parse_done:
  if (digit_count == 0) return p;  // string looks like `.`, `e`, `+.`, `.e+10`
  if (value == MAKEFIXNUM(0)) {
    *ans = make_flonum(heap, 0.0);
    return p;
  }
  if (exponent_negative) exponent = -exponent;
  exponent = exponent - fraction_count;
  double estimation = pow10n(real_to_double(value), exponent);
  *ans = make_flonum(heap, algorithmR(heap, value, exponent, estimation));
  return p;
}

static const char* parse_mantissa(object_heap_t* heap, const char* s, int radix, scm_obj_t* ans, int* digit_count, int* fraction_count) {
  bool fraction = false;
  const char* p = s;
  int workpad_count = BN_QUANTUM;
  BN_TEMPORARY(workpad);
  BN_ALLOC(workpad, workpad_count);
  bn_set_sign(&workpad, 0);
  memset(workpad.elts, 0, sizeof(digit_t) * workpad_count);
  if (*p) {
    char c;
    while ((c = *p++) != 0) {
      int digit;
      if (c == '#') return p - 1;
      if (c == '.') {
        if (fraction) break;
        fraction = true;
        continue;
      }
      if ((c >= '0') & (c <= '9'))
        digit = c - '0';
      else if (c >= 'a')
        digit = c - 'a' + 10;
      else if (c >= 'A')
        digit = c - 'A' + 10;
      else
        break;
      if (digit < radix) {
        if (fraction) *fraction_count = *fraction_count + 1;
        *digit_count = *digit_count + 1;
        BN_TEMPORARY(bn1);
        BN_TEMPORARY(bn2);
        bn1 = bn2 = workpad;
        bn_set_count(&bn1, workpad_count - 1);
        bn_mul_add_digit(&bn2, &bn1, radix, digit);
        if (workpad.elts[workpad_count - 1] == 0) continue;
        digit_t* save_elts = workpad.elts;
        int save_count = workpad_count;
        workpad_count += BN_QUANTUM;
        BN_ALLOC(workpad, workpad_count);
        memcpy(workpad.elts, save_elts, sizeof(digit_t) * save_count);
        memset(&workpad.elts[save_count], 0, sizeof(digit_t) * (workpad_count - save_count));
        continue;
      }
      break;
    }
    if (--p != s) {
      bn_set_sign(&workpad, 1);
      bn_norm(&workpad);
      *ans = bn_dup(heap, &workpad);
    }
    return p;
  }
  return p;
}

static const char* parse_exact_udecimal(object_heap_t* heap, const char* s, int radix, scm_obj_t* ans) {
  if (*s == 0) return s;
  const char* p = s;
  int digit_count = 0;
  int fraction_count = 0;
  int leading_zero_count = 0;
  bool exponent_negative = false;
  scm_bignum_t mantissa;
  scm_obj_t exponent = MAKEFIXNUM(0);
  char c;
  while ((c = *p++) != 0) {
    if (c == '0') {
      leading_zero_count++;
      continue;
    }
    p--;
    goto parse_digits;
  }
  if (--p != s) {
    *ans = MAKEFIXNUM(0);
  }
  return p;
parse_digits:
  scm_obj_t temp = scm_false;
  p = parse_mantissa(heap, p, radix, &temp, &digit_count, &fraction_count);
  if (temp == scm_false) return p;
  assert(BIGNUMP(temp));
  mantissa = (scm_bignum_t)temp;
  if (*p && strchr("esfdlESFDL", *p)) {
    p++;
    if (*p == '-') {
      exponent_negative = true;
      p++;
    } else if (*p == '+') {
      p++;
    }
    if (*p) {
      p = parse_uinteger(heap, p, radix, &exponent);
    } else {
      return p;  // string end with `e`, `e+`, `e-` like `100.4e`, `100.4e-`
    }
  }
  if (leading_zero_count + digit_count == 0) return p;  // string looks like `.`, `e`, `+.`, `.e+10`
  if (!FIXNUMP(exponent)) return p;
  int exp10 = (exponent_negative ? -FIXNUM(exponent) : FIXNUM(exponent)) - fraction_count;
  // #e1.00e100
  if (exp10 == 0) {
    *ans = oprtr_norm_integer(heap, mantissa);
    return p;
  }
  if (exp10 < 0) {
    exp10 = -exp10;
    int count = 1;
    double e = pow10n(1, exp10);
    if (isinf(e)) {
      count = count + exp10 / 9;
    } else {
      while (floor(e) != 0.0) {
        e /= 4294967296.0;
        count++;
      }
    }
    BN_TEMPORARY(bn);
    BN_ALLOC(bn, count);
    memset(bn.elts, 0, sizeof(digit_t) * count);
    bn_set_sign(&bn, 1);
    bn.elts[0] = 1;
    BN_TEMPORARY(workpad);
    while (exp10 >= 9) {
      bn_set_count(&bn, count);
      workpad = bn;
      bn_norm(&workpad);
      bn_mul_add_digit(&bn, &workpad, 1000000000U, 0);
      exp10 -= 9;
    }
    if (exp10) {
      uint32_t n = 10;
      for (int i = 1; i < exp10; i++) n *= 10;
      bn_set_count(&bn, count);
      workpad = bn;
      bn_norm(&workpad);
      bn_mul_add_digit(&bn, &workpad, n, 0);
    }
    *ans = oprtr_reduce(heap, oprtr_norm_integer(heap, mantissa), oprtr_norm_integer(heap, bn_dup(heap, &bn)));
  } else {
    int count = 1;
    double e = pow10n(bignum_to_double(mantissa), exp10);
    if (isinf(e)) {
      count = count + exp10 / 9;
    } else {
      while (floor(e) != 0.0) {
        e /= 4294967296.0;
        count++;
      }
    }
    BN_TEMPORARY(bn);
    BN_ALLOC(bn, count);
    memset(bn.elts, 0, sizeof(digit_t) * count);
    bn_set_sign(&bn, 1);
    assert(count > bn_get_count(mantissa));
    memcpy(bn.elts, mantissa->elts, sizeof(digit_t) * bn_get_count(mantissa));
    BN_TEMPORARY(workpad);
    while (exp10 >= 9) {
      bn_set_count(&bn, count);
      workpad = bn;
      bn_norm(&workpad);
      bn_mul_add_digit(&bn, &workpad, 1000000000U, 0);
      exp10 -= 9;
    }
    if (exp10) {
      uint32_t n = 10;
      for (int i = 1; i < exp10; i++) n *= 10;
      bn_set_count(&bn, count);
      workpad = bn;
      bn_norm(&workpad);
      bn_mul_add_digit(&bn, &workpad, n, 0);
    }
    *ans = bn_to_integer(heap, &bn);
  }
  return p;
}

static const char* parse_ureal(object_heap_t* heap, const char* s, int radix, bool exact, scm_obj_t* ans) {
  if (radix == 10) {
    size_t n = strcspn(s, ".esfdlESFDL|");
    if (s[n] && strcspn(s, "/") > n) {
      if (exact) return parse_exact_udecimal(heap, s, radix, ans);
      return parse_udecimal(heap, s, radix, ans);
    }
  }
  return parse_uinteger(heap, s, radix, ans);
}

static scm_obj_t parse_negate(object_heap_t* heap, scm_obj_t obj) {
  if (FIXNUMP(obj)) {
    intptr_t n = FIXNUM(obj);
    if (n == FIXNUM_MIN) return intptr_to_integer(heap, -n);
    return MAKEFIXNUM(-n);
  }
  if (FLONUMP(obj)) {
    scm_flonum_t fn = (scm_flonum_t)obj;
#if USE_FLONUM_CONST
    if (fn->value == 0.0) return make_flonum(heap, -fn->value);
#endif
    fn->value = -fn->value;
    return fn;
  } else if (BIGNUMP(obj)) {
    scm_bignum_t bn = (scm_bignum_t)obj;
    bn_set_sign(bn, -bn_get_sign(bn));
    if (bn_get_count(bn) == 1) {
      int64_t n = bn->elts[0];
      if (bn_get_sign(bn) < 0) n = -n;
      if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
    }
    return bn;
  } else if (RATIONALP(obj)) {
    scm_rational_t rn = (scm_rational_t)obj;
    rn->nume = parse_negate(heap, rn->nume);
    return rn;
  } else if (COMPLEXP(obj)) {
    scm_complex_t cn = (scm_complex_t)obj;
    cn->real = parse_negate(heap, cn->real);
    cn->imag = parse_negate(heap, cn->imag);
    return cn;
  } else {
    fatal("%s:%u not number.", __FILE__, __LINE__);
  }
}

scm_obj_t parse_number(object_heap_t* heap, const char* s, int prefix, int radix) {
  // prefix should be { e | E | i | I | 0 },
  // radix should be { b | B | o | O | d | D | x | X | 16 | 10 | 8 | 2 | 0 }
  bool negative = false;
  bool nosign = true;
  bool exact = false;
  bool inexact = false;
  switch (prefix) {
    case 'e':
    case 'E':
      exact = true;
      break;
    case 'i':
    case 'I':
      inexact = true;
      break;
  }
  switch (radix) {
    case 'b':
    case 'B':
      radix = 2;
      break;
    case 'o':
    case 'O':
      radix = 8;
      break;
    case 'd':
    case 'D':
      radix = 10;
      break;
    case 'x':
    case 'X':
      radix = 16;
      break;
  }
  while (s[0] == '#') {
    switch (s[1]) {
      case 'i':
      case 'I':
        if (exact | inexact) return scm_false;
        inexact = true;
        break;
      case 'e':
      case 'E':
        if (exact | inexact) return scm_false;
        exact = true;
        break;
      case 'b':
      case 'B':
        if (radix) return scm_false;
        radix = 2;
        break;
      case 'o':
      case 'O':
        if (radix) return scm_false;
        radix = 8;
        break;
      case 'd':
      case 'D':
        if (radix) return scm_false;
        radix = 10;
        break;
      case 'x':
      case 'X':
        if (radix) return scm_false;
        radix = 16;
        break;
      default:
        return scm_false;
    }
    s += 2;
  }
  if (radix == 0) radix = 10;
  if (s[0] == '-') {
    s++;
    if ((s[0] == 'i') | (s[0] == 'I')) {
      if (s[1] == 0) {
        if (inexact)
          return make_complex(heap, make_flonum(heap, 0.0), make_flonum(heap, -1.0));
        else
          return make_complex(heap, MAKEFIXNUM(0), MAKEFIXNUM(-1));  // -i
      }
      // -inf.0 or error
    }
    negative = true;
    nosign = false;
  } else if (s[0] == '+') {
    s++;
    if ((s[0] == 'i') | (s[0] == 'I')) {
      if (s[1] == 0) {
        if (inexact)
          return make_complex(heap, make_flonum(heap, 0.0), make_flonum(heap, 1.0));
        else
          return make_complex(heap, MAKEFIXNUM(0), MAKEFIXNUM(1));  // +i
      }
      // +inf.0 or error
    }
    nosign = false;
  }
  scm_obj_t real = scm_false;
  scm_obj_t imag = scm_false;
  scm_obj_t angle = scm_false;
  s = parse_ureal(heap, s, radix, exact, &real);
  if (real != scm_false) {
    if (negative) real = parse_negate(heap, real);
    if (s[0] == '/') {
      if (FIXNUMP(real) || BIGNUMP(real)) {
        scm_obj_t denominator = scm_false;
        s = parse_uinteger(heap, s + 1, radix, &denominator);
        if (denominator == scm_false) return scm_false;
        if (n_zero_pred(denominator)) return scm_false;
        real = oprtr_reduce(heap, real, denominator);
      } else {
        return scm_false;
      }
    }
    if (exact) real = cnvt_to_exact(heap, real);
    if (inexact) real = cnvt_to_inexact(heap, real);
    switch (s[0]) {
      case 0:
        return real;  // <sign> <ureal R>
      case '@':
        if (s[1] == '+') {
          s += 2;
          negative = false;
          goto angle_part;
        }
        if (s[1] == '-') {
          s += 2;
          negative = true;
          goto angle_part;
        }
        s++;
        goto angle_part;
      case 'I':
      case 'i':
        if (s[1]) return scm_false;    // error: extra char
        if (nosign) return scm_false;  // error: <ureal> i
        if (exact) return oprtr_norm_complex(heap, MAKEFIXNUM(0), cnvt_to_exact(heap, real));
        if (inexact) return oprtr_norm_complex(heap, make_flonum(heap, 0.0), cnvt_to_inexact(heap, real));
        return oprtr_norm_complex(heap, MAKEFIXNUM(0), real);
      case '+':
        if ((s[1] == 'i') | (s[1] == 'I')) {
          if (s[2] == 0) {
            if (exact) return oprtr_norm_complex(heap, cnvt_to_exact(heap, real), MAKEFIXNUM(1));
            if (inexact) return oprtr_norm_complex(heap, cnvt_to_inexact(heap, real), make_flonum(heap, 1.0));
            return oprtr_norm_complex(heap, real, MAKEFIXNUM(1));
          }
          // +inf.0 or error
        }
        s++;
        negative = false;
        goto imag_part;
      case '-':
        if ((s[1] == 'i') | (s[1] == 'I')) {
          if (s[2] == 0) {
            if (exact) return oprtr_norm_complex(heap, cnvt_to_exact(heap, real), MAKEFIXNUM(-1));
            if (inexact) return oprtr_norm_complex(heap, cnvt_to_inexact(heap, real), make_flonum(heap, -1.0));
            return oprtr_norm_complex(heap, real, MAKEFIXNUM(-1));
          }
          // -inf.0 or error
        }
        s++;
        negative = true;
        goto imag_part;
      default:
        return scm_false;
    }
  }
  return scm_false;
imag_part:
  s = parse_ureal(heap, s, radix, exact, &imag);
  if (imag != scm_false) {
    if (negative) imag = parse_negate(heap, imag);
    if (s[0] == '/') {
      if (FIXNUMP(imag) || BIGNUMP(imag)) {
        scm_obj_t denominator = scm_false;
        s = parse_uinteger(heap, s + 1, radix, &denominator);
        if (denominator == scm_false) return scm_false;
        imag = oprtr_reduce(heap, imag, denominator);
      } else {
        return scm_false;
      }
    }
    if (((s[0] == 'i') | (s[0] == 'I')) && s[1] == 0) {
      if (exact) return oprtr_norm_complex(heap, real, cnvt_to_exact(heap, imag));
      if (inexact) return oprtr_norm_complex(heap, real, cnvt_to_inexact(heap, imag));
      return oprtr_norm_complex(heap, real, imag);
    }
    return scm_false;
  }
  return scm_false;
angle_part:
  s = parse_ureal(heap, s, radix, exact, &angle);
  if (angle != scm_false) {
    if (negative) angle = parse_negate(heap, angle);
    if (s[0] == '/') {
      if (FIXNUMP(angle) || BIGNUMP(angle)) {
        scm_obj_t denominator = scm_false;
        s = parse_uinteger(heap, s + 1, radix, &denominator);
        if (denominator == scm_false) return scm_false;
        angle = oprtr_reduce(heap, angle, denominator);
      } else {
        return scm_false;
      }
    }
    if (s[0] == 0) {
      if (n_zero_pred(angle)) return real;
      return arith_polar(heap, real, angle);
    }
    return scm_false;
  }
  return scm_false;
}

scm_obj_t decode_flonum(object_heap_t* heap, scm_flonum_t n) {
  int exp;
  int sign;
  double value = n->value;
  int64_t mant = decode_double(value, &exp, &sign);
  return make_list(heap, 3, int64_to_integer(heap, mant), int32_to_integer(heap, exp), int32_to_integer(heap, sign));
}
