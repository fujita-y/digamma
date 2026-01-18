// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_H_INCLUDED
#define OBJECT_H_INCLUDED

#include "core.h"

typedef void* scm_obj_t;

/*

flonum : 1fffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff
fixnum : 0100000- -------- -------- -------- iiiiiiii iiiiiiii iiiiiiii iiiiiiii
char   : 0100001- -------- -------- -------- cccccccc cccccccc cccccccc cccccccc
true   : 0100010- -------- -------- -------- -------- -------- -------- --------
false  : 0100011- -------- -------- -------- -------- -------- -------- --------
nil    : 0100100- -------- -------- -------- -------- -------- -------- --------
undef  : 0100101- -------- -------- -------- -------- -------- -------- --------
unspec : 0100110- -------- -------- -------- -------- -------- -------- --------
eof    : 0100111- -------- -------- -------- -------- -------- -------- --------
pair   : 0000000p pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp
symbol : 0000001p pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp
string : 0000010p pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp
vector : 0000011p pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp
closure: 0000100p pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp

*/

// clang-format off

namespace {

constexpr uintptr_t tag_mask        = (uintptr_t)0b1111111 << 57;
constexpr uintptr_t tag_fixnum      = (uintptr_t)0b0100000 << 57;
constexpr uintptr_t tag_char        = (uintptr_t)0b0100001 << 57;
constexpr uintptr_t tag_true        = (uintptr_t)0b0100010 << 57;
constexpr uintptr_t tag_false       = (uintptr_t)0b0100011 << 57;
constexpr uintptr_t tag_nil         = (uintptr_t)0b0100100 << 57;
constexpr uintptr_t tag_undef       = (uintptr_t)0b0100101 << 57;
constexpr uintptr_t tag_unspecified = (uintptr_t)0b0100110 << 57;
constexpr uintptr_t tag_eof         = (uintptr_t)0b0100111 << 57;
constexpr uintptr_t tag_symbol      = (uintptr_t)0b0000001 << 57;
constexpr uintptr_t tag_string      = (uintptr_t)0b0000010 << 57;
constexpr uintptr_t tag_vector      = (uintptr_t)0b0000011 << 57;
constexpr uintptr_t tag_closure     = (uintptr_t)0b0000100 << 57;

inline bool is_tagged(scm_obj_t x, uintptr_t tag) { return ((uintptr_t)x & tag_mask) == tag; }
inline scm_obj_t tag(uintptr_t x, uintptr_t tag) { return (scm_obj_t)((x & ~tag_mask) | tag); }
inline uintptr_t untag(uintptr_t x) { return (uintptr_t)x & ~tag_mask; }

}  // namespace

inline bool flonum_p(scm_obj_t x)  { return (intptr_t)x < 0; }
inline bool pair_p(scm_obj_t x)    { return ((uintptr_t)x & tag_mask) == 0; }
inline bool fixnum_p(scm_obj_t x)  { return is_tagged(x, tag_fixnum); }
inline bool char_p(scm_obj_t x)    { return is_tagged(x, tag_char); }
inline bool symbol_p(scm_obj_t x)  { return is_tagged(x, tag_symbol); }
inline bool string_p(scm_obj_t x)  { return is_tagged(x, tag_string); }
inline bool vector_p(scm_obj_t x)  { return is_tagged(x, tag_vector); }
inline bool closure_p(scm_obj_t x) { return is_tagged(x, tag_closure); }


const scm_obj_t scm_true        = (scm_obj_t)tag_true;
const scm_obj_t scm_false       = (scm_obj_t)tag_false;
const scm_obj_t scm_nil         = (scm_obj_t)tag_nil;
const scm_obj_t scm_undef       = (scm_obj_t)tag_undef;
const scm_obj_t scm_unspecified = (scm_obj_t)tag_unspecified;
const scm_obj_t scm_eof         = (scm_obj_t)tag_eof;

// clang-format on

inline scm_obj_t make_flonum(double x) { return (scm_obj_t)((__builtin_bit_cast(uintptr_t, x) >> 1) | 0x8000000000000000ULL); }
inline double flonum(scm_obj_t x) { return __builtin_bit_cast(double, (uintptr_t)x << 1); }

inline scm_obj_t make_fixnum(intptr_t x) { return tag((uintptr_t)x, tag_fixnum); }
inline int32_t fixnum(scm_obj_t x) { return (int32_t)((uintptr_t)x & 0xffffffff); }

#endif
