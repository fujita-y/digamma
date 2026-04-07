// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#include "core.h"

int utf8_byte_count(const uint8_t datum);
int utf8_string_length(const uint8_t* utf8);
int cnvt_ucs4_to_utf8(uint32_t ucs4, uint8_t utf8[4]);
int cnvt_utf8_to_ucs4(const uint8_t* utf8, uint32_t* ucs4);
int utf8_char_index_to_byte_offset(const uint8_t datum[], int index, int limit);
int utf8_sizeof_ucs4(uint32_t ucs4);

#endif
