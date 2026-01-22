// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "utf8.h"
#include "object_heap.h"

int ascii_cstring_pred(const char* s) {
  uint8_t c;
  while ((c = *s++) != 0) {
    if (c > 0x7f) return 0;
  }
  return 1;
}

int cnvt_ucs4_to_utf8(uint32_t ucs4, uint8_t utf8[4]) {
  if (ucs4 >= 0xd800 && ucs4 <= 0xdfff) {
    fatal("cnvt_ucs4_to_utf8() excluded range: %x", ucs4);
  }
  if (ucs4 < 0x80) {
    utf8[0] = ucs4;
    return 1;
  }
  if (ucs4 < 0x800) {
    utf8[0] = ((ucs4 >> 6) & 0x1f) | 0xc0;
    utf8[1] = ((ucs4) & 0x3f) | 0x80;
    return 2;
  }
  if (ucs4 < 0x10000) {
    utf8[0] = ((ucs4 >> 12) & 0x0f) | 0xe0;
    utf8[1] = ((ucs4 >> 6) & 0x3f) | 0x80;
    utf8[2] = ((ucs4) & 0x3f) | 0x80;
    return 3;
  }
  if (ucs4 < 0x200000) {
    utf8[0] = ((ucs4 >> 18) & 0x07) | 0xf0;
    utf8[1] = ((ucs4 >> 12) & 0x3f) | 0x80;
    utf8[2] = ((ucs4 >> 6) & 0x3f) | 0x80;
    utf8[3] = ((ucs4) & 0x3f) | 0x80;
    return 4;
  }
  fatal("cnvt_ucs4_to_utf8() out of range: %x", ucs4);
}

int cnvt_utf8_to_ucs4(const uint8_t* utf8, uint32_t* ucs4) {
  int sv;
  if (utf8[0] < 0x80) {
    sv = utf8[0];
    if (sv >= 0x80) return -1;  // invalid sequence
    *ucs4 = sv;
    return 1;
  } else if (utf8[0] < 0xc2) {
    return -1;  // invalid sequence
  } else if (utf8[0] < 0xe0) {
    if ((utf8[1] < 0x80) | (utf8[1] > 0xbf)) return -1;
    sv = ((utf8[0] & 0x1f) << 6) + (utf8[1] & 0x3f);
    if ((sv < 0x80) | (sv > 0x7FF)) return -1;  // invalid sequence
    *ucs4 = sv;
    return 2;
  } else if (utf8[0] < 0xf0) {
    if ((utf8[1] < 0x80) | (utf8[1] > 0xbf)) return -1;
    if ((utf8[2] < 0x80) | (utf8[2] > 0xbf)) return -1;
    sv = ((utf8[0] & 0x0f) << 12) + ((utf8[1] & 0x3f) << 6) + (utf8[2] & 0x3f);
    if ((sv < 0x800) | (sv > 0xFFFF)) return -1;     // invalid sequence
    if ((sv >= 0xD800) & (sv <= 0xDFFF)) return -1;  // SURROGATE AREA
    // if (sv >= 0xFFFE) return -1;                     // NONCHARACTERS
    *ucs4 = sv;
    return 3;
  } else if (utf8[0] < 0xf8) {
    if ((utf8[1] < 0x80) | (utf8[1] > 0xbf)) return -1;
    if ((utf8[2] < 0x80) | (utf8[2] > 0xbf)) return -1;
    if ((utf8[3] < 0x80) | (utf8[3] > 0xbf)) return -1;
    sv = ((utf8[0] & 0x07) << 18) + ((utf8[1] & 0x3f) << 12) + ((utf8[2] & 0x3f) << 6) + (utf8[3] & 0x3f);
    if ((sv < 0x10000) | (sv > 0x10FFFF)) return -1;  // non-assignment
    *ucs4 = sv;
    return 4;
  }
  return -1;
}

int utf8_sizeof_ucs4(uint32_t ucs4) {
  if (ucs4 < 0x80) return 1;
  if (ucs4 < 0x800) return 2;
  if (ucs4 < 0x10000) return 3;
  if (ucs4 < 0x200000) return 4;
  fatal("utf8_sizeof_ucs4() out of range");
}

int utf8_byte_count(const uint8_t datum) {
  if (datum < 0x80) return 1;
  if (datum < 0xc2) return 1;  // cnvt_utf8_to_ucs4() detect this
  if (datum < 0xe0) return 2;
  if (datum < 0xf0) return 3;
  if (datum < 0xf8) return 4;
  if (datum < 0xfc) return 5;
  return 6;
}

int utf8_char_index_to_byte_offset(const uint8_t datum[], int index, int limit) {
  int n = 0;
  for (int c = 0; c < index && n < limit; c++) n += utf8_byte_count(datum[n]);
  if ((index < 0) || (n >= limit)) return -1;
  return n;
}
