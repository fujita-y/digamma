// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "printer.h"

void printer_t::format(scm_obj_t obj) {
  if (is_fixnum(obj)) {
    out << fixnum(obj);
    return;
  }

  if (is_short_flonum(obj)) {
    out << flonum(obj);
    return;
  }

  if (is_heap_object(obj)) {
    if (is_symbol(obj)) {
      std::string s = (char*)symbol_name(obj);
      bool special = false;
      if (s.empty())
        special = true;
      else {
        if (isdigit(s[0])) special = true;  // simplified check
        for (char c : s) {
          if (!isalnum(c) && !strchr("!$%&*+-./:<=>?@^_~", c)) {
            special = true;
            break;
          }
        }
      }
      if (special) {
        out << '|';
        for (char c : s) {
          if (c == '|' || c == '\\') out << '\\';
          out << c;
        }
        out << '|';
      } else {
        out << s;
      }
      return;
    }
    if (is_string(obj)) {
      out << '"';
      std::string s = (char*)string_name(obj);
      for (char c : s) {
        if (c == '"')
          out << "\\\"";
        else if (c == '\\')
          out << "\\\\";
        else
          out << c;
      }
      out << '"';
      return;
    }
    if (is_long_flonum(obj)) {
      out << flonum(obj);
      return;
    }
    if (is_vector(obj)) {
      out << "#(";
      int n = vector_nsize(obj);
      scm_obj_t* elts = vector_elts(obj);
      for (int i = 0; i < n; i++) {
        if (i > 0) out << " ";
        format(elts[i]);
      }
      out << ")";
      return;
    }
    if (is_u8vector(obj)) {
      out << "#u8(";
      int n = u8vector_nsize(obj);
      uint8_t* elts = u8vector_elts(obj);
      for (int i = 0; i < n; i++) {
        if (i > 0) out << " ";
        out << (int)elts[i];
      }
      out << ")";
      return;
    }
  }

  if (is_cons(obj)) {
    out << "(";
    format(((scm_cons_rec_t*)obj)->car);
    obj = ((scm_cons_rec_t*)obj)->cdr;
    while (is_cons(obj)) {
      out << " ";
      format(((scm_cons_rec_t*)obj)->car);
      obj = ((scm_cons_rec_t*)obj)->cdr;
    }
    if (obj != scm_nil) {
      out << " . ";
      format(obj);
    }
    out << ")";
    return;
  }

  // Immediate constants
  if (obj == scm_true) {
    out << "#t";
    return;
  }
  if (obj == scm_false) {
    out << "#f";
    return;
  }
  if (obj == scm_nil) {
    out << "()";
    return;
  }
  if (obj == scm_undef) {
    out << "#<undef>";
    return;
  }
  if (obj == scm_unspecified) {
    out << "#<unspecified>";
    return;
  }
  if (obj == scm_eof) {
    out << "#<eof>";
    return;
  }

  if (is_char(obj)) {
    out << "#\\";
    uint32_t c = (uint32_t)(obj >> 32);
    if (c > 32 && c < 127) {
      out << (char)c;
    } else {
      switch (c) {
        case ' ':
          out << "space";
          break;
        case '\n':
          out << "newline";
          break;
        case '\r':
          out << "return";
          break;
        case '\t':
          out << "tab";
          break;
        default:
          out << "x" << std::hex << c << std::dec;
          break;
      }
    }
    return;
  }

  out << "#<unknown 0x" << std::hex << obj << std::dec << ">";
}
