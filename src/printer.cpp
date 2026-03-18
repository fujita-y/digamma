// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "printer.h"

#include <format>

void printer_t::write(scm_obj_t obj) { print(obj, false); }

void printer_t::display(scm_obj_t obj) { print(obj, true); }

void printer_t::print(scm_obj_t obj, bool display_mode) {
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
      if (display_mode) {
        out << s;
        return;
      }
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
      if (display_mode) {
        out << (char*)string_name(obj);
        return;
      }
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
        print(elts[i], display_mode);
      }
      out << ")";
      return;
    }
    if (is_values(obj)) {
      out << "#<values";
      int n = values_nsize(obj);
      scm_obj_t* elts = values_elts(obj);
      for (int i = 0; i < n; i++) {
        out << " ";
        print(elts[i], display_mode);
      }
      out << ">";
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
    if (is_closure(obj)) {
      out << std::format("#<closure {:#x} argc:{} rest:{} nenv:{}>", (uintptr_t)obj, closure_argc(obj), closure_rest(obj), closure_nenv(obj));
      return;
    }
    if (is_environment(obj)) {
      out << std::format("#<environment {:#x}>", (uintptr_t)obj);
      return;
    }
    if (is_cell(obj)) {
      out << std::format("#<cell {:#x}>", (uintptr_t)obj);
      return;
    }
    if (is_escape(obj)) {
      out << std::format("#<escape {:#x}>", (uintptr_t)obj);
      return;
    }
    if (is_continuation(obj)) {
      out << std::format("#<continuation {:#x}>", (uintptr_t)obj);
      return;
    }
  }

  if (is_cons(obj)) {
    if (obj == (scm_obj_t) nullptr) {
      out << "#<null>";
      return;
    }
    out << "(";
    print(((scm_cons_rec_t*)obj)->car, display_mode);
    obj = ((scm_cons_rec_t*)obj)->cdr;
    while (is_cons(obj)) {
      if (obj == (scm_obj_t) nullptr) {
        out << "#<null>";
        return;
      }
      out << " ";
      print(((scm_cons_rec_t*)obj)->car, display_mode);
      obj = ((scm_cons_rec_t*)obj)->cdr;
    }
    if (obj != scm_nil) {
      out << " . ";
      print(obj, display_mode);
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
    uint32_t c = (uint32_t)(obj >> 32);
    if (display_mode) {
      out << (char)c;
      return;
    }
    out << "#\\";
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

void printer_t::format(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  const char* p = fmt;
  while (*p) {
    if (*p == '%') {
      p++;
      if (*p == '\0') {
        // format string ends with '%'
        out << '%';
        break;
      }
      if (*p == '%') {
        // '%%' -> '%'
        out << '%';
        p++;
        continue;
      }
      if (*p == 'w') {
        // '%w' -> format a Scheme object
        scm_obj_t obj = va_arg(ap, scm_obj_t);
        write(obj);
        p++;
        continue;
      }
      // Handle standard printf directives
      // We'll collect the format spec and use sprintf for standard types
      const char* spec_start = p - 1;  // points to '%'

      // Skip flags: -, +, space, #, 0
      while (*p == '-' || *p == '+' || *p == ' ' || *p == '#' || *p == '0') {
        p++;
      }

      // Skip width
      while (isdigit(*p)) {
        p++;
      }

      // Skip precision
      if (*p == '.') {
        p++;
        while (isdigit(*p)) {
          p++;
        }
      }

      // Skip length modifiers: h, l, L, z, t, etc.
      while (*p == 'h' || *p == 'l' || *p == 'L' || *p == 'z' || *p == 't' || *p == 'j') {
        p++;
      }

      // Now we should be at the conversion specifier
      if (*p == '\0') {
        // Incomplete format specifier
        break;
      }

      char conversion = *p;
      p++;

      // Extract the format specifier
      size_t spec_len = p - spec_start;
      char spec[64];
      if (spec_len >= sizeof(spec)) {
        spec_len = sizeof(spec) - 1;
      }
      memcpy(spec, spec_start, spec_len);
      spec[spec_len] = '\0';

      char buf[256];
      switch (conversion) {
        case 'd':
        case 'i': {
          int val = va_arg(ap, int);
          snprintf(buf, sizeof(buf), spec, val);
          out << buf;
          break;
        }
        case 'u':
        case 'o':
        case 'x':
        case 'X': {
          unsigned int val = va_arg(ap, unsigned int);
          snprintf(buf, sizeof(buf), spec, val);
          out << buf;
          break;
        }
        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G': {
          double val = va_arg(ap, double);
          snprintf(buf, sizeof(buf), spec, val);
          out << buf;
          break;
        }
        case 'c': {
          int val = va_arg(ap, int);
          snprintf(buf, sizeof(buf), spec, val);
          out << buf;
          break;
        }
        case 's': {
          const char* val = va_arg(ap, const char*);
          snprintf(buf, sizeof(buf), spec, val);
          out << buf;
          break;
        }
        case 'p': {
          void* val = va_arg(ap, void*);
          snprintf(buf, sizeof(buf), spec, val);
          out << buf;
          break;
        }
        default:
          // Unknown conversion, just output the spec as-is
          out << spec;
          break;
      }
    } else {
      // Regular character
      out << *p;
      p++;
    }
  }

  va_end(ap);
}
