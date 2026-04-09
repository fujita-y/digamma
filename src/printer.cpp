// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "printer.h"

#include <format>
#include <unordered_map>

void printer_t::write(scm_obj_t obj) { print(nullptr, obj, false); }

void printer_t::display(scm_obj_t obj) { print(nullptr, obj, true); }

void printer_t::print(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj, bool display_mode) {
  if (is_fixnum(obj)) {
    out << fixnum(obj);
    return;
  }

  if (is_short_flonum(obj)) {
    out << flonum(obj);
    return;
  }

  // SRFI-38: handle shared/circular structure labels for heap and cons objects
  if (visited != nullptr && (is_heap_object(obj) || is_cons(obj))) {
    auto it = visited->find(obj);
    if (it != visited->end()) {
      if (is_fixnum(it->second)) {
        // Back-reference: object already defined above
        out << "#" << fixnum(it->second) << "#";
        return;
      } else {
        // it->second == scm_true: shared, assign new tag
        int tag = m_shared_tag++;
        it->second = make_fixnum(tag);
        out << "#" << tag << "=";
        // Fall through to print the object body with the label
      }
    }
    // it == end(): not shared, print normally (fall through)
  }

  if (is_heap_object(obj)) {
    switch (heap_tc6_num(obj)) {
      case tc6_symbol: {
        const char* s = (const char*)symbol_name(obj);
        if (display_mode) {
          out << s;
          return;
        }
        std::string_view sv(s);
        bool special = sv.empty() || isdigit((unsigned char)sv[0]);
        if (!special) {
          for (char c : sv) {
            if (!isalnum((unsigned char)c) && !strchr("!$%&*+-./:<=>?@^_~", c)) {
              special = true;
              break;
            }
          }
        }
        if (special) {
          out << '|';
          for (char c : sv) {
            if (c == '|' || c == '\\') out << '\\';
            out << c;
          }
          out << '|';
        } else
          out << sv;
        return;
      }
      case tc6_string: {
        const char* s = (const char*)string_name(obj);
        if (display_mode) {
          out << s;
          return;
        }
        out << '"';
        for (const char* p = s; *p; p++) {
          if (*p == '"')
            out << "\\\"";
          else if (*p == '\\')
            out << "\\\\";
          else
            out << *p;
        }
        out << '"';
        return;
      }
      case tc6_long_flonum: {
        out << flonum(obj);
        return;
      }
      case tc6_vector: {
        out << "#(";
        int n = vector_nsize(obj);
        scm_obj_t* elts = vector_elts(obj);
        for (int i = 0; i < n; i++) {
          if (i > 0) out << " ";
          print(visited, elts[i], false);
        }
        out << ")";
        return;
      }
      case tc6_values: {
        out << "#<values";
        int n = values_nsize(obj);
        scm_obj_t* elts = values_elts(obj);
        for (int i = 0; i < n; i++) {
          out << " ";
          print(visited, elts[i], false);
        }
        out << ">";
        return;
      }
      case tc6_u8vector: {
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
      case tc6_closure:
        out << std::format("#<closure {:#x} argc:{} rest:{} nenv:{}>", (uintptr_t)obj, closure_argc(obj), closure_rest(obj), closure_nenv(obj));
        return;
      case tc6_environment:
        out << std::format("#<environment {:#x} name:{}>", (uintptr_t)obj, (char*)environment_name(obj));
        return;
      case tc6_cell:
        out << std::format("#<cell {:#x}>", (uintptr_t)obj);
        return;
      case tc6_escape:
        out << std::format("#<escape {:#x}>", (uintptr_t)obj);
        return;
      case tc6_continuation:
        out << std::format("#<continuation {:#x}>", (uintptr_t)obj);
        return;
      case tc6_hashtable:
        out << std::format("#<hashtable {:#x}>", (uintptr_t)obj);
        return;
      case tc6_port:
        out << std::format("#<port {:#x}>", (uintptr_t)obj);
        return;
      case tc6_tuple:
        if (tuple_nsize(obj) == 0) {
          out << "#<tuple>";
          return;
        }
        if (is_symbol(tuple_elts(obj)[0])) {
          out << "#<" << (const char*)symbol_name(tuple_elts(obj)[0]);
          int n = tuple_nsize(obj);
          scm_obj_t* elts = tuple_elts(obj);
          for (int i = 1; i < n; i++) {
            out << " ";
            print(visited, elts[i], false);
          }
          out << ">";

          return;
        }
        out << std::format("#<tuple {:#x} size:{}>", (uintptr_t)obj, tuple_nsize(obj));
        return;
    }
  }

  if (is_cons(obj)) {
    if (obj == (scm_obj_t) nullptr) {
      out << "#<null>";
      return;
    }
    out << "(";
    print(visited, cons_car(obj), display_mode);
    scm_obj_t cdr = cons_cdr(obj);
    while (is_cons(cdr)) {
      if (cdr == (scm_obj_t) nullptr) {
        out << "#<null>";
        return;
      }
      // In shared-structure mode, if this cdr cell is shared/circular,
      // stop the implicit list traversal and emit it as a dot-pair tail.
      // The top-level shared check in print() will handle the #N= / #N# labels.
      if (visited != nullptr) {
        auto it = visited->find(cdr);
        if (it != visited->end()) {
          // This cdr cell is shared or circular — print as `. cdr`
          out << " . ";
          print(visited, cdr, display_mode);
          out << ")";
          return;
        }
      }
      out << " ";
      print(visited, cons_car(cdr), display_mode);
      cdr = cons_cdr(cdr);
    }
    if (cdr != scm_nil) {
      out << " . ";
      print(visited, cdr, display_mode);
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

void printer_t::printf(const char* fmt, ...) {
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

void printer_t::scan(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj) {
  assert(visited != nullptr);
  // Only cons, heap objects (vectors, strings, symbols) can be shared
  if (!is_cons(obj) && !is_heap_object(obj)) return;

  auto [it, inserted] = visited->try_emplace(obj, scm_false);
  if (!inserted) {
    if (it->second == scm_true) return;  // already confirmed shared, stop recursion
    it->second = scm_true;               // second visit: mark as shared
    return;
  }
  // First visit — recurse into children if needed
  if (is_cons(obj)) {
    scan(visited, cons_car(obj));
    scan(visited, cons_cdr(obj));
    return;
  }
  if (is_heap_object(obj)) {
    switch (heap_tc6_num(obj)) {
      case tc6_vector: {
        int n = vector_nsize(obj);
        scm_obj_t* elts = vector_elts(obj);
        for (int i = 0; i < n; i++) scan(visited, elts[i]);
        break;
      }
      case tc6_values: {
        int n = values_nsize(obj);
        scm_obj_t* elts = values_elts(obj);
        for (int i = 0; i < n; i++) scan(visited, elts[i]);
        break;
      }
        // tc6_string, tc6_symbol: no children, already inserted above
    }
  }
}

void printer_t::write_ss(scm_obj_t obj) {
  std::unordered_map<scm_obj_t, scm_obj_t> visited;
  m_shared_tag = 0;
  scan(&visited, obj);
  // Remove entries that were only seen once — they don't need labels
  for (auto it = visited.begin(); it != visited.end();) {
    if (it->second == scm_false) {
      it = visited.erase(it);
    } else {
      ++it;
    }
  }
  print(&visited, obj, false);
}
