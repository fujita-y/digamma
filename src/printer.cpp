// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "printer.h"

#include <format>
#include <iomanip>
#include <unordered_map>

void printer_t::write(scm_obj_t obj) { print(nullptr, obj, false); }

void printer_t::display(scm_obj_t obj) { print(nullptr, obj, true); }

void printer_t::print(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj, bool display_mode) {
  if (is_fixnum(obj) || is_singleton(obj)) {
    print_immediate(obj);
    return;
  }
  if (is_char(obj)) {
    print_char(obj, display_mode);
    return;
  }
  if (is_flonum(obj)) {
    print_flonum(flonum(obj));
    return;
  }

  // SRFI-38: handle shared/circular structure labels for heap and cons objects
  if (visited != nullptr && (is_heap_object(obj) || is_cons(obj))) {
    auto it = visited->find(obj);
    if (it != visited->end()) {
      if (is_fixnum(it->second)) {
        out << "#" << fixnum(it->second) << "#";
        return;
      } else {
        int tag = m_shared_tag++;
        it->second = make_fixnum(tag);
        out << "#" << tag << "=";
      }
    }
  }

  if (is_heap_object(obj)) {
    switch (heap_tc6_num(obj)) {
      case tc6_symbol:
        print_symbol(obj, display_mode);
        return;
      case tc6_string:
        print_string(obj, display_mode);
        return;
      case tc6_long_flonum:
        print_flonum(flonum(obj));
        return;
      case tc6_vector:
        print_vector(visited, obj);
        return;
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
      case tc6_u8vector:
        print_bytevector(obj);
        return;
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
      case tc6_future:
        out << std::format("#<future {:#x}>", (uintptr_t)obj);
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
    print_list(visited, obj, display_mode);
    return;
  }

  out << "#<unknown 0x" << std::hex << obj << std::dec << ">";
}

void printer_t::print_flonum(double d) {
  std::ostringstream buf;
  buf << std::setprecision(17) << d;
  std::string s = buf.str();
  if (s.find('.') == std::string::npos && s.find('e') == std::string::npos) {
    s += ".0";
  }
  out << s;
}

void printer_t::print_symbol(scm_obj_t obj, bool display_mode) {
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
}

void printer_t::print_string(scm_obj_t obj, bool display_mode) {
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
}

void printer_t::print_vector(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj) {
  out << "#(";
  int n = vector_nsize(obj);
  scm_obj_t* elts = vector_elts(obj);
  for (int i = 0; i < n; i++) {
    if (i > 0) out << " ";
    print(visited, elts[i], false);
  }
  out << ")";
}

void printer_t::print_bytevector(scm_obj_t obj) {
  out << "#u8(";
  int n = u8vector_nsize(obj);
  uint8_t* elts = u8vector_elts(obj);
  for (int i = 0; i < n; i++) {
    if (i > 0) out << " ";
    out << (int)elts[i];
  }
  out << ")";
}

void printer_t::print_list(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj, bool display_mode) {
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
    if (visited != nullptr) {
      auto it = visited->find(cdr);
      if (it != visited->end()) {
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
}

void printer_t::print_immediate(scm_obj_t obj) {
  if (is_fixnum(obj)) {
    out << fixnum(obj);
    return;
  }
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
  out << "#<unknown 0x" << std::hex << obj << std::dec << ">";
}

void printer_t::print_char(scm_obj_t obj, bool display_mode) {
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

void printer_t::newline() { out << std::endl; }

void printer_t::format(int argc, scm_obj_t argv[]) {
  if (argc < 1) fatal("%s:%u too few arguments", __FILE__, __LINE__);
  const char* fmt = (const char*)string_name(argv[0]);
  int arg_idx = 1;

  auto next_arg = [&]() -> scm_obj_t {
    if (arg_idx >= argc) throw std::runtime_error("format: too few arguments: " + to_string(argv[0]));
    return argv[arg_idx++];
  };

  for (const char* p = fmt; *p; p++) {
    if (*p != '~') {
      out << *p;
      continue;
    }
    p++;
    if (*p == '\0') {
      out << '~';
      break;
    }
    char c = *p;
    switch (c) {
      case 'a':
        display(next_arg());
        break;
      case 's':
        write(next_arg());
        break;
      case 'w':
        write_ss(next_arg());
        break;
      case '%':
        out << '\n';
        break;
      case '!':
        out.flush();
        break;
      case '~':
        out << '~';
        break;
      default:
        if (isdigit(static_cast<unsigned char>(c))) {
          int width = 0;
          while (isdigit(static_cast<unsigned char>(*p))) {
            width = width * 10 + (*p++ - '0');
          }
          int fraction = 0;
          bool has_fraction = false;
          if (*p == ',') {
            p++;
            has_fraction = true;
            while (isdigit(static_cast<unsigned char>(*p))) {
              fraction = fraction * 10 + (*p++ - '0');
            }
          }
          if (*p == 'f') {
            scm_obj_t arg = next_arg();
            double val;
            if (is_fixnum(arg))
              val = static_cast<double>(fixnum(arg));
            else if (is_flonum(arg))
              val = flonum(arg);
            else
              throw std::runtime_error("format: argument is not a number: " + to_string(arg));

            out << std::fixed << std::showpoint << std::setw(width) << std::setprecision(fraction) << val;
            break;
          }
          out << '~';
          // Fall through (or handle error)
        }
        out << '~' << c;
        break;
    }
  }
}
