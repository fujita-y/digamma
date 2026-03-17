// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "reader.h"
#include "utf8.h"

reader_t::reader_t(std::istream& is) : in(is) {}

int reader_t::get_char() {
  if (!buffer.empty()) {
    int c = buffer.back();
    buffer.pop_back();
    return c;
  }
  return in.get();
}

void reader_t::unget_char(int c) { buffer.push_back(c); }

int reader_t::peek_char() {
  if (!buffer.empty()) {
    return buffer.back();
  }
  int c = in.get();
  buffer.push_back(c);
  return c;
}

void reader_t::skip_whitespace() {
  int c;
  while ((c = peek_char()) != EOF) {
    if (isspace(c)) {
      get_char();
    } else if (c == ';') {
      skip_line();
    } else if (c == '#') {
      get_char();
      int next = peek_char();
      if (next == '|') {
        get_char();
        skip_block_comment();
      } else {
        unget_char('#');
        break;
      }
    } else {
      break;
    }
  }
}

void reader_t::skip_block_comment() {
  int c;
  while ((c = get_char()) != EOF) {
    if (c == '|') {
      if (peek_char() == '#') {
        get_char();
        return;
      }
    } else if (c == '#') {
      if (peek_char() == '|') {
        get_char();
        skip_block_comment();
      }
    }
  }
}

void reader_t::skip_line() {
  int c;
  while ((c = get_char()) != EOF) {
    if (c == '\n') break;
  }
}

scm_obj_t reader_t::read(bool& err) {
  error_message.clear();
  skip_whitespace();
  int c = get_char();
  if (c == EOF) return scm_eof;

  switch (c) {
    case '(':
      return read_list(err, ')');
    case '[':
      return read_list(err, ']');
    case ')':
    case ']':
      err = true;
      error_message = "unexpected closing bracket";
      return scm_undef;
    case '"':
      return read_string(err);
    case '\'':
      return read_quote(err);
    case '`':
      return read_quasiquote(err);
    case ',': {
      if (peek_char() == '@') {
        get_char();
        return read_unquote_splicing(err);
      }
      return read_unquote(err);
    }
    case '#': {
      int next = peek_char();
      if (next == '(') {
        get_char();
        return read_vector(err, ')');
      }
      if (next == '[') {
        get_char();
        return read_vector(err, ']');
      }
      if (next == ';') {
        get_char();
        bool ignore_err = false;
        read(ignore_err);
        if (ignore_err) {
          err = true;
          error_message = "unexpected end-of-file while reading comments";
          return scm_undef;
        }
        return read(err);
      }
      if (next == 'u') {
        get_char();
        if (peek_char() == '8') {
          get_char();
          if (peek_char() == '(') {
            get_char();
            return read_u8vector(err, ')');
          }
          if (peek_char() == '[') {
            get_char();
            return read_u8vector(err, ']');
          }
          // handle error or other #u syntax?
        }
        // fallback or error
      }
      if (next == 't') {
        get_char();
        return scm_true;
      }
      if (next == 'f') {
        get_char();
        return scm_false;
      }
      if (next == '\\') {
        get_char();
        return read_char(err);
      }
      // Radix and exactness prefixes
      if (next == 'x' || next == 'b' || next == 'o' || next == 'd' || next == 'e' || next == 'i') {
        return read_number(c, err);
      }
      break;
    }
    default:
      // check for number or symbol
      if (isdigit(c)) {
        return read_number(c, err);
      }
      if (c == '+' || c == '-') {
        // Basic heuristic, if it starts with digit, +, - it might be a number.
        // But + and - can be symbols.
        // If + or - is followed by space or delimiter, it's a symbol.
        // If followed by digit, it's a number.
        int next = peek_char();
        if (isdigit(next) || next == '.') {
          return read_number(c, err);
        }
        // Special case for +inf.0, -nan.0 etc?
        // Simplification: treat as symbol and let symbol parser handle weirdness or return symbol
        // read_symbol will handle + and -
      }
      if (c == '.') {  // dot in list or symbol?
                       // if followed by delimiter, it is dot.
        int next = peek_char();
        if (isspace(next) || next == EOF || next == ')') {
          return read_symbol(c);  // treat '.' as symbol, let caller handle if it was meant to be improper list separator
        }
        if (isdigit(next)) {
          return read_number(c, err);
        }
      }
      return read_symbol(c);
  }

  // Placeholder
  return scm_undef;
}

scm_obj_t reader_t::read_list(bool& err, int close_char) {
  scm_obj_t head = scm_nil;
  scm_obj_t tail = scm_nil;

  while (true) {
    skip_whitespace();
    int c = peek_char();
    if (c == EOF) {
      err = true;
      error_message = "unexpected end-of-file while reading list";
      return scm_eof;
    }
    if (c == ')' || c == ']') {
      if (c == close_char) {
        get_char();
        return head;  // proper list
      } else {
        err = true;
        error_message = "mismatched parentheses";
        return scm_undef;
      }
    }
    if (c == '.' && head != scm_nil) {
      // Check if it is really a dot or part of symbol
      get_char();
      int next = peek_char();
      // If delimiter, it's a dot
      if (isspace(next) || next == ')' || next == ']' || next == EOF) {
        scm_obj_t last = read(err);
        if (err) return scm_undef;

        skip_whitespace();
        if (peek_char() != close_char) {
          err = true;  // dot must be followed by one element and then close paren
          error_message = "more than one item following dot('.') while reading list or mismatched parentheses";
          return scm_undef;
        }
        get_char();  // consume closing bracket
        scm_cons_rec_t* cons = (scm_cons_rec_t*)tail;
        cons->cdr = last;
        return head;
      } else {
        unget_char('.');
        // Fall through to normal read
      }
    }

    scm_obj_t obj = read(err);
    if (err) return scm_undef;

    scm_obj_t new_cell = make_cons(obj, scm_nil);
    if (head == scm_nil) {
      head = new_cell;
      tail = new_cell;
    } else {
      scm_cons_rec_t* cons = (scm_cons_rec_t*)tail;
      cons->cdr = new_cell;
      tail = new_cell;
    }
  }
}

scm_obj_t reader_t::read_vector(bool& err, int close_char) {
  std::vector<scm_obj_t> elts;
  while (true) {
    skip_whitespace();
    int c = peek_char();
    if (c == EOF) {
      err = true;
      error_message = "unexpected end-of-file while reading vector";
      return scm_eof;
    }
    if (c == ')' || c == ']') {
      if (c == close_char) {
        get_char();
        break;
      } else {
        err = true;
        error_message = "mismatched parentheses";
        return scm_undef;
      }
    }
    elts.push_back(read(err));
    if (err) return scm_undef;
  }
  scm_obj_t vec = make_vector(elts.size(), scm_nil);
  scm_obj_t* data = vector_elts(vec);
  for (size_t i = 0; i < elts.size(); ++i) data[i] = elts[i];
  return vec;
}

scm_obj_t reader_t::read_u8vector(bool& err, int close_char) {
  std::vector<uint8_t> elts;
  while (true) {
    skip_whitespace();
    int c = peek_char();
    if (c == EOF) {
      err = true;
      error_message = "unexpected end-of-file while reading u8vector";
      return scm_eof;
    }
    if (c == ')' || c == ']') {
      if (c == close_char) {
        get_char();
        break;
      } else {
        err = true;
        error_message = "mismatched parentheses";
        return scm_undef;
      }
    }
    scm_obj_t obj = read(err);
    if (err) return scm_undef;
    if (!is_fixnum(obj)) {
      err = true;
      error_message = "expected fixnum in u8vector";
      return scm_undef;
    }
    intptr_t val = fixnum(obj);
    if (val < 0 || val > 255) {
      err = true;
      error_message = "u8vector element out of range";
      return scm_undef;
    }
    elts.push_back((uint8_t)val);
  }
  scm_obj_t vec = make_u8vector(elts.size());
  uint8_t* data = u8vector_elts(vec);
  for (size_t i = 0; i < elts.size(); ++i) data[i] = elts[i];
  return vec;
}
scm_obj_t reader_t::read_string(bool& err) {
  std::string buf;
  while (true) {
    int c = get_char();
    if (c == EOF) {
      err = true;  // Error: unexpected EOF in string
      error_message = "unexpected end-of-file while reading string";
      return scm_eof;
    }
    if (c == '"') {
      break;
    }
    if (c == '\\') {
      c = get_char();
      switch (c) {
        case 'a':
          c = '\a';
          break;
        case 'b':
          c = '\b';
          break;
        case 't':
          c = '\t';
          break;
        case 'n':
          c = '\n';
          break;
        case 'v':
          c = '\v';
          break;
        case 'f':
          c = '\f';
          break;
        case 'r':
          c = '\r';
          break;
        case '"':
          c = '"';
          break;
        case '\\':
          c = '\\';
          break;
        case 'x': {
          // hex escape \xHH...;
          int hex = 0;
          while (true) {
            int h = peek_char();
            if (isxdigit(h)) {
              get_char();
              hex = hex * 16 + (isdigit(h) ? h - '0' : tolower(h) - 'a' + 10);
            } else if (h == ';') {
              get_char();
              break;
            } else {
              // error in read: inline hex escape missing terminating semi-colon
              err = true;
              error_message = "inline hex escape missing terminating semi-colon";
              return scm_eof;
            }
          }
          if (hex < 128) {
            c = hex;
          } else {
            uint8_t utf8[4];
            int n = cnvt_ucs4_to_utf8(hex, utf8);
            for (int i = 0; i < n; i++) buf.push_back((char)utf8[i]);
            continue;
          }
          break;
        }
        default:
          break;
      }
    }
    buf.push_back((char)c);
  }
  return make_string(buf.c_str());
}

static bool is_symbol_char(int c) {
  return isalnum(c) || c == '!' || c == '$' || c == '%' || c == '&' || c == '*' || c == '+' || c == '-' || c == '.' || c == '/' || c == ':' ||
         c == '<' || c == '=' || c == '>' || c == '?' || c == '@' || c == '^' || c == '_' || c == '~';
}

scm_obj_t reader_t::read_symbol(int c) {
  std::string buf;
  if (c == '|') {
    // pipe quoted symbol
    while (true) {
      int n = get_char();
      if (n == EOF) break;  // error
      if (n == '|') break;
      if (n == '\\') {
        n = get_char();
      }
      buf.push_back((char)n);
    }
    return make_symbol(buf.c_str());
  }

  buf.push_back((char)c);
  while (true) {
    int next = peek_char();
    if (is_symbol_char(next)) {
      buf.push_back((char)get_char());
    } else {
      break;
    }
  }
  return make_symbol(buf.c_str());
}

scm_obj_t reader_t::read_number(int c, bool& err) {
  std::string buf;
  buf.push_back((char)c);
  while (true) {
    int next = peek_char();
    // Allow typical number chars.
    // This is a relaxed parser; exact validation happens in conversion or specific number parser.
    // + - . / @ e E i #
    if (isalnum(next) || next == '+' || next == '-' || next == '.' || next == '/' || next == '@' || next == '#') {
      buf.push_back((char)get_char());
    } else {
      break;
    }
  }

  int radix = 10;
  size_t idx = 0;
  bool has_radix = false;
  bool inexact = false;

  // Check for prefixes
  while (idx < buf.length() && buf[idx] == '#') {
    if (idx + 1 >= buf.length()) {
      err = true;
      error_message = "invalid lexical syntax";
      return scm_undef;
    }
    char p = tolower(buf[idx + 1]);
    idx += 2;
    if (p == 'x') {
      if (has_radix) {
        err = true;
        error_message = "duplicate radix prefix";
        return scm_undef;
      }
      radix = 16;
      has_radix = true;
    } else if (p == 'b') {
      if (has_radix) {
        err = true;
        error_message = "duplicate radix prefix";
        return scm_undef;
      }
      radix = 2;
      has_radix = true;
    } else if (p == 'o') {
      if (has_radix) {
        err = true;
        error_message = "duplicate radix prefix";
        return scm_undef;
      }
      radix = 8;
      has_radix = true;
    } else if (p == 'd') {
      if (has_radix) {
        err = true;
        error_message = "duplicate radix prefix";
        return scm_undef;
      }
      radix = 10;
      has_radix = true;
    } else if (p == 'e') {
      err = true;
      error_message = "exactness prefix #e not supported";
      return scm_undef;
    } else if (p == 'i') {
      inexact = true;
    } else {
      err = true;
      error_message = "invalid lexical syntax";
      return scm_undef;
    }
  }

  std::string num_str = buf.substr(idx);
  if (num_str.empty()) {
    err = true;
    error_message = "invalid lexical syntax";
    return scm_undef;
  }

  // Try to parse as integer
  try {
    size_t processed = 0;
    // check for float indicators only if radix is 10 (and no explicit radix prefix? R7RS says #d1.5 is valid?)
    // If explicit radix is given, usually expects integer syntax for stoll unless we parse hex floats.
    // For now, assume fixnums for non-decimal radixes as per existing scm limitation/simplicity
    // Or if decimal/default, check for float chars.

    // Simplification: if radix is 10, try double parser if it looks like float.
    // But stoll with base 10 parses integers.
    // If radix != 10, use stoll with base.

    if (radix == 10 &&
        (num_str.find('.') != std::string::npos || num_str.find('e') != std::string::npos || num_str.find('E') != std::string::npos)) {
      // float
      double d = std::stod(num_str, &processed);
      if (processed == num_str.length()) return make_flonum(d);
    } else {
      // parse as int
      long long l = std::stoll(num_str, &processed, radix);
      if (processed == num_str.length()) {
        if (inexact) return make_flonum((double)l);
        return make_fixnum(l);
      }
    }
  } catch (...) {
    err = true;
    error_message = "invalid lexical syntax";
    return scm_undef;
  }

  err = true;
  error_message = "invalid lexical syntax";
  return make_fixnum(0);
}

scm_obj_t reader_t::read_char(bool& err) {
  int c = get_char();
  if (c == EOF) {
    err = true;
    error_message = "unexpected end-of-file while reading character";
    return scm_eof;
  }
  if (!isalnum(c)) {
    return make_char(c);
  }

  // Could be named char?
  std::string buf;
  buf.push_back((char)c);
  while (isalnum(peek_char())) {
    buf.push_back((char)get_char());
  }

  if (buf.length() == 1) return make_char(c);
  if (buf == "space") return make_char(' ');
  if (buf == "newline") return make_char('\n');
  if (buf == "return") return make_char('\r');
  if (buf == "tab") return make_char('\t');
  if (buf == "alarm") return make_char('\a');
  if (buf == "backspace") return make_char('\b');
  if (buf == "escape") return make_char(0x1b);
  if (buf == "delete") return make_char(0x7f);
  if (buf == "null") return make_char(0);

  // Hex unicode #\xHHHH
  if (buf[0] == 'x' && buf.length() > 1) {
    // primitive hex check
    try {
      unsigned long ucs4 = std::stoul(buf.substr(1), nullptr, 16);
      return make_char(ucs4);
    } catch (...) {
    }
  }

  // Fallback, maybe it was just a sequence of letters "a" ?
  // No, #\a is just 'a'. #\abc is invalid unless "abc" is named.
  // if unknown named, maybe just return the first char? or error?
  // R7RS says it's an error if not recognized.
  err = true;
  error_message = "invalid lexical syntax";
  return scm_undef;
}

scm_obj_t reader_t::read_quote(bool& err) {
  scm_obj_t obj = read(err);
  if (err) return scm_undef;
  return make_list2(make_symbol("quote"), obj);
}

scm_obj_t reader_t::read_quasiquote(bool& err) {
  scm_obj_t obj = read(err);
  if (err) return scm_undef;
  return make_list2(make_symbol("quasiquote"), obj);
}

scm_obj_t reader_t::read_unquote(bool& err) {
  scm_obj_t obj = read(err);
  if (err) return scm_undef;
  return make_list2(make_symbol("unquote"), obj);
}

scm_obj_t reader_t::read_unquote_splicing(bool& err) {
  scm_obj_t obj = read(err);
  if (err) return scm_undef;
  return make_list2(make_symbol("unquote-splicing"), obj);
}
