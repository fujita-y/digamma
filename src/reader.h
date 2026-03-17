// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef READER_H_INCLUDED
#define READER_H_INCLUDED

#include "core.h"
#include "object.h"

class reader_t {
  std::istream& in;
  std::vector<int> buffer;

  int transform_char(int c);
  int get_char();
  void unget_char(int c);
  int peek_char();
  void skip_whitespace();
  void skip_line();
  void skip_block_comment();
  scm_obj_t read_list(bool& err, int close_char);
  scm_obj_t read_vector(bool& err, int close_char);
  scm_obj_t read_u8vector(bool& err, int close_char);
  scm_obj_t read_string(bool& err);
  scm_obj_t read_symbol(int c);
  scm_obj_t read_number(int c, bool& err);
  scm_obj_t read_char(bool& err);
  scm_obj_t read_quote(bool& err);
  scm_obj_t read_quasiquote(bool& err);
  scm_obj_t read_unquote(bool& err);
  scm_obj_t read_unquote_splicing(bool& err);

  std::string error_message;

 public:
  reader_t(std::istream& is);
  scm_obj_t read(bool& err);
  std::string get_error_message() const { return error_message; }
};

#endif
