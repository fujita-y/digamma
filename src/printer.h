// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef PRINTER_H_INCLUDED
#define PRINTER_H_INCLUDED

#include "core.h"
#include "object.h"

#include <unordered_map>

class printer_t {
  std::ostream& out;
  int m_shared_tag;
  void scan(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj);
  void print(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj, bool display_mode);

 public:
  printer_t(std::ostream& os) : out(os), m_shared_tag(0) {}
  void write(scm_obj_t obj);
  void write_ss(scm_obj_t obj);
  void display(scm_obj_t obj);
  void format(int argc, scm_obj_t argv[]);

 private:
  void print_flonum(double d);
  void print_symbol(scm_obj_t obj, bool display_mode);
  void print_string(scm_obj_t obj, bool display_mode);
  void print_list(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj, bool display_mode);
  void print_vector(std::unordered_map<scm_obj_t, scm_obj_t>* visited, scm_obj_t obj);
  void print_bytevector(scm_obj_t obj);
  void print_immediate(scm_obj_t obj);
  void print_char(scm_obj_t obj, bool display_mode);
};

#endif
