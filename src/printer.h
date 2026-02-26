// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef PRINTER_H_INCLUDED
#define PRINTER_H_INCLUDED

#include "core.h"
#include "object.h"

class printer_t {
  std::ostream& out;
  void print(scm_obj_t obj, bool display_mode);

 public:
  printer_t(std::ostream& os) : out(os) {}
  void write(scm_obj_t obj);
  void display(scm_obj_t obj);
  void format(const char* fmt, ...);
};

#endif
