// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_H_INCLUDED
#define NANOS_H_INCLUDED

#include "core.h"
#include "object.h"
#include <memory>
#include <string>
#include "nanos_jit.h"

#define SUBR extern "C" scm_obj_t

namespace replxx {
class Replxx;
}
class printer_t;

class nanos_t {
  std::unique_ptr<nanos_jit_t> m_jit;

  void init_subr();
  void init_codegen();

  void load_ir(std::string filename);
  void load_script(std::string filename);
  scm_obj_t lookup_system_environment(scm_obj_t symbol);
  scm_obj_t call_core_eval(scm_obj_t obj);

  bool repl(replxx::Replxx& rx, std::string& input_buffer, printer_t& printer);
  void evaluate(scm_obj_t obj, printer_t& printer);

 public:
  void init();
  void destroy();
  void run();
};

#endif  // NANOS_H_INCLUDED
