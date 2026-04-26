// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_H_INCLUDED
#define NANOS_H_INCLUDED

#include "core.h"
#include "object.h"
#include "context.h"
#include "nanos_jit.h"

#include <memory>
#include <string>

#define SUBR extern "C" scm_obj_t

namespace replxx {
class Replxx;
}
class printer_t;

class nanos_t {
 public:
  static nanos_t* current() { return context::s_current_nanos; }

  void init();
  void destroy();
  void run();
  void evaluate(scm_obj_t obj, printer_t& printer);
  scm_obj_t lookup_system_environment(scm_obj_t symbol);
  scm_obj_t call_core_eval(scm_obj_t obj);
  scm_obj_t call_add_load_path(scm_obj_t path);

  std::unique_ptr<nanos_jit_t> m_jit;

  std::string m_boot_file;
  std::string m_env_name;
  std::string m_script_file;
  std::vector<std::string> m_load_paths;

 private:
  void init_subr();
  void init_codegen();
  void load_ir(std::string filename);
  void load_script(std::string filename);
  bool repl(replxx::Replxx& rx, std::string& input_buffer, printer_t& printer);
};

#endif  // NANOS_H_INCLUDED
