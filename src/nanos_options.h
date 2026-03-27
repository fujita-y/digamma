// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_OPTIONS_H_INCLUDED
#define NANOS_OPTIONS_H_INCLUDED

#include "core.h"
#include <CLI/CLI.hpp>

class nanos_options {
 public:
  static void parse(int argc, char** argv);
  static std::string boot_file;
  static std::string env_name;
  static std::string script_file;
};

#endif
