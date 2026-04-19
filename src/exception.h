// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_EXCEPTION_H_INCLUDED
#define NANOS_EXCEPTION_H_INCLUDED

#include "core.h"

class nanos_exit_t : public std::exception {
 public:
  nanos_exit_t(int status) : status(status) {}
  ~nanos_exit_t() override = default;

  const char* what() const noexcept override { return "nanos_exit_t"; }
  int get_status() const noexcept { return status; }

 private:
  int status;
};

#endif
