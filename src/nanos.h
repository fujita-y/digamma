// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_H_INCLUDED
#define NANOS_H_INCLUDED

#include "core.h"

class object_heap_t;
class codegen_t;

class nanos_t {
  object_heap_t* heap;
  codegen_t* codegen;

 public:
  void init();
  void destroy();
  void run();
};

#endif  // NANOS_H_INCLUDED
