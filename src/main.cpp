#include "core.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "nanos.h"

int main() {
  nanos_t* nanos = new nanos_t();
  nanos->init();
  nanos->run();
  nanos->destroy();
  delete nanos;
  return 0;
}

void fatal(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void warning(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
}

void trace(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}
