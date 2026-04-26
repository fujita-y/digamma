#include "core.h"
#include "exception.h"
#include "nanos.h"
#include "nanos_options.h"
#include "uniq_id.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static int nanos_thread() {
  nanos_t* nanos = new nanos_t();
  nanos->init();
  int status = 0;
  try {
    nanos->run();
  } catch (const nanos_exit_t& e) {
    status = e.get_status();
  } catch (const std::exception& e) {
    std::cerr << "Unhandled std::exception: " << e.what() << std::endl;
    status = 1;
  } catch (...) {
    std::cerr << "Unhandled unknown exception" << std::endl;
    status = 1;
  }
  nanos->destroy();
  delete nanos;
  return status;
}

int main(int argc, char** argv) {
  nanos_options::parse(argc, argv);
  std::future<int> root = std::async(&nanos_thread);
  root.get();
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
