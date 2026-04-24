#include "core.h"
#include "exception.h"
#include "fiber.h"
#include "nanos.h"
#include "nanos_options.h"

#include "asio.h"  // include after nanos.h due to CR1 macro conflict

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  context::s_asio_context = new asio_context();
  fiber_init_scheduler();
  nanos_options::parse(argc, argv);
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
  delete context::s_asio_context;
  return status;
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
