#include "object.h"
#include "nanos.h"
#include "codegen.h"
#include "nanos_jit.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

#include <fstream>
#include <iostream>
#include <llvm/Support/TargetSelect.h>
#include <replxx.hxx>
#include <sstream>
#include <string>

// ============================================================================
// Initialization
// ============================================================================

void nanos_t::init_codegen() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  auto jit_expected = nanos_jit_t::Create();
  if (!jit_expected) {
    fatal("Failed to create Nanos JIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
  }
  m_jit = std::move(*jit_expected);

  auto ctx = std::make_unique<llvm::LLVMContext>();
  new codegen_t(std::move(ctx), m_jit.get());
}

void nanos_t::init() {
  object_heap_t* heap = new object_heap_t();
  heap->init((size_t)DEFAULT_HEAP_LIMIT * 1024 * 1024, 4 * 1024 * 1024);
  init_codegen();
  init_subr();
}

void nanos_t::destroy() {
  codegen_t* codegen = codegen_t::current();
  codegen->destroy();
  delete codegen;
  object_heap_t* heap = object_heap_t::current();
  heap->destroy();
  delete heap;
  m_jit.reset();
}

// ============================================================================
// Execution & REPL
// ============================================================================

void nanos_t::load_ir(const char* filename) {
  std::ifstream ifs(filename);
  if (!ifs) {
    puts("Error: failed to open file");
    return;
  }
  reader_t reader(ifs);
  printer_t printer(std::cout);
  bool err = false;
  while (true) {
    scm_obj_t obj = reader.read(err);
    if (err) {
      std::string msg = reader.get_error_message();
      std::cout << "read error: " << msg << std::endl;
      exit(1);
    }
    if (obj == scm_eof) {
      break;
    }
    if (is_cons(obj)) {
      printf("codegen: ");
      printer.write(CAR(CDR(obj)));
      puts("");
      try {
        auto func = codegen_t::current()->compile(obj);
        intptr_t result = func();
        printf("(0x%016lx)\n", result);
        printer.write((scm_obj_t)result);
        puts("");
      } catch (std::exception& e) {
        printf("%s\n", e.what());
      }
    }
  }
}

void nanos_t::run() {
  puts(";; nanos - a small virtual machine for bootstrapping, compile nanos-ir to native code.");
#if USE_TBI
  puts(";; USE_TBI == 1");
#else
  puts(";; USE_TBI == 0");
#endif

  printer_t printer(std::cout);
  auto rx = std::make_unique<replxx::Replxx>();
  rx->install_window_change_handler();

  std::string input_buffer;

  while (true) {
    char const* cinput = rx->input(input_buffer.empty() ? "> " : "  ");
    if (cinput == nullptr) {
      break;  // EOF
    }

    std::string line(cinput);
    if (input_buffer.empty() && line.empty()) continue;

    if (line.starts_with("!")) {
      std::string filename = line.substr(1);
      load_ir(filename.c_str());
      continue;
    }

    input_buffer += line + "\n";

    std::istringstream iss(input_buffer);
    reader_t reader(iss);

    bool err = false;
    std::vector<scm_obj_t> objs;
    bool incomplete = false;

    while (true) {
      err = false;
      scm_obj_t obj = reader.read(err);

      if (err) {
        std::string msg = reader.get_error_message();
        if (msg.find("unexpected end-of-file") != std::string::npos) {
          incomplete = true;
        } else {
          puts("Error: read failed");
          input_buffer.clear();
        }
        break;
      }

      if (obj == scm_eof) {
        break;
      }

      objs.push_back(obj);
    }

    if (incomplete) {
      continue;
    }

    if (!input_buffer.empty()) {
      std::string hist = input_buffer;
      while (!hist.empty() && (hist.back() == '\n' || hist.back() == '\r')) {
        hist.pop_back();
      }
      if (!hist.empty()) {
        rx->history_add(hist);
      }
      input_buffer.clear();
    }

    for (scm_obj_t obj : objs) {
      if (is_cons(obj)) {
        try {
          auto func = codegen_t::current()->compile(obj);
          intptr_t result = func();
          printf("(0x%016lx)\n", result);
          printer.write((scm_obj_t)result);
          puts("");
        } catch (std::exception& e) {
          printf("%s\n", e.what());
        }
      } else {
        printer.write(obj);
        puts("");
      }
    }
  }
}
