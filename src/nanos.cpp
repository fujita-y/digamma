#include "object.h"
#include "nanos.h"
#include "codegen.h"
#include "nanos_jit.h"
#include "nanos_options.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

#include <fstream>
#include <iostream>
#include <llvm/Support/TargetSelect.h>
#include <replxx.hxx>
#include <sstream>
#include <string>

#define IR_MODE    0
#define IR_VERBOSE 0

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
  heap->init((size_t)DEFAULT_HEAP_LIMIT * 1024 * 1024, 64 * 1024 * 1024);
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

void nanos_t::load_script(const char* filename) {
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
    try {
      evaluate(obj, printer);
    } catch (std::exception& e) {
      printf("%s\n", e.what());
      exit(1);
    }
  }
}

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
#if IR_VERBOSE
      printf("codegen: ");
      printer.write(CAR(CDR(obj)));
      puts("");
#endif
      try {
        auto func = codegen_t::current()->compile(obj);
        intptr_t result = func();
#if IR_VERBOSE
        printf("(0x%016lx)\n", result);
        printer.write((scm_obj_t)result);
        puts("");
#endif
      } catch (std::exception& e) {
        printf("%s\n", e.what());
        exit(1);
      }
    }
  }
}

scm_obj_t nanos_t::core_eval(scm_obj_t obj) {
  object_heap_t* heap = object_heap_t::current();
  scm_obj_t core_eval = heap->environment_variable_ref(make_symbol("core-eval"));
  if (core_eval == scm_undef) {
    throw std::runtime_error("core-eval not found in current environment");
  }
  if (!is_closure(core_eval)) {
    throw std::runtime_error("core-eval is not a closure");
  }

  codegen_t* cg = codegen_t::current();
  auto bridge = cg->call_closure_bridge();

  scm_obj_t args[2] = {obj, scm_nil};
  return (scm_obj_t)bridge(core_eval, 2, args);
}

void nanos_t::run() {
  puts(";; nanos - a small virtual machine for bootstrapping, compile nanos-ir to native code.");
#if USE_TBI
  puts(";; USE_TBI == 1");
#else
  puts(";; USE_TBI == 0");
#endif
  std::cout << ";; boot_file: " << nanos_options::boot_file << std::endl;
  std::cout << ";; script_file: " << nanos_options::script_file << std::endl;

  printer_t printer(std::cout);
  auto rx = std::make_unique<replxx::Replxx>();
  rx->install_window_change_handler();

  std::string input_buffer;
  while (repl(*rx, input_buffer, printer));
}

static void add_history(replxx::Replxx& rx, const std::string& line) {
  std::string hist = line;
  while (!hist.empty() && (hist.back() == '\n' || hist.back() == '\r')) hist.pop_back();
  if (!hist.empty()) rx.history_add(hist);
}

bool nanos_t::repl(replxx::Replxx& rx, std::string& input_buffer, printer_t& printer) {
#if IR_MODE
  char const* cinput = rx.input(input_buffer.empty() ? "nanos-ir> " : "        ");
#else
  char const* cinput = rx.input(input_buffer.empty() ? "> " : "  ");
#endif

  if (cinput == nullptr) return false;

  std::string line(cinput);
  if (input_buffer.empty() && line.empty()) return true;

  if (line.starts_with("!")) {
    load_ir(line.substr(1).c_str());
    add_history(rx, line);
    return true;
  }
  if (line.starts_with("@")) {
    load_script(line.substr(1).c_str());
    add_history(rx, line);
    return true;
  }

  input_buffer += line + "\n";

  std::istringstream iss(input_buffer);
  reader_t reader(iss);
  std::vector<scm_obj_t> objs;
  bool incomplete = false;

  while (true) {
    bool err = false;
    scm_obj_t obj = reader.read(err);

    if (err) {
      if (reader.get_error_message().find("unexpected end-of-file") != std::string::npos) {
        incomplete = true;
      } else {
        std::cout << "Error: " << reader.get_error_message() << std::endl;
        input_buffer.clear();
      }
      break;
    }

    if (obj == scm_eof) break;
    objs.push_back(obj);
  }

  if (incomplete) return true;

  add_history(rx, input_buffer);
  input_buffer.clear();

  for (scm_obj_t obj : objs) {
    evaluate(obj, printer);
  }

  return true;
}

void nanos_t::evaluate(scm_obj_t obj, printer_t& printer) {
  try {
#if IR_MODE
    codegen_t* cg = codegen_t::current();
    auto func = cg->compile(obj);
    intptr_t result = func();
    printf("(0x%016lx)\n", result);
    printer.write((scm_obj_t)result);
    puts("\n");
#else
    scm_obj_t result = core_eval(obj);
    printer.write(result);
    puts("\n");
#endif
  } catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
  }
}
