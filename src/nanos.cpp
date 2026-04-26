#include "object.h"
#include "nanos.h"
#include "codegen.h"
#include "context.h"
#include "exception.h"
#include "fiber.h"
#include "hash.h"
#include "nanos_jit.h"
#include "nanos_options.h"
#include "object_heap.h"
#include "port.h"
#include "printer.h"
#include "reader.h"

#include "asio.h"  // include after nanos.h due to CR1 macro conflict

#include <fstream>
#include <iostream>
#include <llvm/Support/TargetSelect.h>
#include <ranges>
#include <replxx.hxx>
#include <sstream>
#include <string>
#include <mutex>

#define IR_MODE    0
#define IR_VERBOSE 0

// ============================================================================
// Initialization
// ============================================================================

void nanos_t::init_codegen() {
  static std::once_flag llvm_init_flag;
  std::call_once(llvm_init_flag, []() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });

  auto jit_expected = nanos_jit_t::Create();
  if (!jit_expected) {
    fatal("Failed to create Nanos JIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
  }
  m_jit = std::move(*jit_expected);

  auto ctx = std::make_unique<llvm::LLVMContext>();
  new codegen_t(std::move(ctx), m_jit.get());
}

void nanos_t::init() {
  m_boot_file = nanos_options::boot_file;
  m_env_name = nanos_options::env_name;
  m_script_file = nanos_options::script_file;
  m_load_paths = nanos_options::load_paths;

  object_heap_t* heap = new object_heap_t();
#ifndef NDEBUG
  // debug build
  heap->init((size_t)DEFAULT_HEAP_LIMIT * 1024 * 1024, 4 * 1024 * 1024);
#else
  // release build
  heap->init((size_t)DEFAULT_HEAP_LIMIT * 1024 * 1024, 4 * 1024 * 1024);
#endif

  context::init();
  context::s_asio_context = new asio_context();
  init_fiber_scheduler();
  init_codegen();
  init_subr();
  context::s_current_nanos = this;
}

void nanos_t::destroy() {
  delete context::s_asio_context;
  m_jit.reset();
  codegen_t* codegen = codegen_t::current();
  codegen->destroy();
  delete codegen;
  object_heap_t* heap = object_heap_t::current();
  context::destroy();
  heap->destroy();
  delete heap;
}

// ============================================================================
// Execution & REPL
// ============================================================================
/*
void nanos_t::load_script(std::string filename) {
  if (!filename.empty()) nanos_options::script_file = filename;
  std::ifstream ifs(nanos_options::script_file);
  if (!ifs) {
    puts("Error: failed to open file");
    return;
  }
  reader_t reader(ifs);
  printer_t printer(std::cout);
  std::vector<scm_obj_t> lst;
  bool err = false;
  while (true) {
    scm_obj_t obj = reader.read(err);
    if (err) {
      std::string msg = reader.get_error_message();
      std::cout << "read error: " << msg << std::endl;
      exit(1);
    }
    if (obj == scm_eof) break;
    lst.push_back(obj);
  }
  scm_obj_t expr = scm_nil;
  for (scm_obj_t obj : lst | std::views::reverse) {
    expr = make_cons(obj, expr);
  }
  expr = make_cons(make_symbol("begin"), expr);
  try {
    evaluate(expr, printer);
  } catch (const nanos_exit_t& e) {
    throw;
  } catch (const std::exception& e) {
    std::cerr << "Exception while executing script: " << e.what() << std::endl;
    exit(1);
  }
}
*/

void nanos_t::load_script(std::string filename) {
  if (!filename.empty()) m_script_file = filename;
  std::ifstream ifs(m_script_file);
  if (!ifs) {
    puts("Error: failed to open file");
    return;
  }
  reader_t reader(ifs);
  printer_t printer(std::cout);
  bool err = false;
  while (true) {
    scm_obj_t obj = reader.read(err);
    scoped_gc_protect protect(obj);
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
    } catch (const nanos_exit_t& e) {
      throw;
    } catch (const std::exception& e) {
      std::cerr << "Exception while loading script: " << e.what() << std::endl;
      exit(1);
    }
  }
}

void nanos_t::load_ir(std::string filename) {
  if (!filename.empty()) m_boot_file = filename;
  std::ifstream ifs(m_boot_file);
  if (!ifs) {
    puts("Error: failed to open file");
    return;
  }
  reader_t reader(ifs);
  printer_t printer(std::cout);
  bool err = false;
  while (true) {
    scm_obj_t obj = reader.read(err);
    scoped_gc_protect protect(obj);
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
      printer.write(cons_car(cons_cdr(obj)));
      puts("");
#endif
      try {
        auto func = codegen_t::current()->compile(obj);
        intptr_t result = func.release_and_run();
#if IR_VERBOSE
        printf("(0x%016lx)\n", result);
        printer.write((scm_obj_t)result);
        puts("");
#endif
      } catch (const nanos_exit_t& e) {
        throw;
      } catch (const std::exception& e) {
        std::cerr << "Exception while loading boot IR: " << e.what() << std::endl;
        exit(1);
      }
    }
  }
}

scm_obj_t nanos_t::lookup_system_environment(scm_obj_t symbol) {
  scm_obj_t variables = environment_variables(context::s_system_environment);
  scm_obj_t cell = hashtable_ref(variables, symbol, scm_undef);
  if (cell == scm_undef) {
    throw std::runtime_error("core-eval not found in system environment");
  }
  return cell_value(cell);
}

scm_obj_t nanos_t::call_core_eval(scm_obj_t obj) {
  scm_obj_t core_eval = lookup_system_environment(make_symbol("core-eval"));
  if (!is_closure(core_eval)) {
    throw std::runtime_error("core-eval is not a closure");
  }
  codegen_t* cg = codegen_t::current();
  auto bridge = cg->call_closure_bridge();
  scm_obj_t args[2] = {obj, context::s_current_environment};
  return (scm_obj_t)bridge(core_eval, 2, args);
}

scm_obj_t nanos_t::call_add_load_path(scm_obj_t path) {
  scm_obj_t add_load_path = lookup_system_environment(make_symbol("add-load-path"));
  if (!is_closure(add_load_path)) {
    throw std::runtime_error("add-load-path is not a closure");
  }
  codegen_t* cg = codegen_t::current();
  auto bridge = cg->call_closure_bridge();
  scm_obj_t args[1] = {path};
  return (scm_obj_t)bridge(add_load_path, 1, args);
}

void nanos_t::run() {
  puts(";; nanos - scheme-like lisp-2 interpreter for bootstrapping.");
#if USE_TBI
  puts(";; USE_TBI == 1");
#else
  puts(";; USE_TBI == 0");
#endif
  std::cout << ";; boot_file: " << m_boot_file << std::endl;
  std::cout << ";; script_file: " << m_script_file << std::endl;

  printer_t printer(std::cout);
  auto rx = std::make_unique<replxx::Replxx>();
  rx->install_window_change_handler();

  if (!m_boot_file.empty()) {
    load_ir(m_boot_file);
  }

  if (m_env_name == "interaction") {
    context::s_current_environment = context::s_interaction_environment;
  } else if (m_env_name == "system") {
    context::s_current_environment = context::s_system_environment;
  } else {
    throw std::runtime_error("Invalid environment name");
  }
  std::cout << ";; environment: " << std::string((char*)environment_name(context::s_current_environment)) << std::endl;

  for (const auto& path : m_load_paths | std::views::reverse) {
    call_add_load_path(make_string(path.c_str()));
  }

  if (!m_script_file.empty()) {
    load_script(m_script_file);
  }

  std::string input_buffer;
  while (repl(*rx, input_buffer, printer)) {
    while (fiber_live_count() > 0) {
      boost::this_fiber::sleep_for(std::chrono::microseconds(60));
    }
  }
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

  scoped_gc_protect_vector protect(objs);
  for (scm_obj_t obj : objs) evaluate(obj, printer);

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
    scm_obj_t result = call_core_eval(obj);
    if (auto out = port_get_ostream(context::s_standard_error_port)) out->flush();
    if (result != scm_unspecified) {
      printer.write(result);
      printer.newline();
      if (auto out = port_get_ostream(context::s_standard_output_port)) out->flush();
    }
#endif
  } catch (const nanos_exit_t& e) {
    throw;
  } catch (const std::exception& e) {
    std::cerr << "exception: " << e.what() << std::endl;
  }
}
