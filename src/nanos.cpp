#include "object.h"
#include "nanos.h"
#include <llvm/Support/TargetSelect.h>
#include "codegen.h"
#include "nanos_jit.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

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
  object_heap_t* heap = object_heap_t::current();
  heap->destroy();
  delete heap;
  m_jit.reset();
}

// ============================================================================
// Execution & REPL
// ============================================================================

void nanos_t::run() {
  puts(";; nanos - a small virtual machine for bootstrapping, compile nanos-ir to native code.");
#if USE_TBI
  puts(";; USE_TBI == 1");
#else
  puts(";; USE_TBI == 0");
#endif

  printer_t printer(std::cout);
  reader_t reader(std::cin);

  while (true) {
    if (std::cin.eof()) break;

    printf("> ");
    fflush(stdout);

    bool err = false;
    scm_obj_t obj = reader.read(err);

    if (err) {
      if (obj == scm_eof) break;  // Should have been caught by checking eof before read, but just in case
      puts("Error: read failed");
      // Simple recovery: consume rest of line
      std::string line;
      std::getline(std::cin, line);
      continue;
    }
    if (obj == scm_eof) break;

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
