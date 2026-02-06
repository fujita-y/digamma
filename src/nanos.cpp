#include <llvm/Support/TargetSelect.h>
#include "codegen.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

int main() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  llvm::LLVMContext context;
  auto module_uptr = std::make_unique<llvm::Module>("nanos_module", context);
  llvm::Module* module = module_uptr.get();

  std::string err_str;
  llvm::ExecutionEngine* engine = llvm::EngineBuilder(std::move(module_uptr)).setErrorStr(&err_str).create();
  if (!engine) {
    fprintf(stderr, "Failed to create ExecutionEngine: %s\n", err_str.c_str());
    return 1;
  }

  object_heap_t heap;
  heap.init((size_t)DEFAULT_HEAP_LIMIT * 1024 * 1024, 4 * 1024 * 1024);
  puts(";; nanos - a small scheme interpreter for bootstrapping");

#if USE_TBI
  puts(";; USE_TBI == 1");
#else
  puts(";; USE_TBI == 0");
#endif

  codegen_t codegen(context, module, engine);

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
        intptr_t result = codegen.compile(obj);
        printf("%ld\n", result);
      } catch (std::exception& e) {
        printf("Error: %s\n", e.what());
      }
    } else {
      printer.format(obj);
      puts("");
    }
    heap.safepoint();
  }

  heap.destroy();
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
