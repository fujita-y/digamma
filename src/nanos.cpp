#include "object.h"
#include <llvm/Support/TargetSelect.h>
#include "codegen.h"
#include "codegen_aux.h"
#include "nanos_subr.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

static void setup_subr() {
  scm_obj_t scm_subr_num_add = c_make_closure_s1((void*)subr_num_add, 2);
  c_global_set(make_symbol("+"), scm_subr_num_add);
  scm_obj_t scm_subr_num_sub = c_make_closure_s1((void*)subr_num_sub, 2);
  c_global_set(make_symbol("-"), scm_subr_num_sub);
  scm_obj_t scm_subr_num_eq = c_make_closure_s1((void*)subr_num_eq, 2);
  c_global_set(make_symbol("="), scm_subr_num_eq);
  scm_obj_t scm_subr_list = c_make_closure((void*)subr_list, 0, 1, 0, nullptr, scm_nil);
  c_global_set(make_symbol("list"), scm_subr_list);
  scm_obj_t scm_subr_car = c_make_closure_s1((void*)subr_car, 1);
  c_global_set(make_symbol("car"), scm_subr_car);
  scm_obj_t scm_subr_cdr = c_make_closure_s1((void*)subr_cdr, 1);
  c_global_set(make_symbol("cdr"), scm_subr_cdr);
  scm_obj_t scm_subr_not = c_make_closure_s1((void*)subr_not, 1);
  c_global_set(make_symbol("not"), scm_subr_not);
  scm_obj_t scm_subr_eq_p = c_make_closure_s1((void*)subr_eq_p, 2);
  c_global_set(make_symbol("eq?"), scm_subr_eq_p);
  scm_obj_t scm_subr_pair_p = c_make_closure_s1((void*)subr_pair_p, 1);
  c_global_set(make_symbol("pair?"), scm_subr_pair_p);
  scm_obj_t scm_subr_null_p = c_make_closure_s1((void*)subr_null_p, 1);
  c_global_set(make_symbol("null?"), scm_subr_null_p);
  scm_obj_t scm_subr_cadr = c_make_closure_s1((void*)subr_cadr, 1);
  c_global_set(make_symbol("cadr"), scm_subr_cadr);
  scm_obj_t scm_subr_caddr = c_make_closure_s1((void*)subr_caddr, 1);
  c_global_set(make_symbol("caddr"), scm_subr_caddr);
}

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

  // setup SUBR
  setup_subr();

  puts(";; nanos - a small scheme interpreter for bootstrapping");

#if USE_TBI
  puts(";; USE_TBI == 1");
#else
  puts(";; USE_TBI == 0");
#endif

  codegen_t codegen(context, engine);

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
        printf("(0x%016lx)\n", result);
        printer.print((scm_obj_t)result);
        puts("");
      } catch (std::exception& e) {
        printf("error: %s\n", e.what());
      }
    } else {
      printer.print(obj);
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
