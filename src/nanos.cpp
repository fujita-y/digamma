#include "object.h"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
#include "codegen.h"
#include "codegen_aux.h"
#include "nanos_subr.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

static void setup_subr() {
  scm_obj_t scm_subr_num_add = make_closure((void*)subr_num_add, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("+"), scm_subr_num_add);
  scm_obj_t scm_subr_num_sub = make_closure((void*)subr_num_sub, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("-"), scm_subr_num_sub);
  scm_obj_t scm_subr_num_eq = make_closure((void*)subr_num_eq, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("="), scm_subr_num_eq);
  scm_obj_t scm_subr_list = make_closure((void*)subr_list, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("list"), scm_subr_list);
  scm_obj_t scm_subr_car = make_closure((void*)subr_car, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("car"), scm_subr_car);
  scm_obj_t scm_subr_cdr = make_closure((void*)subr_cdr, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("cdr"), scm_subr_cdr);
  scm_obj_t scm_subr_not = make_closure((void*)subr_not, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("not"), scm_subr_not);
  scm_obj_t scm_subr_eq_p = make_closure((void*)subr_eq_p, 2, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("eq?"), scm_subr_eq_p);
  scm_obj_t scm_subr_pair_p = make_closure((void*)subr_pair_p, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("pair?"), scm_subr_pair_p);
  scm_obj_t scm_subr_null_p = make_closure((void*)subr_null_p, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("null?"), scm_subr_null_p);
  scm_obj_t scm_subr_cadr = make_closure((void*)subr_cadr, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("cadr"), scm_subr_cadr);
  scm_obj_t scm_subr_caddr = make_closure((void*)subr_caddr, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("caddr"), scm_subr_caddr);
  scm_obj_t scm_subr_cons = make_closure((void*)subr_cons, 2, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("cons"), scm_subr_cons);
  scm_obj_t scm_subr_apply = make_closure((void*)subr_apply, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("apply"), scm_subr_apply);
  scm_obj_t scm_subr_append = make_closure((void*)subr_append, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("append"), scm_subr_append);
  scm_obj_t scm_subr_write = make_closure((void*)subr_write, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("write"), scm_subr_write);
  scm_obj_t scm_subr_newline = make_closure((void*)subr_newline, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("newline"), scm_subr_newline);
}

int main() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // Configure JIT target machine with small code model
  auto jtmb = llvm::orc::JITTargetMachineBuilder::detectHost();
  if (!jtmb) {
    fprintf(stderr, "Failed to detect host target: %s\n", llvm::toString(jtmb.takeError()).c_str());
    return 1;
  }
  jtmb->setCodeModel(llvm::CodeModel::Small);

  auto jit_expected = llvm::orc::LLJITBuilder().setJITTargetMachineBuilder(std::move(*jtmb)).create();
  if (!jit_expected) {
    fprintf(stderr, "Failed to create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
    return 1;
  }
  auto jit = std::move(*jit_expected);

  // Allow the JIT to find symbols in the current process
  auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(jit->getDataLayout().getGlobalPrefix());
  if (!gen) {
    fprintf(stderr, "Failed to create symbol generator: %s\n", llvm::toString(gen.takeError()).c_str());
    return 1;
  }
  jit->getMainJITDylib().addGenerator(std::move(*gen));

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

  auto ts_ctx = std::make_unique<llvm::LLVMContext>();
  codegen_t codegen(llvm::orc::ThreadSafeContext(std::move(ts_ctx)), jit.get());

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
