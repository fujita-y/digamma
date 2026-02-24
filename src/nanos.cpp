#include "object.h"
#include "nanos.h"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
#include "codegen.h"
#include "codegen_aux.h"
#include "nanos_context.h"
#include "nanos_subr.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

void nanos_t::init_subr() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    c_global_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, scm_nil, 1));
  };
  auto make_subr = [](void* func, int req, int opt) { return make_closure(func, req, opt, 0, nullptr, scm_nil, 1); };

  reg("+", (void*)subr_num_add, 0, 1);
  reg("-", (void*)subr_num_sub, 1, 1);
  reg("=", (void*)subr_num_eq, 1, 1);
  reg("<", (void*)subr_num_lt, 1, 1);
  reg("list", (void*)subr_list, 0, 1);
  reg("car", (void*)subr_car, 1, 0);
  reg("cdr", (void*)subr_cdr, 1, 0);
  reg("not", (void*)subr_not, 1, 0);
  reg("eq?", (void*)subr_eq_p, 2, 0);
  reg("pair?", (void*)subr_pair_p, 1, 0);
  reg("null?", (void*)subr_null_p, 1, 0);
  reg("cadr", (void*)subr_cadr, 1, 0);
  reg("caddr", (void*)subr_caddr, 1, 0);
  reg("cons", (void*)subr_cons, 2, 0);
  reg("apply", (void*)subr_apply, 0, 1);
  reg("append", (void*)subr_append, 0, 1);
  reg("write", (void*)subr_write, 1, 0);
  reg("display", (void*)subr_display, 1, 0);
  reg("newline", (void*)subr_newline, 0, 0);
  reg("collect", (void*)subr_collect, 0, 0);
  reg("safepoint", (void*)subr_safepoint, 0, 0);
  reg("call/ec", (void*)subr_call_ec, 1, 0);
  reg("dynamic-wind", (void*)subr_dynamic_wind, 3, 0);
  reg("continuation?", (void*)subr_continuation_p, 1, 0);

  scm_obj_t scm_subr_call_cc = make_subr((void*)subr_call_cc, 1, 0);
  c_global_set(make_symbol("call/cc"), scm_subr_call_cc);
  c_global_set(make_symbol("call-with-current-continuation"), scm_subr_call_cc);
}

void nanos_t::init_codegen() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // Configure JIT target machine with small code model
  auto jtmb = llvm::orc::JITTargetMachineBuilder::detectHost();
  if (!jtmb) {
    fatal("Failed to detect host target: %s\n", llvm::toString(jtmb.takeError()).c_str());
  }
  jtmb->setCodeModel(llvm::CodeModel::Small);
  jtmb->getOptions().GuaranteedTailCallOpt = true;

  auto jit_expected = llvm::orc::LLJITBuilder().setJITTargetMachineBuilder(std::move(*jtmb)).create();
  if (!jit_expected) {
    fatal("Failed to create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
  }
  m_jit = std::move(*jit_expected);

  // Allow the JIT to find symbols in the current process
  auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(m_jit->getDataLayout().getGlobalPrefix());
  if (!gen) {
    fatal("Failed to create symbol generator: %s\n", llvm::toString(gen.takeError()).c_str());
  }
  m_jit->getMainJITDylib().addGenerator(std::move(*gen));

  auto ts_ctx = std::make_unique<llvm::LLVMContext>();
  new codegen_t(llvm::orc::ThreadSafeContext(std::move(ts_ctx)), m_jit.get());
}

void nanos_t::init() {
  object_heap_t* heap = new object_heap_t();
  heap->init((size_t)DEFAULT_HEAP_LIMIT * 1024 * 1024, 4 * 1024 * 1024);
  init_codegen();
  init_subr();
}

void nanos_t::destroy() {
  object_heap_t::current()->destroy();
  m_jit.reset();
}

void nanos_t::run() {
  puts(";; nanos - a small scheme interpreter for bootstrapping");
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
