#include "object.h"
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
#include "codegen.h"
#include "codegen_aux.h"
#include "nanos_context.h"
#include "nanos_subr.h"
#include "object_heap.h"
#include "printer.h"
#include "reader.h"

static void setup_subr() {
  scm_obj_t scm_subr_num_add = make_closure((void*)subr_num_add, 0, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("+"), scm_subr_num_add);
  scm_obj_t scm_subr_num_sub = make_closure((void*)subr_num_sub, 1, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("-"), scm_subr_num_sub);
  scm_obj_t scm_subr_num_eq = make_closure((void*)subr_num_eq, 1, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("="), scm_subr_num_eq);
  scm_obj_t scm_subr_num_lt = make_closure((void*)subr_num_lt, 1, 1, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("<"), scm_subr_num_lt);
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
  c_global_set(make_symbol("display"), scm_subr_write);
  scm_obj_t scm_subr_newline = make_closure((void*)subr_newline, 0, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("newline"), scm_subr_newline);
  scm_obj_t scm_subr_collect = make_closure((void*)subr_collect, 0, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("collect"), scm_subr_collect);
  scm_obj_t scm_subr_safepoint = make_closure((void*)subr_safepoint, 0, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("safepoint"), scm_subr_safepoint);
  scm_obj_t scm_subr_call_ec = make_closure((void*)subr_call_ec, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("call/ec"), scm_subr_call_ec);
  scm_obj_t scm_subr_call_cc = make_closure((void*)subr_call_cc, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("call/cc"), scm_subr_call_cc);
  c_global_set(make_symbol("call-with-current-continuation"), scm_subr_call_cc);
  scm_obj_t scm_subr_dynamic_wind = make_closure((void*)subr_dynamic_wind, 3, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("dynamic-wind"), scm_subr_dynamic_wind);
  scm_obj_t scm_subr_continuation_p = make_closure((void*)subr_continuation_p, 1, 0, 0, nullptr, scm_nil, 1);
  c_global_set(make_symbol("continuation?"), scm_subr_continuation_p);
}

static thread_local std::unique_ptr<llvm::orc::LLJIT> s_jit;
static thread_local std::unique_ptr<codegen_t> s_codegen;

static codegen_t* init_codegen() {
  if (s_codegen) return s_codegen.get();

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // Configure JIT target machine with small code model
  auto jtmb = llvm::orc::JITTargetMachineBuilder::detectHost();
  if (!jtmb) {
    fprintf(stderr, "Failed to detect host target: %s\n", llvm::toString(jtmb.takeError()).c_str());
    exit(1);
  }
  jtmb->setCodeModel(llvm::CodeModel::Small);
  jtmb->getOptions().GuaranteedTailCallOpt = true;

  auto jit_expected = llvm::orc::LLJITBuilder().setJITTargetMachineBuilder(std::move(*jtmb)).create();
  if (!jit_expected) {
    fprintf(stderr, "Failed to create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
    exit(1);
  }
  s_jit = std::move(*jit_expected);

  // Allow the JIT to find symbols in the current process
  auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(s_jit->getDataLayout().getGlobalPrefix());
  if (!gen) {
    fprintf(stderr, "Failed to create symbol generator: %s\n", llvm::toString(gen.takeError()).c_str());
    exit(1);
  }
  s_jit->getMainJITDylib().addGenerator(std::move(*gen));

  auto ts_ctx = std::make_unique<llvm::LLVMContext>();
  s_codegen = std::make_unique<codegen_t>(llvm::orc::ThreadSafeContext(std::move(ts_ctx)), s_jit.get());
  return s_codegen.get();
}

static void destroy_codegen() {
  s_codegen.reset();
  s_jit.reset();
}

#include "nanos.h"

void nanos_t::init() {
  heap = new object_heap_t();
  heap->init((size_t)DEFAULT_HEAP_LIMIT * 1024 * 1024, 4 * 1024 * 1024);

  codegen = init_codegen();
  setup_subr();
}

void nanos_t::destroy() {
  destroy_codegen();
  heap->destroy();
  delete heap;
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
        auto func = codegen->compile(obj);
        intptr_t result = func();
        printf("(0x%016lx)\n", result);
        printer.print((scm_obj_t)result);
        puts("");
      } catch (std::exception& e) {
        printf("%s\n", e.what());
      }
    } else {
      printer.print(obj);
      puts("");
    }
  }
}
