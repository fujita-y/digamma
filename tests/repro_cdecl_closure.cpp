
#include "core.h"
#include <llvm/Support/TargetSelect.h>
#include <sstream>
#include "codegen.h"
#include "hash.h"
#include "object_heap.h"
#include "reader.h"

// Helper macros for cons access (copied from codegen.cpp)
#define CAR(x) (((scm_cons_rec_t*)(x))->car)
#define CDR(x) (((scm_cons_rec_t*)(x))->cdr)

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

class CodegenTest {
 public:
  std::unique_ptr<llvm::orc::LLJIT> jit;
  codegen_t* codegen;

  CodegenTest() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    auto jit_expected = llvm::orc::LLJITBuilder().create();
    if (!jit_expected) {
      fprintf(stderr, "Could not create LLJIT: %s\n", llvm::toString(jit_expected.takeError()).c_str());
      exit(1);
    }
    jit = std::move(*jit_expected);

    auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(jit->getDataLayout().getGlobalPrefix());
    if (!gen) {
      fprintf(stderr, "Failed to create symbol generator: %s\n", llvm::toString(gen.takeError()).c_str());
      exit(1);
    }
    jit->getMainJITDylib().addGenerator(std::move(*gen));

    auto ts_ctx = std::make_unique<llvm::LLVMContext>();
    codegen = new codegen_t(llvm::orc::ThreadSafeContext(std::move(ts_ctx)), jit.get());
  }

  ~CodegenTest() { delete codegen; }

  scm_obj_t read_code(const std::string& input) {
    std::istringstream is(input);
    reader_t reader(is);
    bool err = false;
    scm_obj_t obj = reader.read(err);
    if (err) {
      fatal("Read error: %s", reader.get_error_message().c_str());
    }
    return obj;
  }
};

inline scm_obj_t tc6_pointer(void* x, uintptr_t tc6_num) {
  assert(((uintptr_t)x & 0x07) == 0);
#if USE_TBI
  return (uintptr_t)x | 0x02 | tc6_num << 57;
#else
  return (uintptr_t)x | 0x02;
#endif
}

inline scm_tc6_t tc6_tag(uintptr_t tc6_num) { return (tc6_num << 8) | 0x06; }

static int cdecl_called = 0;
extern "C" scm_obj_t my_c_function(scm_obj_t self, scm_obj_t x) {
  cdecl_called++;
  return x;
}
extern "C" scm_obj_t my_c_function_rest(scm_obj_t self, int argc, scm_obj_t* argv) {
  cdecl_called += argc;
  return argc > 0 ? argv[0] : scm_nil;
}

int main(int argc, char** argv) {
  printf("Starting repro_cdecl_closure\n");
  fflush(stdout);
  object_heap_t heap;
  heap.init(1024 * 1024 * 2, 1024 * 1024);

  {
    printf("Testing CDECL closure (normal argument passing)...\n");
    CodegenTest env;

    scm_closure_rec_t* rec = (scm_closure_rec_t*)heap.alloc_collectible(sizeof(scm_closure_rec_t));
    rec->tag = tc6_tag(tc6_closure);
    rec->literals = scm_nil;
    rec->code = (void*)&my_c_function;
    rec->argc = 1;   // 1 fixed arg
    rec->rest = 0;   // No rest
    rec->cdecl = 1;  // CDECL!
    rec->nsize = 0;

    scm_obj_t closure = tc6_pointer(rec, tc6_closure);

    heap.environment_variable_set(make_symbol("foo"), closure);

    // Use r10 for closure, r0 for argument
    scm_obj_t code = env.read_code(
        "((global-ref r10 foo) "
        "(const r0 123) "
        "(call r10 1) "
        "(ret))");

    cdecl_called = 0;
    intptr_t result = env.codegen->compile(code);

    if (cdecl_called != 1) {
      printf("FAILED: C function not called (count=%d)\n", cdecl_called);
      return 1;
    }
    if (result != make_fixnum(123)) {
      printf("FAILED: Result mismatch: expected 123, got %ld\n", result);
      return 1;
    }
    printf("PASSED: CDECL normal call\n");
  }

  {
    printf("Testing CDECL closure (rest argument passing)...\n");
    CodegenTest env;

    scm_closure_rec_t* rec = (scm_closure_rec_t*)heap.alloc_collectible(sizeof(scm_closure_rec_t));
    rec->tag = tc6_tag(tc6_closure);
    rec->literals = scm_nil;
    rec->code = (void*)&my_c_function_rest;
    rec->argc = 0;   // 0 fixed args
    rec->rest = 1;   // Rest args
    rec->cdecl = 1;  // CDECL!
    rec->nsize = 0;

    scm_obj_t closure = tc6_pointer(rec, tc6_closure);

    heap.environment_variable_set(make_symbol("bar"), closure);

    // Use r10 for closure, r0, r1 for arguments
    scm_obj_t code = env.read_code(
        "((global-ref r10 bar) "
        "(const r0 456) (const r1 789) "
        "(call r10 2) "
        "(ret))");

    cdecl_called = 0;
    intptr_t result = env.codegen->compile(code);

    if (cdecl_called != 2) {
      printf("FAILED: C function (rest) not called correctly (count=%d)\n", cdecl_called);
      return 1;
    }
    if (result != make_fixnum(456)) {
      printf("FAILED: Result mismatch (rest): expected 456, got %ld\n", result);
      return 1;
    }
    printf("PASSED: CDECL rest call\n");
  }

  heap.destroy();
  return 0;
}
