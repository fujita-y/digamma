// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "codegen.h"
#include "context.h"
#include "nanos_jit.h"
#include "object_heap.h"
#include "subr.h"

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Host.h>

#include <atomic>
#include <cstdarg>
#include <cstdio>
#include <cstring>
#include <dlfcn.h>
#include <stdexcept>
#include <string>

using namespace llvm;
using namespace llvm::orc;
using namespace llvm::sys;

// ============================================================================
// Static state
// ============================================================================

static std::mutex s_codegen_lock;
static std::unordered_map<std::string, void*> s_callout_cache;
static std::atomic<int> s_trampoline_uid;
static std::atomic<int> s_cffi_uid;

// ============================================================================
// Helpers
// ============================================================================

static std::string generate_uid_string() {
  int id = s_cffi_uid.fetch_add(1, std::memory_order_relaxed);
  return "cffi_" + std::format("{:06}", id);
}

static inline bool is_valid_type_code(char c) { return c && strchr("budqosx", c); }

static inline bool is_exact_non_negative_integer(scm_obj_t obj) { return is_fixnum(obj) && fixnum(obj) >= 0; }

static inline bool exact_integer_to_uintptr(scm_obj_t obj, uintptr_t* ans) {
  if (is_fixnum(obj)) {
    intptr_t v = fixnum(obj);
    if (v < 0) return false;
    *ans = (uintptr_t)v;
    return true;
  }
  return false;
}

static inline scm_obj_t uintptr_to_integer(uintptr_t val) {
  intptr_t sval = (intptr_t)val;
  if (sval >= 0) return make_fixnum(sval);
  return scm_false;
}

static inline const char* extract_name(scm_obj_t obj) { return (const char*)(is_symbol(obj) ? symbol_name(obj) : string_name(obj)); }

// ============================================================================
// C FFI type converters  (called from JIT-compiled thunks)
// ============================================================================

extern "C" {
  static int8_t c_ffi_to_Int8Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int8_t)fixnum(obj);
    if (is_flonum(obj)) return (int8_t)flonum(obj);
    return 0;
  }
  static int16_t c_ffi_to_Int16Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int16_t)fixnum(obj);
    if (is_flonum(obj)) return (int16_t)flonum(obj);
    return 0;
  }
  static int32_t c_ffi_to_Int32Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int32_t)fixnum(obj);
    if (is_flonum(obj)) return (int32_t)flonum(obj);
    return 0;
  }
  static int64_t c_ffi_to_Int64Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int64_t)fixnum(obj);
    if (is_flonum(obj)) return (int64_t)flonum(obj);
    if (is_string(obj)) return (int64_t)(intptr_t)string_name(obj);
    if (is_symbol(obj)) return (int64_t)(intptr_t)symbol_name(obj);
    if (is_u8vector(obj)) return (int64_t)(intptr_t)u8vector_elts(obj);
    return 0;
  }
  static float c_ffi_to_FloatTy(scm_obj_t obj) {
    if (is_flonum(obj)) return (float)flonum(obj);
    if (is_fixnum(obj)) return (float)fixnum(obj);
    return 0.0f;
  }
  static double c_ffi_to_DoubleTy(scm_obj_t obj) {
    if (is_flonum(obj)) return flonum(obj);
    if (is_fixnum(obj)) return (double)fixnum(obj);
    return 0.0;
  }

  static scm_obj_t c_ffi_from_Int1Ty(bool val) { return val ? scm_true : scm_false; }
  static scm_obj_t c_ffi_from_Int8Ty(int8_t val) { return make_fixnum(val); }
  static scm_obj_t c_ffi_from_Int16Ty(int16_t val) { return make_fixnum(val); }
  static scm_obj_t c_ffi_from_Int32Ty(int32_t val) { return make_fixnum((intptr_t)val); }
  static scm_obj_t c_ffi_from_Int64Ty(int64_t val) { return make_fixnum((intptr_t)val); }
  static scm_obj_t c_ffi_from_FloatTy(float val) { return make_flonum((double)val); }
  static scm_obj_t c_ffi_from_DoubleTy(double val) { return make_flonum(val); }

  static int8_t c_ffi_ret_Int8Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int8_t)fixnum(obj);
    if (is_flonum(obj)) return (int8_t)flonum(obj);
    return 0;
  }
  static int16_t c_ffi_ret_Int16Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int16_t)fixnum(obj);
    if (is_flonum(obj)) return (int16_t)flonum(obj);
    return 0;
  }
  static int32_t c_ffi_ret_Int32Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int32_t)fixnum(obj);
    if (is_flonum(obj)) return (int32_t)flonum(obj);
    return 0;
  }
  static int64_t c_ffi_ret_Int64Ty(scm_obj_t obj) {
    if (is_fixnum(obj)) return (int64_t)fixnum(obj);
    if (is_flonum(obj)) return (int64_t)flonum(obj);
    if (is_u8vector(obj)) return (int64_t)(intptr_t)u8vector_elts(obj);
    if (is_string(obj)) return (int64_t)(intptr_t)string_name(obj);
    if (is_symbol(obj)) return (int64_t)(intptr_t)symbol_name(obj);
    return 0;
  }
  static float c_ffi_ret_FloatTy(scm_obj_t obj) {
    if (is_flonum(obj)) return (float)flonum(obj);
    if (is_fixnum(obj)) return (float)fixnum(obj);
    return 0.0f;
  }
  static double c_ffi_ret_DoubleTy(scm_obj_t obj) {
    if (is_flonum(obj)) return flonum(obj);
    if (is_fixnum(obj)) return (double)fixnum(obj);
    return 0.0;
  }
}  // extern "C"

// ============================================================================
// LLVM IR helpers
// ============================================================================

static Type* builtin_type(LLVMContext& C, char code) {
  switch (code) {
    case 'i':
      return Type::getVoidTy(C);
    case 'b':
      return Type::getInt1Ty(C);
    case 'u':
      return Type::getInt8Ty(C);
    case 'd':
      return Type::getInt16Ty(C);
    case 'q':
      return Type::getInt32Ty(C);
    case 'o':
      return Type::getInt64Ty(C);
    case 's':
      return Type::getFloatTy(C);
    case 'x':
      return Type::getDoubleTy(C);
    default:
      throw std::runtime_error(std::string("assertion-violation: cffi: unknown type code: ") + code);
  }
}

static FunctionType* function_type(LLVMContext& C, const char* signature, bool variadic) {
  std::vector<llvm::Type*> paramTypes;
  int i = 1;
  while (signature[i]) {
    paramTypes.push_back(builtin_type(C, signature[i]));
    i++;
  }
  return FunctionType::get(builtin_type(C, signature[0]), paramTypes, variadic);
}

static llvm::FunctionCallee make_callee(IRBuilder<>& IRB, LLVMContext& C, FunctionType* funcType, void* adrs) {
  return FunctionCallee(funcType, ConstantExpr::getIntToPtr(IRB.getInt64((intptr_t)adrs), PointerType::getUnqual(C)));
}

// "from" thunks: (C-type) -> scm_obj_t
static std::unordered_map<char, FunctionCallee> create_thunk_from_map(IRBuilder<>& IRB, LLVMContext& C) {
  auto IntptrTy = Type::getInt64Ty(C);
  auto F = [&](void* fn, Type* argTy) { return make_callee(IRB, C, FunctionType::get(IntptrTy, {argTy}, false), fn); };
  return {
      {'b', F((void*)c_ffi_from_Int1Ty, Type::getInt1Ty(C))},     {'u', F((void*)c_ffi_from_Int8Ty, Type::getInt8Ty(C))},
      {'d', F((void*)c_ffi_from_Int16Ty, Type::getInt16Ty(C))},   {'q', F((void*)c_ffi_from_Int32Ty, Type::getInt32Ty(C))},
      {'o', F((void*)c_ffi_from_Int64Ty, Type::getInt64Ty(C))},   {'s', F((void*)c_ffi_from_FloatTy, Type::getFloatTy(C))},
      {'x', F((void*)c_ffi_from_DoubleTy, Type::getDoubleTy(C))},
  };
}

// "to" thunks: (scm_obj_t obj) -> C-type
static std::unordered_map<char, FunctionCallee> create_thunk_to_map(IRBuilder<>& IRB, LLVMContext& C) {
  auto IntptrTy = Type::getInt64Ty(C);
  auto F = [&](void* fn, Type* retTy) { return make_callee(IRB, C, FunctionType::get(retTy, {IntptrTy}, false), fn); };
  return {
      {'b', F((void*)c_ffi_to_Int8Ty, Type::getInt8Ty(C))},  // bool handled via Int8
      {'u', F((void*)c_ffi_to_Int8Ty, Type::getInt8Ty(C))},   {'d', F((void*)c_ffi_to_Int16Ty, Type::getInt16Ty(C))},
      {'q', F((void*)c_ffi_to_Int32Ty, Type::getInt32Ty(C))}, {'o', F((void*)c_ffi_to_Int64Ty, Type::getInt64Ty(C))},
      {'s', F((void*)c_ffi_to_FloatTy, Type::getFloatTy(C))}, {'x', F((void*)c_ffi_to_DoubleTy, Type::getDoubleTy(C))},
  };
}

// "ret" thunks: (scm_obj_t) -> C-type
static std::unordered_map<char, FunctionCallee> create_thunk_ret_map(IRBuilder<>& IRB, LLVMContext& C) {
  auto IntptrTy = Type::getInt64Ty(C);
  auto F = [&](void* fn, Type* retTy) { return make_callee(IRB, C, FunctionType::get(retTy, {IntptrTy}, false), fn); };
  return {
      {'b', F((void*)c_ffi_ret_Int8Ty, Type::getInt8Ty(C))},     {'u', F((void*)c_ffi_ret_Int8Ty, Type::getInt8Ty(C))},
      {'d', F((void*)c_ffi_ret_Int16Ty, Type::getInt16Ty(C))},   {'q', F((void*)c_ffi_ret_Int32Ty, Type::getInt32Ty(C))},
      {'o', F((void*)c_ffi_ret_Int64Ty, Type::getInt64Ty(C))},   {'s', F((void*)c_ffi_ret_FloatTy, Type::getFloatTy(C))},
      {'x', F((void*)c_ffi_ret_DoubleTy, Type::getDoubleTy(C))},
  };
}

static void optimize_module(llvm::Module& mod) {
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);
  MPM.run(mod, MAM);
}

// Optimize, verify, and submit a module to the JIT; return the compiled function pointer.
static void* finalize_jit_module(nanos_jit_t* jit, std::unique_ptr<Module> M, std::unique_ptr<LLVMContext> Context,
                                 const std::string& function_id, const char* context_name) {
  optimize_module(*M);
  if (verifyModule(*M, &outs()))
    throw std::runtime_error(std::string("assertion-violation: ") + context_name + ": LLVM module verification failed");
  if (auto err = jit->addIRModule(ThreadSafeModule(std::move(M), std::move(Context))))
    throw std::runtime_error(std::string("assertion-violation: ") + context_name + ": failed to add IR module: " + toString(std::move(err)));
  auto sym = jit->lookup(function_id);
  if (!sym)
    throw std::runtime_error(std::string("assertion-violation: ") + context_name + ": symbol lookup failed: " + toString(sym.takeError()));
  return (void*)sym->getValue();
}

// ============================================================================
// compile_callout_thunk
//
// JIT-compiles a thin wrapper function that:
//   1. Converts each Scheme argument in argv[] to the appropriate C type.
//   2. Calls the native C function at `adrs` with the converted args.
//   3. Converts the return value back to a Scheme object.
//
// The generated function has the cdecl signature expected by Nanos closures
// with cdecl=1:
//     scm_obj_t thunk(scm_obj_t self, int argc, scm_obj_t* argv)
//
// caller_signature: describes how the Scheme side passes arguments.
// callee_signature: describes the actual C function's ABI.
// Both use the character encoding: i=void, b=bool, u=i8, d=i16, q=i32,
//   o=i64, s=float, x=double; first char is return type, rest are args.
// ============================================================================

static void* compile_callout_thunk(uintptr_t adrs, const char* caller_signature, const char* callee_signature) {
  char cache_key[512];
  snprintf(cache_key, sizeof(cache_key), "%s:%s:%p", caller_signature, callee_signature, (void*)adrs);
  auto it = s_callout_cache.find(cache_key);
  if (it != s_callout_cache.end()) return it->second;

  nanos_jit_t* jit = codegen_t::current()->get_jit();

  std::string module_id = generate_uid_string();
  std::string function_id = generate_uid_string();

  auto Context = std::make_unique<LLVMContext>();
  LLVMContext& C = *Context;
  auto M = std::make_unique<Module>(module_id, C);
  M->setDataLayout(jit->getDataLayout());
  M->setTargetTriple(jit->getTargetTriple());

  auto IntptrTy = Type::getInt64Ty(C);

  // Generated thunk signature: (self: intptr, argc: intptr, argv: intptr) -> intptr
  // (cdecl closure ABI used by Nanos)
  Function* F =
      Function::Create(FunctionType::get(IntptrTy, {IntptrTy, IntptrTy, IntptrTy}, false), Function::ExternalLinkage, function_id, M.get());

  BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
  IRBuilder<> IRB(ENTRY);

  auto thunk_to = create_thunk_to_map(IRB, C);
  auto thunk_from = create_thunk_from_map(IRB, C);

  // F args: self=arg0, argc=arg1, argv=arg2
  auto argv_val = F->arg_begin() + 2;  // intptr_t argv (pointer as integer)

  // Convert Scheme arguments → C arguments
  std::vector<llvm::Value*> args;
  int n = (int)strlen(caller_signature) - 1;
  for (int i = 0; i < n; i++) {
    char code = caller_signature[i + 1];
    if (!is_valid_type_code(code)) throw std::runtime_error(std::string("assertion-violation: codegen-cdecl-callout: wrong type code: ") + code);
    Value* arg_ptr = IRB.CreateGEP(IntptrTy, IRB.CreateIntToPtr(argv_val, PointerType::getUnqual(C)), IRB.getInt64((intptr_t)i));
    Value* arg_obj = IRB.CreateLoad(IntptrTy, arg_ptr);
    Value* value = IRB.CreateCall(thunk_to[code], {arg_obj});
    args.push_back(value);
  }

  // Call the native C function
  auto procType = function_type(C, callee_signature, strcmp(caller_signature, callee_signature) != 0);
  auto proc = ConstantExpr::getIntToPtr(IRB.getInt64((intptr_t)adrs), PointerType::getUnqual(C));
  auto retval = IRB.CreateCall(procType, proc, args);

  // Convert C return value → Scheme object
  if (callee_signature[0] == 'i') {
    IRB.CreateRet(IRB.getInt64((intptr_t)scm_unspecified));
  } else {
    char code = callee_signature[0];
    if (!is_valid_type_code(code))
      throw std::runtime_error(std::string("assertion-violation: codegen-cdecl-callout: wrong return type code: ") + code);
    IRB.CreateRet(IRB.CreateCall(thunk_from[code], {retval}));
  }

  void* ptr = finalize_jit_module(jit, std::move(M), std::move(Context), function_id, "codegen-cdecl-callout");
  s_callout_cache[cache_key] = ptr;
  return ptr;
}

// ============================================================================
// subr_codegen_cdecl_callout
//
// (codegen-cdecl-callout address caller-signature)
// (codegen-cdecl-callout address caller-signature callee-signature)
//
// Returns a Nanos closure wrapping the JIT-compiled callout thunk.
// The closure has cdecl=1 so it is called with the standard cdecl ABI.
// ============================================================================

SUBR subr_codegen_cdecl_callout(scm_obj_t self, int argc, scm_obj_t argv[]) {
  std::lock_guard<std::mutex> lock(s_codegen_lock);

  if (argc != 2 && argc != 3)
    throw std::runtime_error("assertion-violation: codegen-cdecl-callout: wrong number of arguments (expected 2 or 3)");

  if (!is_exact_non_negative_integer(argv[0]))
    throw std::runtime_error("assertion-violation: codegen-cdecl-callout: argument 1 must be an exact non-negative integer");

  uintptr_t adrs = 0;
  if (!exact_integer_to_uintptr(argv[0], &adrs))
    throw std::runtime_error("assertion-violation: codegen-cdecl-callout: argument 1 out of range for a pointer");

  if (adrs == 0) throw std::runtime_error("assertion-violation: codegen-cdecl-callout: argument 1 must be a non-zero C-function address");

  if (!is_string(argv[1]))
    throw std::runtime_error("assertion-violation: codegen-cdecl-callout: argument 2 must be a string (caller signature)");

  const char* caller_signature = (const char*)string_name(argv[1]);

  const char* callee_signature = caller_signature;
  if (argc == 3) {
    if (!is_string(argv[2]))
      throw std::runtime_error("assertion-violation: codegen-cdecl-callout: argument 3 must be a string (callee signature)");
    callee_signature = (const char*)string_name(argv[2]);
  }

  void* thunk = compile_callout_thunk(adrs, caller_signature, callee_signature);

  // Count required args from caller signature (all args required, no rest).
  int req = (int)strlen(caller_signature) - 1;

  // make_closure(code, argc, rest, nenv, env[], cdecl)
  // rest=1  → Nanos calls as (self, argc, argv[])  which matches the wrapper's design
  // cdecl=1 → C calling convention
  return make_closure(thunk, req, 1, 0, nullptr, 1);
}

// Called from JIT-compiled callback stubs.
// Looks up the closure by uid, converts C args to Scheme objects, calls it,
// and returns the result.
extern "C" scm_obj_t c_call_scheme(uintptr_t trampoline_uid, intptr_t argc, ...) {
  scm_obj_t* param = (scm_obj_t*)alloca(sizeof(scm_obj_t) * argc);
  va_list ap;
  va_start(ap, argc);
  for (int i = 0; i < argc; i++) param[i] = va_arg(ap, scm_obj_t);
  va_end(ap);

  scm_obj_t closure = scm_undef;
  if (trampoline_uid < context::s_trampolines.size()) closure = context::s_trampolines[trampoline_uid];
  if (!is_closure(closure)) {
    throw std::runtime_error("assertion-violation: codegen-cdecl-callback: callback exists in different thread or was destroyed");
  }
  codegen_t::bridge_func_t bridge = codegen_t::current()->call_closure_bridge();
  return (scm_obj_t)bridge(closure, (int)argc, param);
}

// ============================================================================
// compile_callback_thunk
//
// JIT-compiles a native C function that:
//   1. Converts its C arguments to Scheme objects via thunk_from converters.
//   2. Calls c_call_scheme(trampoline_uid, argc, arg0, arg1, ...) which
//      dispatches into the registered Scheme closure.
//   3. Converts the Scheme return value back to the C return type via ret thunks.
//
// The generated function conforms to the native C ABI described by `signature`
// and can be passed as a function pointer to C code.
// ============================================================================

static void* compile_callback_thunk(uintptr_t trampoline_uid, const char* signature) {
  nanos_jit_t* jit = codegen_t::current()->get_jit();

  std::string module_id = generate_uid_string();
  std::string function_id = generate_uid_string();

  auto Context = std::make_unique<LLVMContext>();
  LLVMContext& C = *Context;
  auto M = std::make_unique<Module>(module_id, C);
  M->setDataLayout(jit->getDataLayout());
  M->setTargetTriple(jit->getTargetTriple());

  auto IntptrTy = Type::getInt64Ty(C);

  // The generated function has the native C signature described by `signature`.
  auto callbackFunctionType = function_type(C, signature, false);
  Function* F = Function::Create(callbackFunctionType, Function::ExternalLinkage, function_id, M.get());

  BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
  IRBuilder<> IRB(ENTRY);

  auto thunk_ret = create_thunk_ret_map(IRB, C);
  auto thunk_from = create_thunk_from_map(IRB, C);

  // Build argument list for c_call_scheme:
  //   (trampoline_uid: intptr, argc: intptr, arg0: scm_obj_t, ...)
  std::vector<llvm::Value*> args;
  args.push_back(IRB.getInt64((intptr_t)trampoline_uid));
  int n = (int)strlen(signature) - 1;
  args.push_back(IRB.getInt64((intptr_t)n));

  // Convert each C argument to a Scheme object
  for (int i = 0; i < n; i++) {
    char code = signature[i + 1];
    if (!is_valid_type_code(code))
      throw std::runtime_error(std::string("assertion-violation: codegen-cdecl-callback: wrong type code: ") + code);
    Value* carg = F->arg_begin() + i;
    Value* scm_val = IRB.CreateCall(thunk_from[code], {carg});
    args.push_back(scm_val);
  }

  // Call c_call_scheme (variadic: uid, argc, scm_obj_t...)
  auto thunkType = FunctionType::get(IntptrTy, {IntptrTy, IntptrTy}, true);
  auto thunk = ConstantExpr::getIntToPtr(IRB.getInt64((intptr_t)c_call_scheme), PointerType::getUnqual(C));
  Value* retval = IRB.CreateCall(thunkType, thunk, args);

  // Convert Scheme return value → C return type
  if (signature[0] == 'i') {
    IRB.CreateRetVoid();
  } else {
    char code = signature[0];
    if (!is_valid_type_code(code))
      throw std::runtime_error(std::string("assertion-violation: codegen-cdecl-callback: wrong return type code: ") + code);
    IRB.CreateRet(IRB.CreateCall(thunk_ret[code], {retval}));
  }

  return finalize_jit_module(jit, std::move(M), std::move(Context), function_id, "codegen-cdecl-callback");
}

// ============================================================================
// subr_codegen_cdecl_callback
//
// (codegen-cdecl-callback closure signature-string)
//
// Registers `closure` in the trampoline table and JIT-compiles a native C stub
// whose function pointer is returned as an exact integer so C code can call it.
// The stub will call back into `closure` with the C arguments converted to
// Scheme objects, and convert the Scheme return value back to C.
// ============================================================================

SUBR subr_codegen_cdecl_callback(scm_obj_t self, scm_obj_t a1, scm_obj_t a2) {
  std::lock_guard<std::mutex> lock(s_codegen_lock);
  if (!is_closure(a1)) throw std::runtime_error("assertion-violation: codegen-cdecl-callback: argument 1 must be a closure");
  if (!is_string(a2)) throw std::runtime_error("assertion-violation: codegen-cdecl-callback: argument 2 must be a string (signature)");

  scm_obj_t closure = a1;
  const char* signature = (const char*)string_name(a2);
  int uid = s_trampoline_uid.fetch_add(1);

  // Register the closure in the trampoline table.
  object_heap_t::current()->write_barrier(closure);
  if (uid >= context::s_trampolines.size()) context::s_trampolines.resize(uid + 1);
  context::s_trampolines[uid] = closure;

  void* thunk = compile_callback_thunk(uid, signature);
  return uintptr_to_integer((uintptr_t)thunk);
}

// ============================================================================
// subr_load_shared_object
//
// (load-shared-object)              ; opens the process itself (RTLD_DEFAULT)
// (load-shared-object path-string)  ; opens the .so at path
//
// Returns an exact integer handle on success, raises on failure.
// ============================================================================

SUBR subr_load_shared_object(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc == 0) {
    void* hdl = dlopen(NULL, RTLD_LAZY | RTLD_GLOBAL);
    if (hdl) return uintptr_to_integer((uintptr_t)hdl);
    throw std::runtime_error(std::string("assertion-violation: load-shared-object: ") + (dlerror() ? dlerror() : "unknown error"));
  }
  if (argc == 1) {
    if (!is_string(argv[0])) throw std::runtime_error("assertion-violation: load-shared-object: argument 1 must be a string");
    const char* path = (const char*)string_name(argv[0]);
    void* hdl = dlopen(path, RTLD_LAZY | RTLD_GLOBAL);
    if (hdl) return uintptr_to_integer((uintptr_t)hdl);
    throw std::runtime_error(std::string("assertion-violation: load-shared-object: ") + (dlerror() ? dlerror() : "unknown error"));
  }
  throw std::runtime_error("assertion-violation: load-shared-object: wrong number of arguments (expected 0 or 1)");
}

// ============================================================================
// subr_lookup_shared_object
//
// (lookup-shared-object name)          ; lookup in RTLD_DEFAULT
// (lookup-shared-object handle name)   ; lookup in specific handle
//
// name may be a string or symbol.
// handle must be an exact non-negative integer returned by load-shared-object.
// Returns an exact integer address, or #f if not found.
// ============================================================================

SUBR subr_lookup_shared_object(scm_obj_t self, int argc, scm_obj_t argv[]) {
  void* hdl = RTLD_DEFAULT;
  int name_idx = 0;

  if (argc == 2) {
    if (!is_exact_non_negative_integer(argv[0]))
      throw std::runtime_error(
          "assertion-violation: lookup-shared-object: argument 1 must be a shared object handle (exact non-negative integer)");
    uintptr_t hdl_val = 0;
    if (!exact_integer_to_uintptr(argv[0], &hdl_val))
      throw std::runtime_error("assertion-violation: lookup-shared-object: handle value out of range");
    hdl = (void*)hdl_val;
    name_idx = 1;
  } else if (argc != 1) {
    throw std::runtime_error("assertion-violation: lookup-shared-object: wrong number of arguments (expected 1 or 2)");
  }

  if (!is_string(argv[name_idx]) && !is_symbol(argv[name_idx]))
    throw std::runtime_error("assertion-violation: lookup-shared-object: name must be a string or symbol");
  uintptr_t adrs = (uintptr_t)dlsym(hdl, extract_name(argv[name_idx]));
  return adrs ? uintptr_to_integer(adrs) : scm_false;
}

// ============================================================================
// subr_string_utf8_nul
//
// (string->utf8/nul string)
//
// Returns a bytevector (u8vector) containing the UTF-8 encoding of `string`
// followed by a NUL terminator byte (\0), suitable for passing to C functions
// that expect a const char* null-terminated string.
// ============================================================================

SUBR subr_string_utf8_nul(scm_obj_t self, scm_obj_t a1) {
  if (!is_string(a1)) throw std::runtime_error("assertion-violation: string->utf8/nul: argument must be a string");
  const uint8_t* name = string_name(a1);
  int size = (int)strlen((const char*)name);  // byte count, NUL not included
  scm_obj_t bv = make_u8vector(size + 1);     // +1 for NUL terminator
  memcpy(u8vector_elts(bv), name, size + 1);  // copies data + NUL
  return bv;
}

void init_subr_cffi() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, 1));
  };

  reg("codegen-cdecl-callout", (void*)subr_codegen_cdecl_callout, 2, 1);
  reg("codegen-cdecl-callback", (void*)subr_codegen_cdecl_callback, 2, 0);
  reg("load-shared-object", (void*)subr_load_shared_object, 0, 1);
  reg("lookup-shared-object", (void*)subr_lookup_shared_object, 1, 1);
  reg("string->utf8/nul", (void*)subr_string_utf8_nul, 1, 0);
}
