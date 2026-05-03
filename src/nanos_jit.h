// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef NANOS_JIT_H_INCLUDED
#define NANOS_JIT_H_INCLUDED

#include "core.h"

#include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
#include <llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h>
#include <llvm/ExecutionEngine/Orc/EPCIndirectionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/MapperJITLinkMemoryManager.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/SelfExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h>

class nanos_jit_t {
 public:
  nanos_jit_t(std::unique_ptr<llvm::orc::ExecutionSession> ES, std::unique_ptr<llvm::orc::EPCIndirectionUtils> EPCIU,
              llvm::orc::JITTargetMachineBuilder JTMB, llvm::DataLayout DL);
  ~nanos_jit_t();

  static llvm::Expected<std::unique_ptr<nanos_jit_t>> Create();

  const llvm::DataLayout &getDataLayout() const { return DL; }
  const llvm::Triple &getTargetTriple() const { return ES->getExecutorProcessControl().getTargetTriple(); }
  llvm::orc::JITDylib &getMainJITDylib() { return MainJD; }
  llvm::orc::ExecutionSession &getExecutionSession() { return *ES; }
  llvm::orc::SymbolStringPtr mangleAndIntern(llvm::StringRef Name) { return Mangle(Name); }
  llvm::Error addIRModule(llvm::orc::ThreadSafeModule TSM, llvm::orc::ResourceTrackerSP RT = nullptr);
  llvm::Expected<llvm::orc::ExecutorAddr> lookup(llvm::StringRef Name);

 private:
  static void handleLazyCallThroughError();

  std::unique_ptr<llvm::orc::ExecutionSession> ES;
  std::unique_ptr<llvm::orc::EPCIndirectionUtils> EPCIU;
  std::unique_ptr<llvm::orc::ObjectLinkingLayer> ObjectLayer;
  std::unique_ptr<llvm::orc::IRCompileLayer> CompileLayer;
  std::unique_ptr<llvm::orc::CompileOnDemandLayer> CODLayer;
  llvm::DataLayout DL;
  llvm::orc::MangleAndInterner Mangle;
  llvm::orc::JITDylib &MainJD;
};

#endif
