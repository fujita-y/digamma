// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "nanos_jit.h"
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/Support/Error.h>

#define USE_WHOLE_MODULE_PARTITION_FUNCTION 0

using namespace llvm;
using namespace llvm::orc;

// ============================================================================
// Constructor & Destructor
// ============================================================================

nanos_jit_t::nanos_jit_t(std::unique_ptr<ExecutionSession> ES, std::unique_ptr<EPCIndirectionUtils> EPCIU, JITTargetMachineBuilder JTMB,
                         DataLayout DL)
    : ES(std::move(ES)),
      EPCIU(std::move(EPCIU)),
      DL(std::move(DL)),
      Mangle(*this->ES, this->DL),
      ObjectLayer(std::make_unique<ObjectLinkingLayer>(*this->ES, this->ES->getExecutorProcessControl().getMemMgr())),
      CompileLayer(std::make_unique<IRCompileLayer>(*this->ES, *ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB)))),
      CODLayer(std::make_unique<CompileOnDemandLayer>(*this->ES, *CompileLayer, this->EPCIU->getLazyCallThroughManager(),
                                                      [this]() { return this->EPCIU->createIndirectStubsManager(); })),
      MainJD(this->ES->createBareJITDylib("<main>")) {
  MainJD.addGenerator(cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->DL.getGlobalPrefix())));
#if USE_WHOLE_MODULE_PARTITION_FUNCTION
  CODLayer->setPartitionFunction(CompileOnDemandLayer::compileWholeModule);
#else
  CODLayer->setPartitionFunction(CompileOnDemandLayer::compileRequested);
#endif
#ifndef NDEBUG
  CompileLayer->setNotifyCompiled([](MaterializationResponsibility &R, ThreadSafeModule TSM) { errs() << "Generating native code...\n"; });
#endif
}

nanos_jit_t::~nanos_jit_t() {
  CODLayer.reset();
  CompileLayer.reset();
  ObjectLayer.reset();
  if (auto Err = EPCIU->cleanup()) {
    ES->reportError(std::move(Err));
  }
  EPCIU.reset();
  if (auto Err = ES->endSession()) {
    ES->reportError(std::move(Err));
  }
}

// ============================================================================
// Factory
// ============================================================================

Expected<std::unique_ptr<nanos_jit_t>> nanos_jit_t::Create() {
  auto EPC = SelfExecutorProcessControl::Create(
      nullptr, std::make_unique<DynamicThreadPoolTaskDispatcher>(std::make_optional(std::thread::hardware_concurrency())));
  if (!EPC) return EPC.takeError();

  auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

  auto EPCIU = EPCIndirectionUtils::Create(*ES);
  if (!EPCIU) return EPCIU.takeError();

  (*EPCIU)->createLazyCallThroughManager(*ES, ExecutorAddr::fromPtr(&handleLazyCallThroughError));

  if (auto Err = setUpInProcessLCTMReentryViaEPCIU(**EPCIU)) return std::move(Err);

  JITTargetMachineBuilder JTMB(ES->getExecutorProcessControl().getTargetTriple());

  JTMB.setCodeModel(llvm::CodeModel::Small);
  JTMB.setRelocationModel(llvm::Reloc::PIC_);
  JTMB.setCodeGenOptLevel(llvm::CodeGenOptLevel::Default);
  JTMB.getOptions().GuaranteedTailCallOpt = true;

  auto DL = JTMB.getDefaultDataLayoutForTarget();
  if (!DL) return DL.takeError();

  return std::make_unique<nanos_jit_t>(std::move(ES), std::move(*EPCIU), std::move(JTMB), std::move(*DL));
}

void nanos_jit_t::handleLazyCallThroughError() {
  errs() << "LazyCallThrough error.\n";
  exit(1);
}

// ============================================================================
// JIT Operations
// ============================================================================

Error nanos_jit_t::addIRModule(ThreadSafeModule TSM, ResourceTrackerSP RT) {
  if (!RT) RT = MainJD.getDefaultResourceTracker();
  return CODLayer->add(std::move(RT), std::move(TSM));
}

Expected<ExecutorAddr> nanos_jit_t::lookup(StringRef Name) {
  auto Sym = ES->lookup({&MainJD}, Mangle(Name.str()));
  if (!Sym) return Sym.takeError();
  return Sym->getAddress();
}
