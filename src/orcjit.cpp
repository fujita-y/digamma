#include "core.h"
#include "object.h"

#if ENABLE_LLVM_JIT

#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/Support/Error.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llvm::orc;

static ExitOnError ExitOnErr;
static std::unique_ptr<LLJIT> s_jit;

void orcjit_init() {
    auto J = ExitOnErr(LLJITBuilder().create()); // std::unique_ptr<LLJIT>
    auto D = J->getDataLayout();
    auto G = ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(D.getGlobalPrefix()));
    J->getMainJITDylib().setGenerator(G);
    s_jit = std::move(J);
}

static void emit_foobar(Function& F, IRBuilder<>& B)
{
    auto one = B.getInt32(1);
    auto x = F.arg_begin() + 0; // F.getArg(0);
    auto add = B.CreateAdd(one, x);
    B.CreateRet(add);
}

ThreadSafeModule orcjit_make_module()
{
    auto Context = llvm::make_unique<LLVMContext>();
    auto M = llvm::make_unique<Module>("test", *Context);

    // Create the add1 function entry and insert this entry into module M.  The
    // function will have a return type of "int" and take an argument of "int".

    std::vector<Type*> argTypes;
    argTypes.push_back(Type::getInt32Ty(*Context));

    Type* retType = Type::getInt32Ty(*Context);

    Function *Add1F =
        Function::Create(
            FunctionType::get(retType, argTypes, false),
            Function::ExternalLinkage,
            "add1",
            M.get());

/*
    Function *Add1F =
        Function::Create(
            FunctionType::get(Type::getInt32Ty(*Context), {Type::getInt32Ty(*Context)}, false),
            Function::ExternalLinkage,
            "add1",
            M.get());
*/
    // Add a basic block to the function. As before, it automatically inserts
    // because of the last argument.
    BasicBlock *BB = BasicBlock::Create(*Context, "EntryBlock", Add1F);

    // Create a basic block builder with default parameters.  The builder will
    // automatically append instructions to the basic block `BB'.
    IRBuilder<> builder(BB);

    emit_foobar(*Add1F, builder);
/*
    // Get pointers to the constant `1'.
    Value *One = builder.getInt32(1);

    // Get pointers to the integer argument of the add1 function...
    assert(Add1F->arg_begin() != Add1F->arg_end()); // Make sure there's an arg
    Argument *ArgX = &*Add1F->arg_begin();          // Get the arg
    ArgX->setName("AnArg"); // Give it a nice symbolic name for fun.

    // Create the add instruction, inserting it into the end of BB.
    Value *Add = builder.CreateAdd(One, ArgX);

    // Create the return instruction and add it to the basic block
    builder.CreateRet(Add);
*/
    return ThreadSafeModule(std::move(M), std::move(Context));
}

void testflight() {
    auto Context = llvm::make_unique<LLVMContext>();
    auto M = llvm::make_unique<Module>("test", *Context);

    std::vector<Type*> argTypes;
    argTypes.push_back(Type::getInt64PtrTy(*Context));

    Type* retType = Type::getInt64Ty(*Context);

    Function* get2nd =
        Function::Create(
            FunctionType::get(retType, argTypes, false),
            Function::ExternalLinkage,
            "get2nd",
            M.get());

    BasicBlock *BB = BasicBlock::Create(*Context, "EntryBlock", get2nd);
    IRBuilder<> builder(BB);
    auto base = (*get2nd).arg_begin() + 0;

    Value* index = builder.getInt64(2);
    Value* ea = builder.CreateGEP(base, index);
    Value* val1 = builder.CreateLoad(Type::getInt64Ty(*Context), ea);

    Value* val2 = builder.CreateExtractElement(base, 2);

    builder.CreateRet(val2);
    M.get()->print(outs(), nullptr);
}

void orcjit_compile() {
    testflight();
    return;
    auto M = orcjit_make_module();
    ExitOnErr(s_jit->addIRModule(std::move(M)));
    s_jit->getMainJITDylib().dump(llvm::outs());

    puts("Look symbol");
    auto Add1Sym = ExitOnErr(s_jit->lookup("add1"));

    puts("Get address");
    int (*Add1)(int) = (int (*)(int))Add1Sym.getAddress();

    puts("Call");
    int Result = Add1(42);
    outs() << "add1(42) = " << Result << "\n";
}

/*
(current-environment (system-environment)) (native-compile)
*/

class code_generator_t {
    ExitOnError ExitOnErr;
    static std::unique_ptr<LLJIT> s_main;
public:
    code_generator_t() {

    }
    void assemble(scm_closure_t closure) {

    }
};

#endif
