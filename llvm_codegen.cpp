#include "llvm.h"
using namespace llvm;
static LLVMContext &SL_LLVMContext = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static Module *SL_Module;
static ExecutionEngine *SL_ExecutionEngine;
static FunctionPassManager *FPM;
static Value *toLLVMValue(sexp obj);
static Type *getLLVMType(sexp obj);
