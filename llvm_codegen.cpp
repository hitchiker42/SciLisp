#include "llvm.h"
using namespace llvm;
static LLVMContext &SL_Context = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static Module *SL_Module;
static ExecutionEngine *SL_ExecutionEngine;
static FunctionPassManager *FPM;
static Value *toLLVMValue(sexp obj);
static Type *getLLVMType(sexp obj);
static Type* LispDouble;
static Type* LispLong;
static Type* LispChar;
static Type* LispData;
static Type* LispSexp;
static int initialize_types(){
  LispDouble=Type::getDoubleTy(SL_Context);
  LispLong=Type::getInt64Ty(SL_Context);
  //vaguely non portable but really who doesn't use libc
  LispChar=Type::getInt32Ty(SL_Context);
  LispSexp=StructType::create(SL_Context,"sexp");
  LispData=StructType::create(SL_Context,"data");
}
Value* Codegen(sexp obj);
Value* Codegen(sexp obj){
  switch(obj.tag){
    case _long:
      return GetLongVal(obj.val.int64);
    case _double:
      return GetDoubleVal(obj.val.real64);
  }
}
