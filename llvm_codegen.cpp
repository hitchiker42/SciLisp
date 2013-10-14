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
static Type* LispInt;
static Type* LispShort;
static Type* LispChar;
static Type* LispWChar;
static Type* LispData;
static Type* LispSexp;
static Type* LispCons;
static Type* LispArray;
static Type** LispArgs;
static FunctionType** LispFxnTypes;
//perhaps use AvailableExternallyLinkage for prims
//I don't actually need this
static llvm::Function* defun_prim(char* name,int maxargs){
  return  Function::Create(LispFxnTypes[maxargs],
                           GlobalValue::ExternalLinkage,Twine(name));
}
//Need to deal with scoping issues back in C
static Constant* Module_getOrInsertFunction(Module* mod,char* name,Type* type){
  return mod->getOrInsertFunction(name,(FunctionType*)type);
}
extern "C"{
  void initialize_llvm();
}
void initialize_llvm(){
  LispDouble=Type::getDoubleTy(SL_Context);
  LispLong=Type::getInt64Ty(SL_Context);
  LispInt=Type::getInt32Ty(SL_Context);
  LispShort=Type::getInt16Ty(SL_Context);
  LispChar=Type::getInt8Ty(SL_Context);
  LispWChar=LispInt;
  LispData=StructType::create("data",LispDouble,NULL);
  Type *sexpTypes[4]={LispData,LispInt,LispShort,LispShort};
  ArrayRef<Type*> Aref_sexpTypes=ArrayRef<Type*>(sexpTypes,4);//?the hell is this
  LispSexp=StructType::create(Aref_sexpTypes,"sexp",true);
  LispCons=StructType::create("cons",LispSexp,LispSexp,NULL);
  static Type* StupidCpp_hack[8]={LispSexp,LispSexp,LispSexp,LispSexp,
                                  LispSexp,LispSexp,LispSexp,LispSexp};
  LispArgs=StupidCpp_hack;
  static FunctionType* StupidCpp_hack2[8]={voidFunction(),mkFunType(1),mkFunType(2),
                                   mkFunType(3),mkFunType(4),mkFunType(5),
                                   mkFunType(6),mkFunType(7)};
  LispFxnTypes=StupidCpp_hack2;
  LispArray=PointerType::getUnqual(LispData);
  int i,imax=sizeof(lisp_prims)/sizeof(name_args_pair);
  for(i=0;i<imax;i++){
    Module_getOrInsertFunction(SL_Module,lisp_prims[i].name,
                               LispFxnTypes[lisp_prims[i].args]);
  }
}
extern "C"{
  LLVMValueRef LLVM_Codegen(sexp obj);
}
Value* Codegen(sexp obj);
LLVMValueRef LLVM_Codegen(sexp obj){
  return wrap(Codegen(obj));
}
Value* Codegen(sexp obj){
  switch(obj.tag){
    case _long:
      return GetLongVal(obj.val.int64);
    case _double:
      return GetDoubleVal(obj.val.real64);
    case _fun:
    default:
      return ConstantPointerNull::get((PointerType*)LispArray);
  }
}
