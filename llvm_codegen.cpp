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
static Value* Codegen_special(scoped_sexp scoped_obj);
static Value* Codegen_def(scoped_sexp scoped_obj);
static Value* Codegen_defun(scoped_sexp scoped_obj);
static Value* Codegen_if(scoped_sexp scoped_obj);
static Value* Codegen_while(scoped_sexp scoped_obj);
static Value* Codegen_lambda(scoped_sexp scoped_obj);
static Value* Codegen_let(scoped_sexp scoped_obj);
static Value* Codegen_progn(scoped_sexp scoped_obj);
static Value* Codegen_prog1(scoped_sexp scoped_obj);
static Value* Codegen_do(scoped_sexp scoped_obj); 
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
  LLVMValueRef LLVM_Codegen(scoped_sexp scoped_obj);
}
Value* Codegen(scoped_sexp scoped_obj);
LLVMValueRef LLVM_Codegen(scoped_sexp scoped_obj){
  return wrap(Codegen(obj));
}
/* Error handling not yet added, but to prevend horrible crashes
   any conditional has a default case which goes to FAILSAFE*/
Value* Codegen(scoped_sexp scoped_obj){
  sexp obj=scoped_obj.sexp;
  env scope=*scoped_obj.env;
  switch(obj.tag){
    case _long:
      return ConstantInt::get(LispLong,obj.val.int64);
    case _double:
      return ConstantFP::get(LispDouble,obj.val.real64);
    case _char:
      return ConstantInt::get(LispWChar,obj.val.utf8_char);
    case _array:
      switch(obj.meta){
        case _double_array:
          return ConstantDataVector(SL_Context,SexpToAref(double,obj));
        case _long_array:
          return ConstantDataVector(SL_Context,SexpToAref(long,obj));
        default:
          goto FAILSAFE;
      }
    case _var:
      symref tmpSym=getSym(env,obj.val.var->name);
      if(symref){
        return Codegen((scoped_sexp){tmpSym->val,&env});
      } else {
        goto FAILSAFE;
      }
    case _fun:
    default:
      goto FAILSAFE;
  }
 FAILSAFE:
  return ConstantPointerNull::get((PointerType*)LispArray);
 ERROR:
}
