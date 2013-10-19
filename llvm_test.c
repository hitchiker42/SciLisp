#include "llvm_c.h"
int main(){
   initialize_llvm(1);
   //not sure if there's a point to this
   LLVMPassManagerRef SL_Pass = LLVMCreatePassManager();
   LLVMAddTargetData(LLVMGetExecutionEngineTargetData(SL_Engine),SL_Pass);
   LLVMAddConstantPropagationPass(SL_Pass);
   LLVMAddInstructionCombiningPass(SL_Pass);
   LLVMAddPromoteMemoryToRegisterPass(SL_Pass);
   LLVMRunPassManager(SL_Pass,SL_Module);
   LLVMValueRef hello_world_fn=LLVMGetNamedFunction(SL_Module,"hello_world");
   LLVMValueRef* hello_world_2=xmalloc(sizeof(LLVMValueRef));
   if(!LLVMFindFunction(SL_Engine,"hello_world",hello_world_2)){
     PRINT_FMT("%#0x",hello_world_2);
     LLVMRunFunction(SL_Engine,*hello_world_2,0,0);
     void(*f)()=LLVMGetPointerToGlobal(SL_Engine,*hello_world_2);
     f();
   }
   LLVMValueRef* lispadd_fn2 = xmalloc(sizeof(LLVMValueRef));
   if(!LLVMFindFunction(SL_Engine,"lisp_add",lispadd_fn2)){
     //LLVMVerifyFunction(*lispadd_fn2,LLVMPrintMessageAction);
    sexp(*lispadd)(sexp,sexp)=LLVMRecompileAndRelinkFunction(SL_Engine,*lispadd_fn2);
    if(lispadd){
      PRINT_MSG("Evaluating (+ 2 2) after recompiling lisp_add");
      PRINT_MSG(print(lispadd(long_sexp(2),long_sexp(2))));
    } else {
      return 1;
    }
  } else {
    return 1;
  }
  LLVMValueRef lispadd_fn=LLVMGetNamedFunction(SL_Module,"lisp_add");
  sexp(*lispadd)(sexp,sexp)=LLVMGetPointerToGlobal(SL_Engine,lispadd_fn);
  if(lispadd){
    PRINT_MSG("evaluating (+ 2 2)");
    PRINT_MSG(print(lispadd(long_sexp(2),long_sexp(2))));
  } else {
    return 1;
  }
  LLVMValueRef lisplog_fn=LLVMGetNamedFunction(SL_Module,"lisp_log");
  LLVMValueRef lispiota_fn=LLVMGetNamedFunction(SL_Module,"lisp_iota");
  sexp(*lisplog)(sexp)=LLVMGetPointerToGlobal(SL_Engine,lisplog_fn);
  sexp(*lispiota)(sexp,sexp,sexp,sexp)=LLVMRecompileAndRelinkFunction(SL_Engine,lispiota_fn);
  if(lisplog){
    PRINT_MSG("evaluating (log 2.781828)");
    PRINT_MSG(print(lisplog(double_sexp(2.781828))));
  } else {
    return 1;
  }
  if(lispiota){
    PRINT_MSG("evaluating (iota 1 10 1)");
    PRINT_MSG(print(lispiota(long_sexp(1),long_sexp(10),long_sexp(1),NIL)));
  } else {
    return 1;
  }
  return 0;
}
