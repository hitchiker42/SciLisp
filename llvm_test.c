#include "llvm_c.h"
LLVMValueRef GenerateSumFunction(){
  //use llvm to codegen (defun sum (x) (reduce x +))
  //make sure llvm is initalized before calling
  LLVMValueRef sum = LLVMAddFunction(SL_Module,"sum",LispFxnTypes[1]);
  LLVMBasicBlockRef entry_block=LLVMAppendBasicBlock(sum,"");
  LLVMPositionBuilderAtEnd(SL_Builder,entry_block);
  LLVMValueRef red=LLVMGetNamedFunction(SL_Module,"reduce");
  LLVMValueRef rparams[4];
  LLVMValueRef sum_arg=LLVMGetParam(sum,0);
  HERE();
  LLVMValueRef add_temp=LLVMGetNamedGlobal(SL_Module,"lisp_addcall_ptr");
  if(!add_temp){exit(1);}
  HERE();
  rparams[0]=LLVMBuildExtractValue(SL_Builder,sum_arg,0,"");
  rparams[1]=LLVMBuildExtractValue(SL_Builder,sum_arg,1,"");
  HERE();
  rparams[2]=LLVMConstInt(LispLong,5,0);
  HERE();
  rparams[3]=LLVMConstBitCast(add_temp,LispLong);
  HERE();  
  LLVMValueRef retval=LLVMBuildCall(SL_Builder,red,rparams,4,"");
  HERE();
  LLVMBuildRet(SL_Builder,retval);               
  HERE();
  LLVMRunFunctionPassManager(SL_Pass,sum);
  HERE();
  return sum;
}
LLVMValueRef GenerateFMAFunction(){
  //(fma a b c) = (+ a (* b c))
  LLVMValueRef fma = LLVMAddFunction(SL_Module,"fma",LispFxnTypes[3]);
  LLVMBasicBlockRef entry_block=LLVMAppendBasicBlock(fma,"");
  HERE();
  LLVMPositionBuilderAtEnd(SL_Builder,entry_block);
  LLVMValueRef args[3]={
    LLVMBuildBitCast(SL_Builder,LLVMGetParam(fma,0),LLVMSexp,""),
    LLVMBuildBitCast(SL_Builder,LLVMGetParam(fma,1),LLVMSexp,""),
    LLVMBuildBitCast(SL_Builder,LLVMGetParam(fma,2),LLVMSexp,"")};
  HERE();
  LLVMValueRef mul_args[4]={
    LLVMBuildExtractValue(SL_Builder,args[0],0,""),
    LLVMBuildExtractValue(SL_Builder,args[0],1,""),
    LLVMBuildExtractValue(SL_Builder,args[1],0,""),
    LLVMBuildExtractValue(SL_Builder,args[1],1,"")};
  HERE();
  LLVMValueRef mul_fn=LLVMGetNamedFunction(SL_Module,"lisp_mul");
  LLVMValueRef mul_call=LLVMBuildCall(SL_Builder,mul_fn,mul_args,4,"");
  HERE();
  LLVMValueRef add_args[4]={
    LLVMBuildExtractValue(SL_Builder,args[2],0,""),
    LLVMBuildExtractValue(SL_Builder,args[2],1,""),
    LLVMBuildExtractValue(SL_Builder,mul_call,0,""),
    LLVMBuildExtractValue(SL_Builder,mul_call,1,"")};
  HERE();
  LLVMValueRef add_fn=LLVMGetNamedFunction(SL_Module,"lisp_add");
  LLVMValueRef retval=LLVMBuildCall(SL_Builder,add_fn,add_args,4,"");
  HERE();
  LLVMBuildRet(SL_Builder,retval);
  HERE();
  LLVMRunFunctionPassManager(SL_Pass,fma);
  HERE();
  return fma;
}
int main(){
   initialize_llvm(1);
   LLVMValueRef lispfma_defn=GenerateFMAFunction();
   //   LLVMValueRef lispsum_defn=GenerateSumFunction();
   LLVMValueRef hello_worldworld_fn=LLVMGetNamedFunction(SL_Module,"hello_world");
   LLVMValueRef* hello_world_2=xmalloc(sizeof(LLVMValueRef));
   LLVM_IF(LLVMFindFunction(SL_Engine,"hello_world",hello_world_2)){
     PRINT_FMT("%#0x",hello_world_2);
     LLVMRunFunction(SL_Engine,*hello_world_2,0,0);
     void(*f)()=LLVMGetPointerToGlobal(SL_Engine,*hello_world_2);
     f();
   }
   LLVMValueRef* lispadd_fn2 = xmalloc(sizeof(LLVMValueRef));
   LLVM_IF(LLVMFindFunction(SL_Engine,"lisp_add",lispadd_fn2)){
     //LLVMVerifyFunction(*lispadd_fn2,LLVMPrintMessageAction);
    sexp(*lispadd)(sexp,sexp)=LLVMRecompileAndRelinkFunction(SL_Engine,*lispadd_fn2);
    if(lispadd){
      PRINT_MSG("Evaluating (+ 2 2) after recompiling lisp_add");
      PRINT_MSG(print(lispadd(long_sexp(2),long_sexp(2))));
    } else {return 1;}
  } else {return 1;}
  LLVMValueRef lispadd_fn=LLVMGetNamedFunction(SL_Module,"lisp_add");
  sexp(*lispadd)(sexp,sexp)=LLVMGetPointerToGlobal(SL_Engine,lispadd_fn);
  if(lispadd){
    PRINT_MSG("evaluating (+ 2 2)");
    PRINT_MSG(print(lispadd(long_sexp(2),long_sexp(2))));
  } else {return 1;}
  LLVMValueRef lisplog_fn=LLVMGetNamedFunction(SL_Module,"lisp_log");
  LLVMValueRef lispiota_fn=LLVMGetNamedFunction(SL_Module,"lisp_iota");
  HERE();
  LLVMValueRef lispfma_fn=LLVMGetNamedFunction(SL_Module,"fma");
  HERE();
  sexp(*lisplog)(sexp)=LLVMGetPointerToGlobal(SL_Engine,lisplog_fn);
  sexp(*lispfma)(sexp,sexp,sexp)=LLVMGetPointerToGlobal(SL_Engine,lispfma_fn);
  sexp(*lispiota)(sexp,sexp,sexp,sexp)=LLVMRecompileAndRelinkFunction(SL_Engine,lispiota_fn);
  HERE();
  if(lisplog){
    PRINT_MSG("evaluating (log 2.781828)");
    PRINT_MSG(print(lisplog(double_sexp(2.781828))));
  } else {return 1;}
  if(lispfma){
    PRINT_MSG("evaluating (fma 10 100 2 )");
    PRINT_MSG(print 
              (lispfma(long_sexp(10),long_sexp(100),long_sexp(2))));
  } else {return 1;}
  if(lispiota){
    PRINT_MSG("evaluating (iota 1 10 1)");
    PRINT_MSG("or not, because it causes a segfault");
    // PRINT_MSG(print(lispiota(long_sexp(1),long_sexp(10),long_sexp(1),NIL)));
  } else {return 1;}
  return 0;
}



