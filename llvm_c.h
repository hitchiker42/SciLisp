#ifndef _LLVM_C_
#define _LLVM_C_
#include "common.h"
#include "cons.h"
#include <llvm-c/Core.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Initialization.h>
//types are named Lisp##Typename
//llvm structures are named SL_##llvm struct name
//constant values are named Lisp##(upcase valuename)
LLVMTypeRef LispDouble;
LLVMTypeRef LispLong;
LLVMTypeRef LispInt;
LLVMTypeRef LispShort;
LLVMTypeRef LispChar;
LLVMTypeRef LispWChar;
LLVMTypeRef LispData;
LLVMTypeRef LispSexp;
LLVMTypeRef LispCons;
LLVMTypeRef LispArray;
LLVMTypeRef LispVoid;
LLVMValueRef LLVM_Codegen_Special(sexp expr,env cur_env,
                                                LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_if(sexp expr,env cur_env,
                                    LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_progn(sexp expr,env cur_env,
                                    LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_do(sexp expr,env cur_env,
                                    LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_prog1(sexp expr,env cur_env,
                                    LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_while(sexp expr,env cur_env,
                                    LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_defun(sexp expr,env cur_env,
                                    LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_def(sexp expr,env cur_env,
                                     LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_lambda(sexp expr,env cur_env,
                                    LLVMBuilderRef builder);
LLVMValueRef LLVM_Call_Function(sexp expr,env cur_env,
                                       LLVMBuilderRef builder);

LLVMValueRef LLVM_Call_Builtin(sexp expr,env cur_env,
                                     LLVMBuilderRef builder);
LLVMValueRef LLVM_Call_Lambda(sexp expr,env cur_env,
                                     LLVMBuilderRef builder);
LLVMValueRef* get_args(sexp arglist,function fun,env cur_env,
                              LLVMBuilderRef builder);
static LLVMTypeRef getLLVMType(sexp obj){
  switch(obj.tag){
    case _double:
      return LispDouble;
    case _long:
      return LispLong;
    case _char:
      return LispWChar;
    case _cons:
      return LispCons;//this won't work, cons is actually a pointer
    case _array:
      return LispArray;
    default:
      return LispVoid;
  }
}
#endif
