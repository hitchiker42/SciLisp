#ifndef _LLVM_C_
#define _LLVM_C_
#include "common.h"
#include "cons.h"
#include <llvm-c/Core.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/ExecutionEngine.h>
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
#endif
