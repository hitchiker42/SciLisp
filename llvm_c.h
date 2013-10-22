/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef _LLVM_C_
#define _LLVM_C_
#include "common.h"
#include "cons.h"
#include "prim.h"
//include fucking everything, because why not
#include <llvm-c/Core.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Initialization.h>
#include <llvm-c/Target.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Vectorize.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include <llvm-c/Object.h>
#include <llvm-c/TargetMachine.h>
//llvm returns 0 on success, so use this to keep my brain straight
#define LLVM_IF(cond) if (!cond)
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
LLVMTypeRef LLVMSexp;
LLVMModuleRef SL_Module;
LLVMContextRef SL_Context;
LLVMBuilderRef SL_Builder;
LLVMPassRegistryRef SL_Registry;
LLVMPassManagerRef SL_Pass;
LLVMPassManagerRef SL_Opt;
LLVMValueRef LispNIL;
LLVMValueRef LispTRUE;
LLVMValueRef LispFALSE;
LLVMTypeRef LispArgs[8];
LLVMTypeRef LispFxnTypes[9];
LLVMExecutionEngineRef SL_Engine;//can be jit, mcjit, or an interpreter
void initialize_llvm(int engine);//1=jit,2=mcjit,3=interpreter
void dump_mod();//dump SL_Module to stderr
LLVMModuleRef* Parse_Prim_bc(const char* name);//create a module from prim.bc
//Just a quick note
/* for llvm(at least in c) every basic block is attached to a function 
 * so for top level evaluation we need to create a dummy function to call */
LLVMValueRef LLVM_Codegen(sexp expr,env *cur_env,LLVMBuilderRef builder);
sexp LLVMEval(sexp expr,env *cur_env);
LLVMValueRef LLVM_Codegen_Special(sexp expr,env *cur_env,
                                  LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_if(sexp expr,env *cur_env,
                             LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_progn(sexp expr,env *cur_env,
                                LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_do(sexp expr,env *cur_env,
                             LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_prog1(sexp expr,env *cur_env,
                                LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_while(sexp expr,env *cur_env,
                                LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_defun(sexp expr,env *cur_env,
                                LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_def(sexp expr,env *cur_env,
                              LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen_lambda(sexp expr,env *cur_env,
                                 LLVMBuilderRef builder);
LLVMValueRef LLVM_Call_Function(sexp expr,env *cur_env,
                                LLVMBuilderRef builder);
LLVMValueRef LLVM_Call_Builtin(sexp expr,env *cur_env,
                               LLVMBuilderRef builder);
LLVMValueRef LLVM_Call_Lambda(sexp expr,env *cur_env,
                              LLVMBuilderRef builder);
LLVMValueRef* get_args(sexp arglist,function fun,env *cur_env,
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
//note the lack of typedef
union hack{
  sexp as_sexp;
  long as_longs[2];
};
#endif
/*
LLVM Code for the various types
%struct.env = type { %struct.env*, %union.symbol_ref, i8 }
%union.symbol_ref = type { %struct.global_symbol* }
%struct.global_symbol = type { i8*, %struct.sexp, %struct.UT_hash_handle }
%struct.sexp = type { i8, i16, [2 x i8], %union.data }
%union.data = type { double }
%struct.UT_hash_handle = type { %struct.UT_hash_table*, i8*, i8*, %struct.UT_hash_handle*, %struct.UT_hash_handle*, i8*, i32, i32 }
%struct.UT_hash_table = type { %struct.UT_hash_bucket*, i32, i32, i32, %struct.UT_hash_handle*, i64, i32, i32, i32, i32, i32 }
%struct.UT_hash_bucket = type { %struct.UT_hash_handle*, i32, i32 }
%struct.global_env = type { %struct.env*, %struct.global_symbol* }
%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }
%struct.__jmp_buf_tag = type { [8 x i64], i32, %struct.__sigset_t }
%struct.__sigset_t = type { [16 x i64] }
%struct.cons = type { %struct.sexp, %struct.sexp }
%struct.__va_list_tag = type { i32, i32, i8*, i8* }
%struct.fxn_proto = type { i8*, i8*, i16, i16, %union.funcall }
%union.funcall = type { {}* }
%struct.function = type { i16, i16, %union.anon, i8 }
%union.anon = type { %struct.fxn_proto* }
%struct.symbol = type { i8*, %struct.sexp }
%struct.lambda = type { %struct.local_env*, i16, i16, %struct.sexp }
%struct.local_env = type { %struct.env*, %struct.local_symbol* }
%struct.local_symbol = type { i8*, %struct.sexp, %struct.local_symbol* }
*/
