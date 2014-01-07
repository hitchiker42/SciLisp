#ifndef LLVM_EXTERNS_H
#define LLVM_EXTERNS_H
#ifdef USE_LLVM
sexp LLVMGenFunction(sexp lambda_expr,env* cur_env);
sexp LLVMEval(sexp expr,env *cur_env);
void initialize_llvm(int engine);//1=jit,2=mcjit,3=interpreter
#define initialize_llvm_default() initialize_llvm(1)
#endif
#endif
