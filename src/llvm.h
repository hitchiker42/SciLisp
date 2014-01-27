#ifndef SCILISP_LLVM_H
#define SCILISP_LLVM_H
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
//I don't like the llvm naming conventions
typedef LLVMValueRef llvm_val;
typedef LLVMBasicBlockRef llvm_bb;
typedef LLVMContextRef llvm_context;
typedef LLVMTypeRef llvm_type;
typedef LLVMBuilderRef llvm_builder;
typedef LLVMModuleRef llvm_module;
//global data
static llvm_type llvm_sexp;
static llvm_type llvm_sexp_data;
static llvm_type llvm_symbol;
static llvm_type llvm_cons;
static llvm_type llvm_uint8_t;
static llvm_type llvm_uint16_t;
static llvm_type llvm_uint32_t;
static llvm_type llvm_uint64_t;
static llvm_type llvm_real32_t;
static llvm_type llvm_real64_t;
static llvm_value llvm_nil;
//lexical environment for llvm,lexical bindings can be 
//resolved at compile time, though I still need to
//keep the environment around I guess
struct lex_binding{
  symbol *sym;
  llvm_value val;
};
typedef struct llvm_environment *llvm_env_ptr
struct llvm_environment {
  llvm_context context;//llvm context, generally unimportant
  llvm_module module;//llvm module, generally unimportant
  llvm_builder builder;//llvm instruction builder, used to actually build ir
  llvm_bb current_block;//current basic block we're inserting instructions into
  llvm_value current_fun;
  struct lex_binding *lex_bindings;  
};
static thread_local llvm_env_ptr current_llvm_env;
//called once to initialize types
static llvm_init_types(){
  //make stuff thread safe
  llvm_start_multithreaded();
  llvm_context context=llvm_get_global_context();
  llvm_uint64_t = llvm_int64_type();
  llvm_uint32_t = llvm_int32_type();
  llvm_real64_t = llvm_real64_type();
  llvm_sexp_data=llvm_create_named_struct(context,"data",&llvm_uint64_t,1);
  llvm_type sexp_elements[2]={llvm_sexp_data,llvm_uint32_t};
  llvm_sexp=llvm_create_named_struct(context,"sexp",sexp_elements,2);
}  
//called once per thread before starting compilation
static llvm_init(){
  
  current_llvm_env=xmalloc_atomic(sizeof(llvm_environment));
  llvm_env_ptr env=current_llvm_env;
  env->context=llvm_context_create();
  env->module=llvm_create_module("name",env->context);
  env->builder=llvm_create_builder(env->context);
}
static llvm_cleanup(){
  llvm_context_dispose(env->context);
  llvm_dispose_module(env->module);
  llvm_dispose_builder(env->builder);
#endif
