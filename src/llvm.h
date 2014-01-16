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
static llvm_type llvm_uint8_t;
static llvm_type llvm_uint16_t;
static llvm_type llvm_uint32_t;
static llvm_type llvm_uint64_t;
static llvm_type llvm_real32_t;
static llvm_type llvm_real64_t;
//thread local data
typedef struct llvm_environment *llvm_env_ptr
struct llvm_environment {
  llvm_context context;//llvm context, generally unimportant
  llvm_module module;//llvm module, generally unimportant
  llvm_builder builder;//llvm instruction builder, used to actually build ir
  llvm_bb current_block;//current basic block we're inserting instructions into
  llvm_value current_fun;
};
#endif
