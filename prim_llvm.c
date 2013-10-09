#include "llvm.h"
LLVMModuleRef GlobalModule;
LLVMBuilderRef GlobalBuilder;
LLVMModuleRef mkLLVMPrims(){
  GlobalModule=LLVMModuleCreateWithName("Global");
  LLVMTypeRef LispLong = LLVMInt64Type();
  LLVMTypeRef LispDouble = LLVMDoubleType();
  LLVMTypeRef UnionData = LLVMDoubleType();
}
