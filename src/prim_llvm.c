#include "llvm.h"
LLVMModuleRef GlobalModule;
LLVMBuilderRef GlobalBuilder;
static void* LLVMCompileFunction(LLVMExecutionEngineRef EE,LLVMValueRef Fn){
  //if Fn is not compiled this will compile it, then again if it is compiled it
  //will also compile it so...
  return LLVMGetPointerToGlobal(EE,Fn);
}
LLVMModuleRef mkLLVMPrims(){
  GlobalModule=LLVMModuleCreateWithName("Global");
  LLVMTypeRef LispLong = LLVMInt64Type();
  LLVMTypeRef LispDouble = LLVMDoubleType();
  LLVMTypeRef UnionData = LLVMDoubleType();
  LLVMExecutionEngineRef MainJit;
  char* error;
  LLVMBool LLVMCreateJITCompilerForModule(MainJit,MainModule,0,&error);
  
);
}
