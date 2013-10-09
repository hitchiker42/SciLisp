#include "llvm.h"
//possibly more
static LLVMModuleRef CurModule;
static LLVMBuilderRef Builder;

LLVMTypeKind toLLVMType(_tag type){
  switch (type){
    case _double: return LLVMDoubleTypeKind;
    case _char:
    case _long: return LLVMIntegerTypeKind;
    case _fun: return LLVMFunctionTypeKind;
    case _array: return LLVMFunctionArrayKind;
    default: return LLVMVoidTypeKind;
  }
}
void Codegen(t){
  Builder=LLVMCreateBuilder();
  LLVMTypeRef LispLong = LLVInt64Type();
  LLVMTypeRef LispDouble = LLVMDoubleType();
