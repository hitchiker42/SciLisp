/* (defun llvm-fix-case (str) 
   (let ((case-fold-search nil))
   (downcase (replace-regexp-in-string "\\([A-Z][a-z]+\\)" "_\\&" str t))))
   (defun llvm-insert-define (str)
   (interactive "s")
   (insert 
     (concat
     "#define " (llvm-fix-case str) " " str "\n")))
*/
#define llvm_append_bb LLVMAppendBasicBlockInContext
#define llvm_insert_bb LLVMinsertBasicBlockInContext
#define llvm_build_cond_br LLVMBuildCondBr
#define llvm_build_br LLVMBuildBr
#define llvm_position_builder_at_end LVMPositionBuilderAtEnd
#define llvm_position_builder_before LLVMPositionBuilderBefore
#define llvm_position_at_end_of_bb(env,bb)    \
  LLVMPositionBuilderAtEnd(env->builder,bb);   \
  env->current_block=bb
#define llvm_position_before_bb(env,bb)         \
  LLVMPositionBuilderBefore(env->builder,bb)    \
  env->current_bb=bb
#define llvm_add_incoming LLVMAddIncoming
#define llvm_build_phi LLVMBuildPhi
#define llvm_build_extract_value LLVMBuildExtractValue
#define llvm_build_int_cmp LLVMBuildICmp
#define llvm_int_eq LLVMIntEQ
#define llvm_int_ne LLVMIntNE
#define llvm_move_basic_block_after LLVMMoveBasicBlockAfter
#define llvm_const_null LLVMConstNull
#define llvm_const_all_ones LLVMConstAllOnes
#define llvm_const_int LLVMConstInt
#define llvm_const_int64(int) LLVMConstInt(llvm_uint64_t,int,0)
#define llvm_const_real LLVMConstReal
#define llvm_const_real64(double) LLVMConstReal(llvm_real64_t,double)
#define llvm_const_string LLVMConstString
#define llvm_const_struct LLVMConstStruct
#define llvm_const_array LLVMConstArray
#define llvm_function_type LLVMFunctionType
