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
#define llvm_const_null LLVMConstNull
#define llvm_move_basic_block_after LLVMMoveBasicBlockAfter
