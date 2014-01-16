#include "common.h"
#include "llvm.h"
#define llvm_is_true(val,env)                   \
  llvm_build_int_cmp(env->builder,llvm_int_eq,          \
    llvm_build_extract_value(env->builder,val,0,""),    \
                     llvm_const_null(llvm_uint64_t))
  
llvm_value llvm_build(sexp expr,llvm_env_ptr env);//equivlant to eval(ie, main entry point)
llvm_value llvm_build_atomic(sexp expr,llvm_env_ptr env);
//(if cond then &rest else)
llvm_value llvm_build_if(sexp args,llvm_env_ptr env){
  if(!CONSP(args) || !(CONSP(XCDR(args)))){
    return error_sexp("too few arguments passed to if");
  }
  sexp cond=XCAR(args);
  llvm_bb cond_bb=llvm_append_bb(env->current_fun,env->context,"Cond");
  sexp then_br=XCADR(args);
  llvm_bb then_bb=llvm_append_bb(env->current_fun,env->context,"Then");
  sexp else_br=XCDDR(args);
  llvm_bb else_bb=llvm_append_bb(env->current_fun,env->context,"Else");
  llvm_bb end_bb=llvm_append_bb(env->current_fun,env->context,"End");
  llvm_bb phi_blocks[2];
  llvm_position_at_end_of_bb(env,cond_bb);
  llvm_value cond =llvm_build(cond,env);
  llvm_build_cond_br(env->builder,cond,then_bb,else_bb);
  llvm_position_at_end_of_bb(env,then_bb);
  llvm_value then_val=llvm_build(then_br,env);
  llvm_build_br(env->builder,end);
  phi_blocks[0]=env->current_bb;
  llvm_position_at_end_of_bb(env,else_bb);
  llvm_value else_val=llvm_build(else_br,env);
  llvm_build_br(env->builder,end);
  phi_blocks[1]=env->current_bb;
  llvm_value phi_vals[2]={then_val,else_val};
  llvm_position_at_end_of_bb(env,end_bb);
  llvm_value retval=llvm_build_phi(env->builder,llvm_sexp,"Result");
  llvm_add_incoming(retval,phi_vals,phi_blocks,2);
  return retval;
}
llvm_value llvm_build_and(sexp exprs,llvm_env_ptr env){
  //if exprs is nil or a single expression we don't need to branch
  if(!CONSP(exprs)){
    return llvm_nil;
  } else if(!CONSP(XCDR(exprs))){
    return llvm_build(exprs,env);
  } else {
    llvm_bb end_block=llvm_append_bb(env->current_fun,env->context,"End");
    int len=cons_len(exprs);
    int i;
    llvm_value *phi_vals=xmalloc(sizeof(llvm_value)*len);
    llvm_bb *phi_blocks=xmalloc(sizeof(llvm_value)*len);
    llvm_bb cur_block,next_block;
    llvm_value cur_value;
    cur_block=llvm_append_bb(env->current_fun,env->context,"");
    for(i=0;i<len-1;i++){
      next_block=llvm_append_bb(env->current_fun,env->context,"");
      llvm_position_at_end_of_bb(env,cur_block);
      cur_value=llvm_build(XCAR(exprs),env);
      exprs=XCDR(exprs);
      cur_value_is_true=llvm_is_true(cur_val,env);      
      llvm_build_cord_br(env->builder,cur_value_is_true,next_block,end_block);
      phi_vals[i]=llvm_nil;
      phi_blocks=cur_block;
      cur_block=next_block;
    }
    llvm_position_before_bb(env,cur_block);
    cur_value=llvm_build(XCAR(exprs),env);
    exprs=XCDR(exprs);
    cur_value_is_true=llvm_is_true(cur_val,env);      
    llvm_build_cond_br(env->builder,cur_value_is_true,next_block,end_block);
    phi_vals[i]=cur_value;
    phi_blocks=cur_block;
    llvm_move_basic_block_after(end_block,cur_block);
    llvm_position_at_end_of_bb(env,end_block);
    llvm_value retval=llvm_build_phi(env->builder,llvm_sexp,"Result");
    llvm_add_incoming(retval,phi_vals,phi_blocks,len);
    return retval;
  }     
}
llvm_val llvm_build_while(sexp args,llvm_env_ptr env){
  if(!CONSP(args)){
    return error_sexp("to few arguments passed to while");
  sexp cond=XCAR(args);
  sexp body=XCDR(args);
  llvm_bb cond_bb=llvm_append_bb(env->current_fun,env->context,"Cond");
  llvm_bb body_bb=llvm_append_bb(env->current_fun,env->context,"Body");
  llvm_bb end_bb=llvm_append_bb(env->current_fun,env->context,"End");
  llvm_position_at_end_of_bb(env,cond_bb);
  llvm_value cond =llvm_build(cond,env);
