#include "common.h"
#include "prim.h"
/* minargs=0,maxarg=1,restarg=1*/
sexp lisp_and(sexp exprs,sexp cur_env_sexp){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp retval=LISP_TRUE;
  while(CONSP(exprs)){
    if(!(isTrue(eval(XCAR(exprs),cur_env)))){
      return LISP_FALSE;
    } else {
      exprs=XCDR(exprs);
    }
  }
  return retval;
}
sexp lisp_or(sexp exprs,sexp cur_env_sexp){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp retval=LISP_FALSE;
  while(CONSP(exprs)){
    if(isTrue(eval(XCAR(exprs),cur_env))){
      return LISP_TRUE;
    } else {
      exprs=XCDR(exprs);
    }
  }
  return retval;
}
sexp lisp_dec_ref(sexp sym_sexp,sexp cur_env_sexp){
  if(!SYMBOLP(sym_sexp)){
    return format_type_error("decf","symbol",sym_sexp.tag);
  }
  env *cur_env=cur_env_sexp.val.cur_env;
  symref sym=getSym(cur_env,sym_sexp.val.var->name);
  if(!sym){
    return format_error_sexp("undefined variable %r",sym->name);
  }
  sexp temp=lisp_dec(sym->val);
  if(ERRORP(temp)){
    return temp;
  } else {
    sym->val=temp;
    return temp;
  }
}
/*sexp lisp_defconst(sexp sym_sexp,sexp val_sexp,sexp cur_env_sexp){
  sexp code=Cons(spec_sexp(_def),Cons(sym_sexp,Cons(val_sexp,NIL)));
  eval(code,cur_env_sexp.val.cur_env);
  }*/
sexp lisp_inc_ref(sexp sym_sexp,sexp cur_env_sexp){
  if(!SYMBOLP(sym_sexp)){
    return format_type_error("incf","symbol",sym_sexp.tag);
  }
  env *cur_env=cur_env_sexp.val.cur_env;
  symref sym=getSym(cur_env,sym_sexp.val.var->name);
  if(!sym){
    return format_error_sexp("undefined variable %r",sym->name);
  }
  sexp temp=lisp_inc(sym->val);
  if(ERRORP(temp)){
    return temp;
  } else {
    sym->val=temp;
    return temp;
  }
}
