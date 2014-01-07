#include "common.h"
#include "prim.h"
/* minargs=0,maxarg=1,restarg=1*/
#define eval_sub eval
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
sexp lisp_defvar(sexp var,sexp val,sexp docstr,env *cur_env){
  if(!getSymFromSexp(var,NULL)){//if var isn't already bound
    symref new_sym=xmalloc(sizeof(symbol));
    new_sym->name=var.val.var->name;
    new_sym->val=eval_sub(val,cur_env);//set var to val
    //set docstring to docstr iff docstr is a string, but just ignore it if not
    if(STRINGP(docstr)){
      new_sym->props.doc=docstr.val.cord;
    }
  }
  return var;//always return var
}
sexp lisp_defun(sexp var,sexp arglist,sexp body,env *cur_env){
  sexp val=Cons(spec_sexp(_lambda),Cons(arglist,body));//(lambda <arglist> <body>...)
  addSymFromSexp(var,val,cur_env);
  return var;
}
sexp lisp_define(sexp var,sexp val,env *cur_env){
  addSymFromSexp(var,eval_sub(val,cur_env),cur_env);
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
sexp lisp_setq(sexp args,env *cur_env){
  sexp var,val;
  if(CONSP(args)){
    var=XCAR(args);
    if(!CONSP(XCDR(args))){
      return error_sexp("uneven number of args passed to setq");
    }
    val=eval_sub(XCADR(args),cur_env);
    args=XCDDR(args);
    symref sym=getSymFromSexp(var,cur_env);
    if(!sym){
      addSymFromSexp(var,val,cur_env);
    } else {
      sym->val=val;
    }
  }
}
      
//(if cond then &rest else)
sexp lisp_if(sexp cond,sexp then_br,sexp else_br,sexp cur_env_sexp){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp test_result=eval(cond,cur_env);
  if(ERRORP(cond)){
    return cond;
  }
  if(isTrue(test_result)){
    return eval(then_br,cur_env);
  } else {
    return eval(else_br,cur_env);
  }
}
sexp lisp_dotimes_expander(sexp var,sexp times,sexp body,sexp cur_env_sexp,int expand){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp test=Cons(function_sexp(&lisp_numlt_call),Cons(var,Cons(times,NIL)));
  sexp do_parameters=
    Cons(var,Cons(long_sexp(0),Cons(long_sexp(1),Cons(test,NIL))));
  sexp code=Cons(spec_sexp(_do),
                 Cons(do_parameters,body));
  if(expand){
    return code;
  } else {
    return eval(code,cur_env);
  }
}
sexp lisp_dolist_expander(sexp var,sexp list,sexp body,sexp cur_env_sexp,int expand){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp test=Cons(function_sexp(&lisp_consp_call),Cons(list,NIL));
  sexp var_step=Cons(spec_sexp(_setq),
                     Cons(var,Cons(function_sexp(&car_call),Cons(list,NIL))));
  sexp list_step=Cons(spec_sexp(_setq),
                      Cons(list,Cons(function_sexp(&cdr_call),Cons(list,NIL))));
  sexp step=Cons(spec_sexp(_progn),Cons(var_step,Cons(list_step,NIL)));
  sexp loop=Cons(spec_sexp(_while),Cons(test,Cons(step,Cons(body,NIL))));
  if(expand){
    return loop;
  } else {
    return eval(loop,cur_env);
  }
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
sexp lisp_incf_expander(sexp sym_sexp,env *cur_env){
  /* (incf <var>) ->
     `(setq ,var (++ ,var))
     (setq . (var . ((++ . (var . nil)) . nil)))
  */
  symref inc_symbol=xmalloc(sizeof(symbol));
  inc_symbol->name="++";
  inc_symbol->val=UNBOUND;
  sexp body=Cons(symref_sexp(inc_symbol),
                 Cons(eval_sub(sym_sexp,cur_env),NIL));
  sexp code=Cons(spec_sexp(_setq),Cons(sym_sexp,Cons(body,NIL)));
  /*  cons *code=xmalloc(5*sizeof(cons));
  cons *code_ptr=code;
  code_ptr->car=spec_sexp(_setq);// (setq .
  code_ptr->cdr=cons_sexp(code+1);// (setq . (
  code_ptr=code_ptr->cdr.val.cons;
  code_ptr->car=sym_sexp;//(setq . (var
  code_ptr->car.has_comma=1;//(setq . (,var
  code_ptr->cdr=cons_sexp(code+2);
  code_ptr=code_ptr->cdr.val.cons;
  code_ptr->cdr=NIL;//(setq . (,var . (_ . nil)))
  code_ptr->car=cons_sexp(code+3);
  code_ptr=code_ptr->car.val.cons;

  code_ptr->car=symref_sexp(inc_symbol);
  code_ptr->cdr=cons_sexp(code+4);
  code_ptr->car=sym_sexp;
  code_ptr->cdr=NIL;
  sexp retval=cons_sexp(code);
  retval.quoted=1;
  retval.has_comma=1;
  return retval;*/
}
