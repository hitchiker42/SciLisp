#include "common.h"
#include "prim.h"
#include "cons.h"
/* minargs=0,maxarg=1,restarg=1*/
sexp eval_top(sexp expr,env_ptr env){
  switch(expr.tag){
    case sexp_sym:
      if(!NILP(env->lex_env)){//if there is a lexical environment search it first
        sexp lex_binding=c_assq(env->lex_env,expr.val.sym);
        if(!NILP(lex_binding)){
          return XCDR(lex_binding);
        }
      }
      return expr.val.sym->val;
    case cons_sym:
    default:
      return expr;
  }
}

#define eval_sub eval
sexp lisp_c_funcall(sexp c_fun,env_ptr env){
  int num_args=data_size(env);
  if(num_args < c_fun->req_args){
    return format_error_sexp("too few args passed to %s",c_fun->lname->string);
  }
  if(num_args>c_fun->maxargs && !(c_fun->has_rest_arg)){
    return format_error_sexp("excess args passed to %s",c_fun->lname->string);
  }
  if(c_fun->has_rest_arg){
    sexp *args=xmalloc(sizeof(sexp)*num_args);
    mempcy(args,env->data_stack,sizeof(sexp)*num_args);
    return c_fun->comp.fmany(num_args,args);
  }
  sexp *args=xmalloc(sizeof(sexp)*c_fun->maxargs);
  memcpy(args,env->data_stack,sizeof(sexp)*num_args);
  //gc_malloc zeros storage, NIL is defined such that it is a sexp with
  //all fields zero, so we don't need to actually set any optional arguments
  
  //this is just kinda annoying
  switch(c_fun->maxargs){
    case 0:
      return c_fun->comp.f0();
    case 1:
      return c_fun->comp.f1(args[0]);
    case 2:
      return c_fun->comp.f1(args[0],args[1]);
    case 3:
      return c_fun->comp.f1(args[0],args[1],args[2]);
    case 4:
      return c_fun->comp.f1(args[0],args[1],args[2],args[3]);
    case 5:
      return c_fun->comp.f1(args[0],args[1],args[2],args[3],args[4]);
    case 6:
      return c_fun->comp.f1(args[0],args[1],args[2],args[3],args[4],args[5]);
    case 7:
      return c_fun->comp.f1(args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
  }
}
sexp lisp_and(sexp exprs){
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
sexp lisp_defvar(sexp args){
  sexp var,val,docstr=NIL;
  var=XCAR(args);
  args=XCDR(args);
  val=(CONSP(args)?XCAR(args):NIL);
  if(CONSP(XCDR(args))){
    docstr=XCADR(args);
  }
  if(var.val.sym->val == UNBOUND){
    var.val.sym->val=eval(val,current_environment);
    if(!NILP(docstr)){
      var.val.sym->plist=Cons(Qdocstring,Cons(docstr,var.val.sym->plist));
    }
  }
  return var;
}
sexp lisp_defun(sexp args){
  sexp var=XCAR(args);
  args=XCDR(args);
  if(!CONSP(args) || !CONSP(XCDR(args))){
    return error_sexp("Malformed defun");
  }
  if(!CONS_OR_NIL(XCAR(args))){
    return error_sexp("Malformed argument list");
  }
  sexp arglist=XCAR(args);
  sexp body=XCADR(args);
  //defun overwrites any existing defination
  var.val.sym->Val=Cons(Qlambda,Cons(arglist,Cons(body,NIL)));
  return var;
}
sexp lisp_setq(sexp args){
  if(!CONSP(args)|!CONSP(XCDR(args))){
    return error_sexp("too few arguments to setq");
  }
  sexp var=XCAR(args);
  sexp val=XCADR(args);
  val.var.sym->val=eval(val,current_environment);
  return var;
}
sexp lisp_defmacro(sexp args){}

sexp lisp_or(sexp exprs){
  sexp retval=LISP_FALSE;
  while(CONSP(exprs)){
    if(isTrue(eval(XCAR(exprs),currrent_environment))){
      return LISP_TRUE;
    } else {
      exprs=XCDR(exprs);
    }
  }
  return retval;
}
//(when cond &rest then)
sexp lisp_when(sexp args){
  sexp cond=XCAR(args);
  args=XCDR(args);
  return lisp_progn(args);
}
//(if cond then &rest else)
sexp lisp_if(sexp args){
  if(!CONSP(args) || !(CONSP(XCDR(args)))){
    return error_sexp("too few arguments passed to if");
  }
  sexp cond=XCAR(args);
  args=XCDR(args);
  sexp then_br=XCAR(args);
  args=XCDR(args);
  sexp else_br=args;
  sexp test_result=eval(cond,current_environment);
  if(ERRORP(cond)){
    return cond;
  }
  if(isTrue(test_result)){
    return eval(then_br,current_environment);
  } else {
    return lisp_progn(else_br);
  }
}
sexp lisp_lambda(sexp args,env_ptr env){
  if(!CONSP(XCADR(args))){
    return error_sexp("lambda missing argument list");
  }
  if(!NILP(env->lex_env)){
    sexp closure=Cons(Qclosure,env->lex_env);
    return Cons(closure,XCDR(args));
  } else {
    return args;
  }
}
sexp lisp_let(sexp args,env_ptr env){
}
sexp lisp_let_star(sexp args,env_ptr env){}
sexp lisp_flet(sexp args,env_ptr env){}
sexp lisp_macrolet(sexp args,env_ptr env){}
//simple looping construct
//(while cond &rest body)
sexp lisp_while(sexp cond,sexp body){
  sexp result;
  while(isTrue(eval(cond,current_environment))){
    result=eval(body,current_envrionment);
  }
  return result;
}
sexp lisp_progn(sexp args){
  sexp result=NIL;
  while(CONSP(args)){
    result=eval(XCAR(args),current_environment);
    args=XCDR(args);
  }
  return result;
}
sexp lisp_prog1(sexp expr,sexp args){
  sexp result=eval(expr,current_environment);
  while(CONSP(args)){
    eval(XCAR(args),current_envrionment);
  }
  return result;
}
sexp lisp_prog2(sexp expr1,sexp expr2,sexp args){
  eval(expr1,current_environment);
  sexp result=eval(expr2,current_environment);
  while(CONSP(args)){
    eval(XCAR(args),current_envrionment);
  }
  return result;
}
//(do (var init [step])(end-test) body..)
sexp lisp_do_expander(sexp args){
  if(!CONSP(args) || !CONSP(XCDR(args))){
    return error_sexp("too few args passed to do");
  }
  sexp binding=XCAR(args);
  if(!CONSP(binding)){
    return error_sexp("malformed bindings list in do expression");
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
}
sexp apply(sexp args,envrionment *env){
  sexp fun_sym=XCAR(args).val.sym;
  if(!FUNCTIONP(sym->val)){
    return error_sexp
      (CORD_cat_const_char_star("No function ",sym->name->name,sym->name->len));
  }
  function fun=fun_sym->val;

}
//sexp c_apply(function *fun,environment *env){


void unwind_bindings(binding *bindings,int len){
  int i;
  binding cur_binding;
  for (i=0;i<len;i++){
    cur_binding=bindings[i];
    binding->sym.val=binding.prev_val;
  }
}
//internal means of lexically binding a set of variables
sexp internal_let(struct lexical_env *env,sexp form){
