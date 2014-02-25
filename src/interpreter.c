/* lisp interpreter and special forms

   copyright (c) 2013-2014 tucker dinapoli

   this file is part of scilisp.

   scilisp is free software: you can redistribute it and/or modify
   it under the terms of the gnu general public license as published by
   the free software foundation, either version 3 of the license, or
   (at your option) any later version.

   scilisp is distributed in the hope that it will be useful,
   but without any warranty; without even the implied warranty of
   merchantability or fitness for a particular purpose.  see the
   gnu general public license for more details.

   you should have received a copy of the gnu general public license
   along with scilisp.  if not, see <http://www.gnu.org*/
#include "common.h"
#include "prim.h"
#include "cons.h"
sexp eval_top(sexp expr,env_ptr env);
sexp lookup_var (symbol *sym,env_ptr env);
sexp funcall(subr sub,sexp args,env_ptr env);
//special forms, and macros reimplemented as special forms for speed
//these aren't really treated specially, and are just called similarly
//to functions
sexp lisp_c_funcall(subr *c_fun,int num_args,env_ptr env);
sexp lisp_and(sexp expr);
sexp lisp_or(sexp expr);
sexp lisp_defun(sexp expr);
sexp lisp_defvar(sexp expr);
sexp lisp_defmacro(sexp expr);
sexp lisp_lambda(sexp expr,env_ptr env);
sexp lisp_quote(sexp expr);
sexp lisp_setq(sexp expr,env_ptr env);
sexp eval(sexp expr,sexp env);
sexp lisp_progn(sexp args);//special form
sexp lisp_prog1(sexp args);//specail form
sexp lisp_prog2(sexp args);//macro
sexp lisp_while(sexp expr,env_ptr env);//special form implementing looping
sexp lisp_if(sexp expr);//special form implementing conditionals
sexp lisp_do(sexp expr);//macro
sexp lisp_dolist(sexp expr);//macro
sexp lisp_let(sexp args,env_ptr env);
sexp lisp_let_star(sexp args,env_ptr env);
sexp lisp_flet(sexp args,env_ptr env);
sexp lisp_flet_star(sexp args,env_ptr env);
sexp apply(uint64_t numargs,sexp *args);//function
static sexp get_symbol_value(symbol *sym,env_ptr env);
sexp eval(sexp expr,sexp env){
  if(nilp(env)){
    return eval_top(expr,current_env);
  } else {
    raise_simple_error(etype,"error eval with non current environment unimplemented");
  }
}
/* minargs=0,maxarg=1,restarg=1...?*/
sexp eval_top(sexp expr,env_ptr env){
  switch(expr.tag){
    case sexp_sym:
      //if(expr.val.sym->special){return expr.val.sym->val;}
      return get_symbol_value(expr.val.sym);
    case sexp_cons:
      if(!symbolp(xcar(expr))){
        raise_simple_error_fmt(etype,"invalid function %r",print(xcar(expr)).str);
      }
      sexp subr_var=get_symbol_value(expr.val.sym);
      if(!subrp(subr_var)){
        raise_simple_error_fmt(etype,"invalid function %r",print(xcar(expr)).str);
      }
      return funcall(subr_var.val.subr,xcddr(expr),env);
      //it might be a good idea to dispatch on special forms via switching
      //on the symbol before looking it up and calling funcall
      //being as it would only add 1 extra comparison and a jump
      //and special forms happen often enough that it'd probably be worth it
    default:
     return expr;
  }
}
sexp lookup_var (symbol *sym,env_ptr env){
  if(env->lex_env){
    sexp lex_binding=lex_assq(env->lex_env,sym);
    if(!nilp(lex_binding)){
      return xcdr(lex_binding):
    }
  }
  if(unbound(sym->val)){
      raise_simple_error_fmt(eunbound,"error undefined variable %r",sym->name->name);
  } else {
    return symref_sexp(sym);
  }
}
/* search current lexical environment (if any) for sym, and return
   the value if found, if not found raise an error if sym is undefined
   otherwise return the value of sym
 */
static inline sexp get_symbol_value(symbol *sym,env_ptr env){
  if(!nilp(env->lex_env)){//if there is a lexical environment search it first
    sexp lex_binding=c_assq(env->lex_env,sym);
    if(!nilp(lex_binding)){
      return xcdr(lex_binding);
    }
  }
  if(unbound(sym->val)){
    raise_simple_error_fmt(eunbound,"error undefined variable %r",sym->name->name);
  } else {
    return sym->val;
  }
}
//shortcut for evaluating arguments, assuming
//enough arguments are symbols or literals to make
//this worthwhile
#define eval_arg(arg,env)                       \
  ({sexp argval;                                \
  if(!consp(arg) && !symbolp(arg)){             \
    argval=arg;                                 \
  } if(symbolp(arg)){                           \
    argval=lookup_arg(arg,env);                 \
  } else {                                      \
    argval=eval(arg,env);                       \
  }                                             \
  argval;})

//not sure where to ultimately put this but i want to write it now


#define eval_sub eval_top
sexp lisp_c_funcall(subr *c_fun,int num_args,env_ptr env){
  /*already delt with in general funcall
    int num_args=data_size(env);
  if(num_args < c_fun->req_args){
    return format_error_sexp("too few args passed to %s",c_fun->lname->string);
  }
  if(num_args>c_fun->maxargs && !(c_fun->has_rest_arg)){
    return format_error_sexp("excess args passed to %s",c_fun->lname->string);
    }*/
  if(c_fun->has_rest_arg){
    sexp *args=xmalloc(sizeof(sexp)*num_args);
    mempcy(args,env->data_stack,sizeof(sexp)*num_args);
    return c_fun->comp.fmany(num_args,args);
  }
  sexp *args=xmalloc(sizeof(sexp)*c_fun->maxargs);
  memcpy(args,env->data_stack,sizeof(sexp)*num_args);
  //gc_malloc zeros storage, nil is defined such that it is a sexp with
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
  sexp retval=lisp_true;
  while(consp(exprs)){
    if(!(istrue(eval(xcar(exprs),cur_env)))){
      return lisp_false;
    } else {
      exprs=xcdr(exprs);
    }
  }
  return retval;
}
//this isn't really atomic, fix that
sexp lisp_defvar(sexp args){
  sexp var,val,docstr=nil;
  var=xcar(args);
  args=xcdr(args);
  val=(consp(args)?xcar(args):nil);
  if(consp(xcdr(args))){
    docstr=xcadr(args);
  }
  if(var.val.sym->val == unbound){
    var.val.sym->special=1;
    var.val.sym->val=eval(val,current_env);
    if(!nilp(docstr)){
      var.val.sym->plist=cons(qdocstring,cons(docstr,var.val.sym->plist));
    }
  }
  return var;
}
sexp lisp_defun(sexp args){
  sexp var=xcar(args);
  args=xcdr(args);
  if(!consp(args) || !consp(xcdr(args))){
    return error_sexp("malformed defun");
  }
  if(!cons_or_nil(xcar(args))){
    return error_sexp("malformed argument list");
  }
  sexp arglist=xcar(args);
  sexp body=xcadr(args);
  //defun overwrites any existing defination
  var.val.sym->val=cons(qlambda,cons(arglist,cons(body,nil)));
  var.val.sym->special=1;
  return var;
}
//i think that this counts as atomic, single writes
//should be atomic
sexp lisp_setq(sexp args,env_ptr env){
  //(setq [place form]*)
  if(nilp(args)){
    return nil;
  }
  //we need to make sure we have an even number of args first
  //otherwise we might set some values and  not others
  uint32_t len=cons_len(args);
  if(args%2){
    raise_simple_error(eargs,"uneven number of arguments to setq");
  }
  sexp var,val=nil;
  while(consp(args)){
    var=xcar(args);
    val=eval(xcadr(args),env);
    args=xcddr(args);
    //should (setq <undefined symbol> val) set the value of the symbol
    //in the current lexical environment, of set the global value of symbol?
    if(!nilp(env->lex_env)){
      sexp lex_var=c_assq(env->lex_env,var);
      if(!nilp(lex_var)){
        //don't modify lexical environments, just push on a new value
        push(env->lex_env,fcons(var,val));
        continue;
      }
    }
    val.var.sym->val=val;
  }
  return val;
}
sexp lisp_defmacro(sexp args){
}

sexp lisp_or(sexp exprs){
  sexp retval=lisp_false;
  while(consp(exprs)){
    if(istrue(eval(xcar(exprs),currrent_env))){
      return lisp_true;
    } else {
      exprs=xcdr(exprs);
    }
  }
  return retval;
}
//(when cond &rest then)
sexp lisp_when(sexp args){
  sexp cond=xcar(args);
  args=xcdr(args);
  return lisp_progn(args);
}
//(if cond then &rest else)
sexp lisp_if(sexp args,env_ptr env){
  if(!consp(args) || !(consp(xcdr(args)))){
    return error_sexp("too few arguments passed to if");
  }
  sexp cond=xcar(args);
  args=xcdr(args);
  sexp then_br=xcar(args);
  args=xcdr(args);
  sexp else_br=args;
  sexp test_result=eval(cond,current_env);
  if(errorp(cond)){
    return cond;
  }
  if(istrue(test_result)){
    return eval(then_br,current_env);
  } else {
    return lisp_progn(else_br);
  }
}
sexp lisp_lambda(sexp args,env_ptr env){
  if(!consp(xcadr(args))){
    return error_sexp("lambda missing argument list");
  }
  if(!nilp(env->lex_env)){
    sexp closure=cons(qclosure,env->lex_env);
    return cons(closure,xcdr(args));
  } else {
    return args;
  }
}
//common to all kinds of let
//checks argumens and deals with trivial cases (i.e (let() ...)
//needs a better error message  for (let (<atom>))(ie not (let (<cons>*))
#define let_prefix()                                                    \
  args=xcdr(args);                                                      \
  if(!consp(args)){                                                     \
    raise_simple_error(eargs,format_arg_error("let","1 or more","0"));  \
  } else if (!consp(xcar(args))){                                       \
  if(!nilp(xcar(args))){                                                \
    raise_simple_error(etype,"maleformed lex binding list");            \
  }
  if(!nilp(xcdr(args))){                                                \
    return eval(xcadr(args),env);                                        \
  } else {                                                              \
    return nil;                                                         \
  }
//i think eval xcadr args up above is right,
//the value forms in let bindings aren't wrapped in
//implicit progns, but i'm really not sure
sexp lisp_let(sexp args,env_ptr env){
  let_prefix();
  sexp lex_vars=xcar(args);
  sexp cur_var=pop(lex_vars);
  //i do this since the way things are you can't do push(val,nil)
  //without dereferencing a null pointer
  sexp lex_env=c_list1(fcons(xcar(cur_var),eval(xcdr(cur_var,env))));
  while(consp(lex_vars) && (cur_var=pop(lex_vars))){
  //maybe check if cdr is nil before calling eval ?
    push(lex_env,fcons(xcar(cur_var),eval(xcdr(cur_var,env))));
  }
  if(!nilp(lex_vars)){
    raise_simple_error(etype,"maleformed lex binding list");
  }
  sexp old_lex_env=env->lex_env;
  env->lex_env=append(lex_env,env->lex_env);
  sexp retval=eval(xcdr(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
sexp lisp_progv(sexp args,env_ptr env){
}
sexp lisp_let_star(sexp args,env_ptr env){
  let_prefix();
  sexp lex_vars=xcar(args);
  sexp cur_var=pop(lex_vars);
  sexp old_lex_env=env->lex_env;
  //maybe check if cdr is nil before calling eval ?
  do {
    push(env->lex_env,fcons(xcar(cur_var),eval(xcdr(cur_var,env))));
  } while (consp(lex_vars) && (cur_var=pop(lex_vars)));
  if(!nilp(lex_vars)){
    env->lex_env=old_lex_env;
    raise_simple_error(etype,"maleformed lex binding list");
  }
  sexp retval=eval(xcdr(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
/* unlike in common lisp this is just a macro,
   given:(flet ((<name> (<lambda_list>) <body>)*) body...)
   translate to:(let ((name (lambda (<lambda_list>) <body>))) body...)

   we don't actually do the macro expansion in the interpreter unless
   the user actually calls macroexpand, since it's faster to just do
   the macroexpansion inline and in interpreted code there's no difference
 */
//flet needs to use closures to prevent the bound functions from seeing themselves
#define flet_expand_sub(lex_var)                                        \
  (fcons(xcar(lex_var),fcons2(qclosure,env->lex_env,xcdr(lex_var))))
#define flet_star_expand_sub(lex_var)                                   \
  (fcons(xcar(lex_var),fcons(qlambda,xcdr(lex_var))))
sexp lisp_flet(sexp args,env_ptr env){
  lex_prefix();
  sexp lex_vars=xcar(args);
  sexp cur_var=pop(lex_vars);
  sexp lex_env=c_list1(flet_expand_sub(cur_var));
  while(consp(lex_vars) && (cur_var=pop(lex_vars))){
    push(lex_env,flet_expand_sub(cur_var));
  }
  if(!nilp(lex_vars)){
    raise_simple_error(etype,"maleformed lex binding list");
  }
  sexp old_lex_env=env->lex_env;
  env->lex_env=append(lex_env,env->lex_env);
  sexp retval=eval(xcdr(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
sexp lisp_flet_star(sexp args,env_ptr env){
  let_prefix();
  sexp lex_vars=xcar(args);
  sexp cur_var=pop(lex_vars);
  sexp old_lex_env=env->lex_env;
  //maybe check if cdr is nil before calling eval ?
  do {
    push(env->lex_env,flet_star_expand_sub(cur_var));
  } while (consp(lex_vars) && (cur_var=pop(lex_vars)));
  if(!nilp(lex_vars)){
    env->lex_env=old_lex_env;
    raise_simple_error(etype,"maleformed lex binding list");
  }
  sexp retval=eval(xcdr(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
sexp flet_macroexpand(sexp args,env_ptr env){
  sexp code_ptr=xcdr(args);
  if(!consp(args)){
    raise_simple_error(eargs,format_arg_error("let","1 or more","0"));
  } else if(!consp(xcar(args))){
    return args;//macro expansion i guess doesn't check types
  }
  code_ptr=xcar(code_ptr);
  sexp cur_var=pop(code_ptr);
  do {
    set_cdr(cur_var,fcons2(qclosure,env->lex_env,xcdr(lex_var)));
  } while (consp(code_ptr) && (cur_var=pop(code_ptr)));
  return args;
}
sexp flet_star_macroexpand(sexp args,env_ptr env){
  sexp code_ptr=xcdr(args);
  if(!consp(args)){
    raise_simple_error(eargs,format_arg_error("let","1 or more","0"));
  } else if(!consp(xcar(args))){
    return args;//macro expansion i guess doesn't check types
  }
  code_ptr=xcar(code_ptr);
  sexp cur_var=pop(code_ptr);
  do {
    set_cdr(cur_var,fcons(qlambda,xcdr(lex_var)));
  } while (consp(code_ptr) && (cur_var=pop(code_ptr)));
  return args;
}
sexp lisp_macrolet(sexp args,env_ptr env){}
//simple looping construct
//(while cond &rest body)
sexp lisp_while(sexp cond,sexp body){
  sexp result;
  while(istrue(eval(cond,current_env))){
    result=eval(body,current_envrionment);
  }
  return result;
}
sexp lisp_progn(sexp args){
  sexp result=nil;
  while(consp(args)){
    result=eval(xcar(args),current_env);
    args=xcdr(args);
  }
  return result;
}
sexp lisp_prog1(sexp expr,sexp args){
  sexp result=eval(expr,current_env);
  while(consp(args)){
    eval(xcar(args),current_envrionment);
  }
  return result;
}
sexp lisp_prog2(sexp expr1,sexp expr2,sexp args){
  eval(expr1,current_env);
  sexp result=eval(expr2,current_env);
  while(consp(args)){
    eval(xcar(args),current_envrionment);
  }
  return result;
}
//(do (var init [step])(end-test) body..)
sexp lisp_do_expander(sexp args){
  if(!consp(args) || !consp(xcdr(args))){
    return error_sexp("too few args passed to do");
  }
  sexp binding=xcar(args);
  if(!consp(binding)){
    return error_sexp("malformed bindings list in do expression");
  }
}
sexp lisp_dotimes_expander(sexp var,sexp times,sexp body,sexp cur_env_sexp,int expand){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp test=cons(function_sexp(&lisp_numlt_call),cons(var,cons(times,nil)));
  sexp do_parameters=
    cons(var,cons(long_sexp(0),cons(long_sexp(1),cons(test,nil))));
  sexp code=cons(spec_sexp(_do),
                 cons(do_parameters,body));
  if(expand){
    return code;
  } else {
    return eval(code,cur_env);
  }
}
sexp lisp_dolist_expander(sexp var,sexp list,sexp body,sexp cur_env_sexp,int expand){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp test=cons(function_sexp(&lisp_consp_call),cons(list,nil));
  sexp var_step=cons(spec_sexp(_setq),
                     cons(var,cons(function_sexp(&car_call),cons(list,nil))));
  sexp list_step=cons(spec_sexp(_setq),
                      cons(list,cons(function_sexp(&cdr_call),cons(list,nil))));
  sexp step=cons(spec_sexp(_progn),cons(var_step,cons(list_step,nil)));
  sexp loop=cons(spec_sexp(_while),cons(test,cons(step,cons(body,nil))));
  if(expand){
    return loop;
p  } else {
    return eval(loop,cur_env);
  }
}
sexp lisp_dec_ref(sexp sym_sexp,sexp cur_env_sexp){
  if(!symbolp(sym_sexp)){
    return format_type_error("decf","symbol",sym_sexp.tag);
  }
  env *cur_env=cur_env_sexp.val.cur_env;
  symref sym=getsym(cur_env,sym_sexp.val.var->name);
  if(!sym){
    return format_error_sexp("undefined variable %r",sym->name);
  }
  sexp temp=lisp_dec(sym->val);
  if(errorp(temp)){
    return temp;
  } else {
    sym->val=temp;
    return temp;
  }
}
/*sexp lisp_defconst(sexp sym_sexp,sexp val_sexp,sexp cur_env_sexp){
  sexp code=cons(spec_sexp(_def),cons(sym_sexp,cons(val_sexp,nil)));
  eval(code,cur_env_sexp.val.cur_env);
  }*/
sexp lisp_inc_ref(sexp sym_sexp,sexp cur_env_sexp){
  if(!symbolp(sym_sexp)){
    return format_type_error("incf","symbol",sym_sexp.tag);
  }
  env *cur_env=cur_env_sexp.val.cur_env;
  symref sym=getsym(cur_env,sym_sexp.val.var->name);
  if(!sym){
    return format_error_sexp("undefined variable %r",sym->name);
  }
  sexp temp=lisp_inc(sym->val);
  if(errorp(temp)){
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
  inc_symbol->val=unbound;
  sexp body=cons(symref_sexp(inc_symbol),
                 cons(eval_sub(sym_sexp,cur_env),nil));
  sexp code=cons(spec_sexp(_setq),cons(sym_sexp,cons(body,nil)));
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
//rought sketch of how to do this
sexp cond_expand(sexp expr){
  sexp retval=expr;
  sexp cond_case;
  while(!nilp((cond_case=pop(expr)))){
    //(test then...)->(test
    xcdr(cond_case)=fcons(qprogn,xcdr(cond_case));
    push(cond_case,qif);
  }
}
sexp apply(uint64_t numargs,sexp *args){
  sexp fun_sym=*args++;
  if(!FUNCTIONP(fun_sym.val.sym->val)){
    return error_sexp
      (CORD_cat_const_char_star("No function ",sym->name->name,sym->name->len));
  }
}
sexp funcall(subr sub,sexp args,env_ptr env){
  switch(sub->subr_type){
    case subr_compiled:{
      int numargs,maxargs;
      int minargs=sub->minargs;
      if(sub->rest_arg){
        maxargs=data_stack_size/sizeof(sexp);
      } else {
        maxargs=sub->maxargs;
      }
      while(CONSP(args)){
        sexp arg=POP(args);
        if(!CONSP(arg) && !SYMBOLP(arg)){
          push_data(arg);
        } if(SYMBOLP(arg)){
          push_data(lookup_var(arg,env));
        } else {
          push_data(eval(arg,env));
        }
        //only call eval if we have to(i.e. if arg is a cons cell
        numargs++;
        if(numargs>maxargs){
          raise_simple_error_fmt(Eargs,"Excess args passed to %s",sym->name->name);
        }
      }
      if(numargs<minargs){
        raise_simple_error_fmt(Eargs,"Too few args passed to %s",sym->name->name);
      }
      return lisp_c_funcall(sub,namargs,env);
    }
    case subr_compiler_macro:
      return subr->comp.funevaled(args);
    case subr_special_form:
      return subr->comp.fspecial(args,env);
    case subr_lambda
    case subr_closure:{
      cons *lambda=sub->lambda_body;
      sexp fun_env=NIL;
      if(lambda->car.val.uint64 == (uint64_t)Qclosure){
        fun_env=Fcons(XCAR(lambda->cdr),NIL);
        lambda=lambda->cdr.val.cons->cdr.val.cons;
      } else {
        lambda=lambda->cdr.val.cons;
      }
      lambda_list arglist=*(sub->lambda_arglist);
      int num_reqargs = arglist->req_args->car.val.int64;
      symbol *req_arg_names= arglist->req_args->cdr.sym;
      int num_optargs = arglist->opt_args->car.val.int64;
      cons*opt_arg_names= arglist->opt_args->cdr.sym;//array of conses
      int num_keyargs = arglist->key_args->car.val.int64;
      cons *key_arg_names= arglist->key_args->cdr.sym;//array of conses
      /*
      cons *opt_arg_names= opt_args->cdr.sym;
      if(num_optargs){
        //push the defaulats, if any optional arguments
        //get passed they get pushed on top of these
        //and the defaults never get seen
        PUSH(cons_sexp(opt_arg_names),fun_env);
        }*/
      int i;
      for(i=0;i<num_reqargs;i++){
        if(!CONSP(args)){
          raise_simple_error_fmt(Eargs,"Too few args passed to %r",sub->lname->cord);
        } else {
          PUSH(Fcons(req_arg_names[i],eval_arg(POP(args),env)));
        }
      }
      for(i=0;i<num_optargs;i++){
        if(!CONSP(args)){
          while(i<num_optargs){
            PUSH(cons_sexp(&opt_arg_names[i++]),fun_env);
          }
          break;
        } else {
          PUSH(Fcons(opt_arg_names[i].car,eval_arg(POP(args),fun_env)));
        }
      }
      /*      for(i=0;i<num_keyargs;i++){
        if(!CONSP(args)){
          while(i<num_keyargs){
            PUSH(key_arg_names[i++].cdr,fun_env);
          }
          break;
        } else {
          sexp key = POP(args);
          if(!SYMBOL(key)){
            raise_simple_error_fmt(Etype,"Invaild keyword %s, expected a symbol",
                                   print(key).cord);
          } else {
          while*/
      CALL:{
        subr_call fcall=(subr_call){.lex_env=env->lex_env,.lisp_subr=sub,
                                    .bindings_index=env->bindings_index};
        push_call(env,fcall);
        env->lex_env=Fcons(fun_env,env->lex_env);
        sexp retval=eval(sub->lambda,env);
        env->lex_env=pop_call(env).lex_env;
        return retval;
      }
    }
  }
}
