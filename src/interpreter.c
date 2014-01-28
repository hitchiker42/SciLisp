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
      if(UNBOUND(expr.val.sym->val)){
        raise_simple_error_fmt(Eunbound,"Error undefined variable %r",sym->name->name);
      } else {
        return expr.val.sym->val;
      }
    case cons_sym:
      if(!SYMBOLP(XCAR(expr))){
        raise_simple_error_fmt(Etype,"Invalid function %r",print(XCAR(expr)).str);
      }
      sexp subr_var;
      if(!NILP(env->lex_env)){//if there is a lexical environment search it first
        sexp lex_binding=c_assq(env->lex_env,XCAR(expr).val.sym);
        if(!NILP(lex_binding)){
          subr_var=XCDR(lex_binding);
        }
      }
      subr_var=XCAR(expr).val.sym->val
      if(UNBOUND(subr_var)){
        raise_simple_error_fmt(Eunbound,"Error undefined variable %r",sym->name->name);
      }
      if(!SUBRP(subr_var)){
        raise_simple_error_fmt(Etype,"Invalid function %r",print(XCAR(expr)).str);
      }
      return funcall(subr_var.val.subr,XCDDR(expr),env);
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
    if(!NILP(lex_binding)){
      return XCDR(lex_binding):
    }
  }
  if(UNBOUND(sym->val)){
      raise_simple_error_fmt(Eunbound,"Error undefined variable %r",sym->name->name);
  } else {
    return symref_sexp(sym);
  }
}
//shortcut for evaluating arguments, assuming
//enough arguments are symbols or literals to make
//this worthwhile
#define eval_arg(arg,env)                       \
  ({sexp argval;                                \
  if(!CONSP(arg) && !SYMBOLP(arg)){             \
    argval=arg;                                 \
  } if(SYMBOLP(arg)){                           \
    argval=lookup_arg(arg,env);                 \
  } else {                                      \
    argval=eval(arg,env);                       \
  }                                             \
  argval;})

//not sure where to ultimately put this but I want to write it now
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

#define eval_sub eval
sexp lisp_c_funcall(subr *c_fun,int num_args,env_ptr env){
  /*  int num_args=data_size(env);
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
    var.val.sym->val=eval(val,current_env);
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
  val.var.sym->val=eval(val,current_env);
  return var;
}
sexp lisp_defmacro(sexp args){
}

sexp lisp_or(sexp exprs){
  sexp retval=LISP_FALSE;
  while(CONSP(exprs)){
    if(isTrue(eval(XCAR(exprs),currrent_env))){
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
  sexp test_result=eval(cond,current_env);
  if(ERRORP(cond)){
    return cond;
  }
  if(isTrue(test_result)){
    return eval(then_br,current_env);
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
//common to all kinds of let
//checks argumens and deals with trivial cases (i.e (let() ...)
//needs a better error message  for (let (<atom>))(ie not (let (<cons>*))
#define let_prefix()                                                    \
  args=XCDR(args);                                                      \
  if(!CONSP(args)){                                                     \
    raise_simple_error(Eargs,format_arg_error("let","1 or more","0"));  \
  } else if (!CONSP(XCAR(args))){                                       \
  if(!NILP(XCAR(args))){                                                \
    raise_simple_error(Etype,"Maleformed lex binding list");            \
  }
  if(!NILP(XCDR(args))){                                                \
    return eval(XCDR(args),env);                                        \
  } else {                                                              \
    return NIL;                                                         \
  }
sexp lisp_let(sexp args,env_ptr env){
  let_prefix();
  sexp lex_vars=XCAR(args);
  sexp cur_var=POP(lex_vars);
  //I do this since the way things are you can't do PUSH(val,NIL)
  //without dereferencing a null pointer
  sexp lex_env=c_list1(Fcons(XCAR(cur_var),eval(XCDR(cur_var,env))));
  while(CONSP(lex_vars) && (cur_var=POP(lex_vars))){
  //maybe check if cdr is nil before calling eval ?
    PUSH(lex_env,Fcons(XCAR(cur_var),eval(XCDR(cur_var,env))));
  }
  if(!NILP(lex_vars)){
    raise_simple_error(Etype,"Maleformed lex binding list");
  }
  sexp old_lex_env=env->lex_env;
  env->lex_env=APPEND(lex_env,env->lex_env);
  sexp retval=eval(XCDR(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
sexp lisp_let_star(sexp args,env_ptr env){
  let_prefix();
  sexp lex_vars=XCAR(args);
  sexp cur_var=POP(lex_vars);
  sexp old_lex_env=env->lex_env;
  //maybe check if cdr is nil before calling eval ?
  do {
    PUSH(env->lex_env,Fcons(XCAR(cur_var),eval(XCDR(cur_var,env))));
  } while (CONSP(lex_vars) && (cur_var=POP(lex_vars)));
  if(!NILP(lex_vars)){
    env->lex_env=old_lex_env;
    raise_simple_error(Etype,"Maleformed lex binding list");
  }
  sexp retval=eval(XCDR(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
/* unlike in common lisp this is just a macro,
   given:(flet ((<name> (<lambda_list>) <body>)*) body...)
   translate to:(let ((name (lambda (<lambda_list>) <body>))) body...)

   We don't actually do the macro expansion in the interpreter unless
   the user actually calls macroexpand, since it's faster to just do
   the macroexpansion inline and in interpreted code there's no difference
 */
//flet needs to use closures to prevent the bound functions from seeing themselves
#define flet_expand_sub(lex_var)                                        \
  (Fcons(XCAR(lex_var),Fcons2(Qclosure,env->lex_env,XCDR(lex_var))))
#define flet_star_expand_sub(lex_var)                                   \
  (Fcons(XCAR(lex_var),Fcons(Qlambda,XCDR(lex_var))))
sexp lisp_flet(sexp args,env_ptr env){
  lex_prefix();
  sexp lex_vars=XCAR(args);
  sexp cur_var=POP(lex_vars);
  sexp lex_env=c_list1(flet_expand_sub(cur_var));
  while(CONSP(lex_vars) && (cur_var=POP(lex_vars))){
    PUSH(lex_env,flet_expand_sub(cur_var));
  }
  if(!NILP(lex_vars)){
    raise_simple_error(Etype,"Maleformed lex binding list");
  }
  sexp old_lex_env=env->lex_env;
  env->lex_env=APPEND(lex_env,env->lex_env);
  sexp retval=eval(XCDR(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
sexp lisp_flet_star(sexp args,env_ptr env){
  let_prefix();
  sexp lex_vars=XCAR(args);
  sexp cur_var=POP(lex_vars);
  sexp old_lex_env=env->lex_env;
  //maybe check if cdr is nil before calling eval ?
  do {
    PUSH(env->lex_env,flet_star_expand_sub(cur_var));
  } while (CONSP(lex_vars) && (cur_var=POP(lex_vars)));
  if(!NILP(lex_vars)){
    env->lex_env=old_lex_env;
    raise_simple_error(Etype,"Maleformed lex binding list");
  }
  sexp retval=eval(XCDR(args),env);
  env->lex_env=old_lex_env;
  return retval;
}
sexp flet_macroexpand(sexp args,env_ptr env){
  sexp code_ptr=XCDR(args);
  if(!CONSP(args)){
    raise_simple_error(Eargs,format_arg_error("let","1 or more","0"));
  } else if(!CONSP(XCAR(args))){
    return args;//macro expansion I guess doesn't check types 
  }
  code_ptr=XCAR(code_ptr);
  sexp cur_var=POP(code_ptr);
  do {
    SET_CDR(cur_var,Fcons2(Qclosure,env->lex_env,XCDR(lex_var)));
  } while (CONSP(code_ptr) && (cur_var=POP(code_ptr)));
  return args;
}
sexp flet_star_macroexpand(sexp args,env_ptr env){
  sexp code_ptr=XCDR(args);
  if(!CONSP(args)){
    raise_simple_error(Eargs,format_arg_error("let","1 or more","0"));
  } else if(!CONSP(XCAR(args))){
    return args;//macro expansion I guess doesn't check types 
  }
  code_ptr=XCAR(code_ptr);
  sexp cur_var=POP(code_ptr);
  do {
    SET_CDR(cur_var,Fcons(Qlambda,XCDR(lex_var)));
  } while (CONSP(code_ptr) && (cur_var=POP(code_ptr)));
  return args;
}
sexp lisp_macrolet(sexp args,env_ptr env){}
//simple looping construct
//(while cond &rest body)
sexp lisp_while(sexp cond,sexp body){
  sexp result;
  while(isTrue(eval(cond,current_env))){
    result=eval(body,current_envrionment);
  }
  return result;
}
sexp lisp_progn(sexp args){
  sexp result=NIL;
  while(CONSP(args)){
    result=eval(XCAR(args),current_env);
    args=XCDR(args);
  }
  return result;
}
sexp lisp_prog1(sexp expr,sexp args){
  sexp result=eval(expr,current_env);
  while(CONSP(args)){
    eval(XCAR(args),current_envrionment);
  }
  return result;
}
sexp lisp_prog2(sexp expr1,sexp expr2,sexp args){
  eval(expr1,current_env);
  sexp result=eval(expr2,current_env);
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
