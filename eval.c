/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/*TODO: add quoting in such a way that I can assign a list 
  to a variable*/
#include "common.h"
#include "cons.h"
jmp_buf error_buf;
//static fuctions which are really just part of eval, but broken
//up into seperate functions to modularise eval, and make it 
//easier to write and understand, should be self explanitory
static sexp call_builtin(sexp expr,env *cur_env);
static sexp call_lambda(sexp expr,env *cur_env);
static sexp eval_special(sexp expr,env *cur_env);
static sexp eval_def(sexp expr,env *cur_env);
static sexp eval_defun(sexp expr,env *cur_env);
static sexp eval_if(sexp expr,env *cur_env);
static sexp eval_while(sexp expr,env *cur_env);
static sexp eval_lambda(sexp expr,env *cur_env);
static sexp eval_progn(sexp expr,env *cur_env);
static sexp eval_prog1(sexp expr,env *cur_env);
static sexp eval_do(sexp expr,env *cur_env);
static sexp eval_let(sexp expr,env *cur_env);
static sexp eval_and(sexp expr,env *cur_env);
static sexp eval_or(sexp expr,env *cur_env);
//standard error handling function
static sexp handle_error(void){
  CORD_fprintf(stderr,error_str);fputs("\n",stderr);
  //  return error_sexp(error_str); would need to copy error_str to do this
  longjmp(error_buf,-1);
  return NIL;
}
//evaluate the lisp expression expr in the environment cur_env
sexp eval(sexp expr,env *cur_env){
  symref tempsym=0;
  switch(expr.tag){
    //a cons cell must be a function call or a special form
    case _cons:
      if(SYMBOLP(car(expr))){
        sexp curFun=XCAR(expr).val.var->val;
        if(LAMBDAP(curFun)){
          return call_lambda(expr,cur_env);
        }
        if(!FUNP(curFun)){          
          CORD_fprintf(stderr,"tag = %s, name = %s\n",typeName(curFun),
                       XCAR(expr).val.var->name);
          CORD_sprintf(&error_str,"%r is not a function or special form",
                       print(curFun));
          goto ERROR;
        } else {
          return call_builtin(expr,cur_env);
        }
      } else if(SPECP(car(expr))){        
        return eval_special(expr,cur_env);
      } else {
        format_error_str("car of unquoted list is not a function or special form"
                         "\ncar is %s",print(car(expr)));
        
        goto ERROR;
      }
    case _sym:
      tempsym = getSym(cur_env,expr.val.var->name);
      if(tempsym){
        return eval(tempsym->val,cur_env);
      } else {
        CORD_sprintf(&error_str,"undefined variable %r used",expr.val.var->name);
        goto ERROR;
      }
    case _fun:
      return expr;
    case _error:
      error_str=expr.val.cord;
      return handle_error();
    default:
      return expr;
  }
 ERROR:
  return handle_error();
}
static inline sexp eval_special(sexp expr,env *cur_env){
  //this is an internal only inline function, ie this function itself
  //won't be in the generated code, it's just used to git the source code
  //a bit more clarity
  //this is always called on a cons, no need to check
  sexp special_sexp=car(expr);
  symref newSym;
  switch(special_sexp.val.special){
    //for now focus on def,defun,if and do
    case _def:
      return eval_def(expr,cur_env);
    case _setq: 
      newSym = getSym(cur_env,cadr(expr).val.var->name);
      sexp symVal=eval(caddr(expr),cur_env);
      if(!newSym){
        newSym=xmalloc(sizeof(symbol_val));
        newSym->name=(cadr(expr).val.var->name);
        newSym=addSym(cur_env,newSym);
        newSym->val=symVal;
      } else {
        newSym->val=symVal;
      }
      return (sexp){.tag = _sym,.val={.var = newSym}};
    case _lambda: 
      return eval_lambda(expr,cur_env);
    case _if: 
      return eval_if(expr,cur_env);
    case _do: 
      return eval_do(expr,cur_env);
    case _while:
      return eval_while(expr,cur_env);
    case _defun:
      return eval_defun(expr,cur_env);
    case _progn:
      return eval_progn(expr,cur_env);
    case _prog1:
      return eval_prog1(expr,cur_env);
    case _let:
      return eval_let(expr,cur_env);
    case _and:
      return eval_and(expr,cur_env);
    case _or:
      return eval_or(expr,cur_env);
    default:
      goto error;
  }
 error:
  return handle_error();
}

static sexp* get_args(sexp arglist,function fun,env *cur_env){
  //arglist is (sexp . (sexp . (sexp ....()...)))
  int minargs=fun.min_args;int maxargs=fun.max_args;
  int i=0;
  sexp *args;
  if(maxargs < 0){//varargs function
    args=xmalloc(sizeof(sexp)*(minargs+1));
    while(i<minargs && CONSP(arglist)){
      args[i++]=eval(XCAR(arglist),cur_env);
      arglist=XCDR(arglist);
    }
    args[i]=arglist;
    return args;
  }
  args=xmalloc(sizeof(maxargs));
  //fill in args
  while(CONSP(arglist) && (i < maxargs)){
    args[i++]=eval(XCAR(arglist),cur_env);
    arglist=XCDR(arglist);
  }
  //check to make sure all required arguments are filled
  if (i < minargs){
    format_error_str("Too few arguments given to %s, minimum %d args",
                     function_name(fun),minargs);
    handle_error();
    return 0;
  }
  //fill in any unfilled optional arguments
  while(i<maxargs){
    args[i++]=NIL;
  }
  return args;
}
static inline sexp call_builtin(sexp expr,env *cur_env){
  sexp curFun=car(expr).val.var->val;
  //PRINT_MSG(print(expr));
  int i;
  sexp cur_arg,*args;
  long numargs;
  if(FMAX_ARGS(curFun)< 0){
    numargs=FMIN_ARGS(curFun) + 1;
  } else {
    numargs=FMAX_ARGS(curFun);
  }
  switch (numargs){
    case 0:
      if(!NILP(XCDR(expr))){
        CORD_sprintf(&error_str,"Arguments given to %r which takes no arguments",
                     FLNAME(curFun));
        PRINT_MSG(print(expr));
        return handle_error();
      } else {
        return F_CALL(curFun).f0();
      }
    case 1:
      if(!NILP(cdr(expr)) && !NILP(cddr(expr))){
        CORD_sprintf(&error_str,"Excess Arguments given to %r",
                     FLNAME(curFun));
        PRINT_MSG(print(expr));
        return handle_error();
      } else {
        return F_CALL(curFun).f1((eval(cadr(expr),cur_env)));
      }
    case 2:
      args=get_args(cdr(expr),prim_to_fun(curFun.val.fun),cur_env);
      if(!args){goto ERROR;}
      sexp retval=F_CALL(curFun).f2(args[0],args[1]);
      return retval;
    case 3:
      args=get_args(cdr(expr),prim_to_fun(curFun.val.fun),cur_env);
      if(!args){goto ERROR;}
      return F_CALL(curFun).f3(args[0],args[1],args[2]);
    case 4:    
      args=get_args(cdr(expr),prim_to_fun(curFun.val.fun),cur_env);
      if(!args){goto ERROR;}
      return F_CALL(curFun).f4(args[0],args[1],args[2],args[3]);
    default:
      goto ERROR;
  }
 ERROR:
  return handle_error();
}
static inline sexp eval_progn(sexp expr, env *cur_env){
  sexp prog=XCDR(expr);
  sexp retval;
  while(CONSP(prog)){
    retval = eval(XCAR(prog),cur_env);
    prog=XCDR(prog);
  }
  return retval;
}
static inline sexp eval_prog1(sexp expr, env *cur_env){
  sexp prog=XCDR(expr);
  sexp retval;
  retval = eval(XCAR(prog),cur_env);
  while(CONSP(prog)){
    eval(XCAR(prog),cur_env);
    prog=XCDR(prog);
  }
  return retval;
}
sexp eval_lambda(sexp expr,env *cur_env){
  //for now assume expr is a sexp of the form
  //(lambda (args ...) (body ...))
  sexp args,body;
  //  PRINT_MSG(print(cdr(expr)));
  //PRINT_MSG(tag_name(cadr(expr).tag));
  local_env *closure=xmalloc(sizeof(local_env));
  *closure=(local_env){.enclosing = cur_env,.head=cadr(expr).val.lenv};
  int numargs=cadr(expr).len;
  body=caddr(expr);
  /*This is now delt with in the parser
    while(CONSP(args)){
    if(!SYMBOLP(car(args))){
      format_error_str("argument %s is not a symbol",print(car(args)));
      handle_error();
    }
    cur_arg->name=car(args).val.var->name;
    cur_arg->val=UNBOUND;
    cur_arg->next=xmalloc(sizeof(local_symbol));
    args=cdr(args);
    numargs++;
    }*/
  lambda *retval=xmalloc(sizeof(lambda));
  retval->env=closure;
  retval->minargs=retval->maxargs=numargs;//need to fix to allow optional args
  retval->body=body;
  return (sexp){.tag=_lam,.val={.lam = retval}}; /*  */
}
static inline sexp eval_def(sexp expr,env *cur_env){
  //should i go with the lisp standard of define only assigning
  //to a value once or not?
  symref newSym;
  //PRINT_FMT("%s",typeName(cadr(expr)));  
  newSym=getSym(cur_env,cadr(expr).val.var->name);
  sexp symVal=eval(caddr(expr),cur_env);
  if(!newSym){
    newSym=xmalloc(symbolSize(cur_env));
    newSym->name=(cadr(expr).val.var->name);
    newSym->val=symVal;
    newSym=addSym(cur_env,newSym);
  } else {
    newSym->val=symVal;
  }
  return (sexp){.tag = _sym,.val={.var = newSym}};
}
static inline sexp eval_defun(sexp expr,env *cur_env){
  sexp temp_lambda;
  //expr=(defun sym arglist body)
  temp_lambda.val.cons=xmalloc(sizeof(cons));
  temp_lambda.tag=_cons;
  XCAR(temp_lambda)=(sexp){.tag=_special,.val={.special=_lambda}};
  XCDR(temp_lambda)=cddr(expr);
  //temp_lambda = (lambda arglist body)
  //temp_lambda=eval(temp_lambda,cur_env);
  XCDDR(expr).val.cons=xmalloc(sizeof(cons));
  XCADDR(expr)=temp_lambda;
  XCDDDR(expr)=NIL;
  //expr = (defun sym temp_lambda)
  XCAR(expr)=(sexp){.tag=_special,.val={.special=_def}};
  //expr = (def sym temp_lambda)
  return eval(expr,cur_env);
}
static inline sexp eval_if(sexp expr,env *cur_env){
  //car  cadr    caddr   car(cdddr)
  //(if .(cond . (then . (else .()))))
  if(cdr(cdddr(expr)).tag != _nil){
    CORD_sprintf(&error_str,"excess arguments to if expression\n");
    return handle_error();
  } else {
    register sexp cond = eval(cadr(expr),cur_env);
    return (isTrue(cond) ? eval(caddr(expr),cur_env) 
            : eval(cadddr(expr),cur_env));
  }
}
static inline sexp eval_while(sexp expr,env *cur_env){
  //(while cond body)
  expr=cdr(expr);
  register sexp cond=car(expr);
  register sexp body=cadr(expr);
  register sexp retval=NIL;
  while(isTrue(eval(cond,cur_env))){
    retval=eval(body,cur_env);
  }
  return retval;
}
static inline sexp call_lambda(sexp expr,env *cur_env){
  lambda *cur_fun = XCAR(expr).val.var->val.val.lam;
  assert(cur_fun !=0);
  int i=0;
  long maxargs=cur_fun->maxargs;//maybe short?
  long minargs=cur_fun->minargs;//""        ""
  sexp args=cdr(expr);
  local_symref cur_param=cur_fun->env->head;
  assert(cur_param != 0);
  assert(cur_param == cur_fun->env->head);
  //PRINT_FMT("cur_param = %#0x",cur_param);
  while((i<minargs || CONSP(args)) && cur_param != 0){
    if(!CONSP(args)){
      format_error_str("not enough arguments passed to function");
      return handle_error();
    }
    //PRINT_MSG(print(args));
    cur_param->val=NIL;
    cur_param->val=eval(XCAR(args),cur_env);//set curent formal parameter
    i++;
    cur_param=cur_param->next;
    args=XCDR(args);//update values
  }
  cur_param=cur_fun->env->head;
  env parameters={.enclosing=cur_fun->env->enclosing,
               .head={.local=cur_param},.tag=_local};
  sexp retval=eval(cur_fun->body,&parameters);
  //PRINT_MSG(print(retval));
  //clear parameters(I think this is necessary)
  while(cur_param!=0){
    cur_param->val=NIL;
    cur_param=cur_param->next;
  }
  return retval;
}
static inline sexp eval_let(sexp expr,env *cur_env){
  /*syntax (let ((var def)+)(body ...))*/
  sexp vars=XCDR(expr);
  if(!CONSP(vars)){
    return error_sexp("empty let expression");
  }
  local_symref cur_var=xmalloc(sizeof(local_symbol));
  local_symref last_var=cur_var;
  env *scope = xmalloc(sizeof(env));
  *scope=(env){.enclosing = cur_env,.head = {.local = cur_var},.tag=_local};
  while(CONSP(XCAR(vars))){//for each cons cell in the let expression
    //take the name from the car(assuming it's a symbol)
    cur_var->name=XCAAR(vars).val.var->name;//I think...
    //set the value to eval(cdr) 
    cur_var->val=eval(XCDAR(vars),cur_env);
    //allocate next variable
    cur_var->next=xmalloc(sizeof(local_symbol));
    last_var=cur_var;
    cur_var=cur_var->next;
    vars=XCDR(vars);
    if(!CONSP(vars)){return error_sexp("malformed let expression");}
  }
  last_var->next=0;
  return eval(cddr(expr),scope);
  return error_sexp("let unimplemented");
}
static inline sexp eval_do(sexp expr,env *cur_env){
  /*syntax (do (var init step end) body)*/
  //for now body must be a single sexp(ie use explict progn)
  expr=cdr(expr);//we don't need the do anymore
  sexp loop_params=car(expr);//(var init step end)
  if(!CONSP(loop_params) || !CONSP(XCDR(loop_params)) ||
     !CONSP(XCDDR(loop_params)) || !CONSP(XCDDDR(loop_params))){
    return error_sexp("incomplete paramenter list for do expression");
  }
  //create environment for loop
  local_symref loop_var=alloca(sizeof(local_symbol));
  *loop_var=*(local_symref)XCAR(loop_params).val.var;
  loop_var->next=NULL;
  sexp retval=NIL;
  env *loop_scope=alloca(sizeof(env));
  *loop_scope=(env){.enclosing=cur_env,.head={.local=loop_var},.tag=_local};
  //initialize loop var
  eval(XCADR(loop_params),loop_scope);
  loop_params=XCDDR(loop_params);//(step end)
  while(isTrue(eval(XCADR(loop_params),loop_scope))){//test loop end condition
    retval=eval(cadr(expr),loop_scope);//execute loop body
    //run loop step function
    eval(XCAR(loop_params),loop_scope);//I'm not sure if this will work
  }
  return retval;
}
static sexp eval_and(sexp expr,env *cur_env){
  //(and sexp*) if all sexp's are true return value of last sexp
  //otherwise return false
  expr=cdr(expr);//discard and
  sexp retval=NIL;
  while(CONSP(expr)){
    retval=eval(XCAR(expr),cur_env);
    if(!(isTrue(retval))){
      return LISP_FALSE;
    } else {
      expr=XCDR(expr);
    }
  }
  return retval;    
}
static sexp eval_or(sexp expr,env *cur_env){
  expr=cdr(expr);
  sexp retval=NIL;
  while(CONSP(expr)){
    retval=eval(XCAR(expr),cur_env);
    if(isTrue(retval)){
      return retval;
    } else {
      expr=XCDR(expr);
    }
  }
  return LISP_FALSE;
}
static sexp eval_dolist(sexp expr,env *cur_env){
  //(dolist (var list) body)
  expr=cdr(expr);
  sexp loop_params=car(expr);//(var list)
  if(!CONSP(loop_params) || !CONSP(XCDR(loop_params))){
    return error_sexp("incomplete paramenter list for dolist expression");
  }
  //create environment for loop
  local_symref loop_var=alloca(sizeof(local_symbol));
  sexp loop_list=XCADAR(expr);
  *loop_var=*(local_symref)XCAR(loop_params).val.var;
  loop_var->next=NULL;
  sexp retval=NIL;
  env *loop_scope=alloca(sizeof(env));
  *loop_scope=(env){.enclosing=cur_env,.head={.local=loop_var},.tag=_local};
  while(CONSP(loop_list)){
    loop_var->val=unsafe_pop(loop_list);
    retval=eval(cdr(expr),loop_scope);//execute loop body
  }
  return retval;
}
