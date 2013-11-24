 /*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/*ARGS Structure is modified by a funciton call this is bad, fix it*/
/*TODO: add quoting in such a way that I can assign a list
  to a variable*/
#include "common.h"
#include "cons.h"
jmp_buf error_buf;
static long lambda_counter=0;
sexp call_builtin(sexp expr,env *cur_env);
sexp call_lambda(sexp expr,env *cur_env);
//lisp special forms and builtin macros
//see the definitions for documentation
static sexp call_macro(sexp expr,env *cur_env);
static sexp eval_special(sexp expr,env *cur_env);
static sexp eval_def(sexp expr,env *cur_env);
static sexp eval_setq(sexp expr,env *cur_env);
static sexp eval_defun(sexp expr,env *cur_env);
static sexp eval_if(sexp expr,env *cur_env);
static sexp eval_while(sexp expr,env *cur_env);
static sexp eval_lambda(sexp expr,env *cur_env);
static sexp eval_progn(sexp expr,env *cur_env);
static sexp eval_prog1(sexp expr,env *cur_env);
static sexp eval_do(sexp expr,env *cur_env);
static sexp eval_dolist(sexp expr,env *cur_env);
static sexp eval_dotimes(sexp expr,env *cur_env);
static sexp eval_let(sexp expr,env *cur_env);
static sexp eval_and(sexp expr,env *cur_env);
static sexp eval_or(sexp expr,env *cur_env);
static sexp eval_defmacro(sexp expr,env *cur_env);
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
  if(expr.quoted){
    if(SYMBOLP(expr)){
      if(expr.val.var->val.quoted){
        expr.val.var->val.quoted-=1;
      }
    }
    expr.quoted-=1;
    return expr;
  }
  switch(expr.tag){
    //a cons cell must be a function call or a special form
    case _cons:
      if(SYMBOLP(car(expr))){
        symref funSym=getSym(cur_env,XCAR(expr).val.var->name);
        if(!funSym){
          format_error_str("undefined function %s",XCAR(expr).val.var->name);
          return handle_error();
        }
        sexp curFun=funSym->val;
        XCAR(expr).val.var=funSym;
        if(LAMBDAP(curFun)){
          return call_lambda(expr,cur_env);
        } else if (FUNP(curFun)){
          return call_builtin(expr,cur_env);
        } else if (MACROP(curFun)){
          return call_macro(expr,cur_env);
        } else {
          CORD_fprintf(stderr,"tag = %s, name = %s\n",typeName(curFun),
                       XCAR(expr).val.var->name);
          CORD_sprintf(&error_str,"%r is not a function or special form",
                       print(curFun));
          goto ERROR;
        }
      } else if(SPECP(car(expr))){
        return eval_special(expr,cur_env);
      } else {
        format_error_str("car of unquoted list is not a function or special form"
                         "\ncar is %s",print(car(expr)));
        goto ERROR;
      }
    case _sym:
      if(expr.val.var->val.quoted){
        expr.val.var->val.quoted=0;
        return expr;
      } else {
        tempsym = getSym(cur_env,expr.val.var->name);
        if(tempsym){
          return eval(tempsym->val,cur_env);
        } else {
        UNDEF_ERROR:
          CORD_sprintf(&error_str,"undefined variable %r used",expr.val.var->name);
          goto ERROR;
        }
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
  switch(special_sexp.val.special){
    //for now focus on def,defun,if and do
    case _def:
      return eval_def(expr,cur_env);
    case _setq:
      return eval_setq(expr,cur_env);
    case _lambda:
      return eval_lambda(expr,cur_env);
    case _if:
      return eval_if(expr,cur_env);
    case _do:
      return eval_do(expr,cur_env);
    case _dolist:
      return eval_dolist(expr,cur_env);
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
    case _defmacro:
      return eval_defmacro(expr,cur_env);
    case _dotimes:
      return eval_dotimes(expr,cur_env);
    default:
      goto error;
  }
 error:
  format_error_str("unknown special form");
  return handle_error();
}
static inline sexp eval_setq(sexp expr,env *cur_env){
  symref newSym;
  newSym = getSym(cur_env,cadr(expr).val.var->name);
  sexp symVal=eval(caddr(expr),cur_env);
  if(!newSym){
    newSym=xmalloc(sizeof(symbol_val));
    newSym->name=(cadr(expr).val.var->name);
    newSym=addSym(cur_env,newSym);
    newSym->val=symVal;
  } else {
    if(newSym->props.is_const){
      return error_sexp("cannot set a constant to a new value");
    }
    newSym->val=symVal;
  }
  return symVal;
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
static inline sexp eval_def(sexp expr,env *cur_env){
  //should i go with the lisp standard of define only assigning
  //to a value once or not?, for now, yes.
  symref newSym;
  PRINT_FMT("%s",typeName(cadr(expr)));
  newSym=getSym(cur_env,cadr(expr).val.var->name);
  sexp symVal=eval(caddr(expr),cur_env);
  if(!newSym){
    newSym=xmalloc(symbolSize(cur_env));
    newSym->name=(cadr(expr).val.var->name);
    newSym->val=symVal;
    addSym(cur_env,newSym);
  } //else {
  //    newSym->val=symVal;
  //  }
  return symref_sexp(newSym);
}
/*define a possibly recursive named function
 *syntax: (defun name (arglist) (body))*/
static inline sexp eval_defun(sexp expr,env *cur_env){

  expr=cdr(expr);
  sexp temp_lambda;
  symref fun_sym=getGlobalSym(car(expr).val.var->name);
  if(!fun_sym){
    fun_sym=xmalloc(symbolSize(cur_env));
    fun_sym->name=(XCAR(expr).val.var->name);
  }
  //eval lambda
  function *new_fun=xmalloc(sizeof(function));
  lambda *new_lam=xmalloc(sizeof(lambda));
  env* closure = xmalloc(sizeof(env));
  function_args *args=cadr(expr).val.funargs;
  CORD funname;
  *closure=*cur_env;
  new_lam->body=caddr(expr);
  new_lam->env=closure;
  funname=fun_sym->name;
  *new_fun=(function){.args=args,.lname=funname,.cname=funname,
                      .lam=new_lam,.type=_lambda_fun};

  fun_sym->val=function_sexp(new_fun);
  addGlobalSym(fun_sym);
  //test code
  //  symref test=getGlobalSym(car(expr).val.var->name);
  //  PRINT_MSG(print(symref_sexp(test)));
  return symref_sexp(fun_sym);
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
//can parallize this,(i.e the binding of variables bit)
/*create and bind a set of local variables in parallel
 *syntax (let ((var def)*)(body ...))*/
static inline sexp eval_let(sexp expr,env *cur_env){
  sexp vars=XCADR(expr);
  if(!CONSP(vars)){
    return error_sexp("empty let expression");
  }
  sexp cur_expr;
  local_symref cur_var=xmalloc(sizeof(local_symbol));
  local_symref last_var=cur_var;
  env *scope = xmalloc(sizeof(env));
  *scope=(env){.enclosing = cur_env,.head = {.local = cur_var},.tag=_local};
  //for each cons cell in the let expression
  while(CONSP(vars) && CONSP((cur_expr=XCAR(vars)))){
    //take the name from the car(assuming it's a symbol)
    cur_var->name=XCAR(cur_expr).val.var->name;//I think...
    PRINT_FMT("current let var is %s",cur_var->name);
    //set the value to eval(cdr)
    cur_var->val=eval(XCADR(cur_expr),cur_env);
    //allocate next variable
    cur_var->next=xmalloc(sizeof(local_symbol));
    last_var=cur_var;
    cur_var=cur_var->next;
    vars=XCDR(vars);
  }
  if(!NILP(vars)){
    return error_sexp("malformed let expression");
  }
  last_var->next=0;
  return eval(caddr(expr),scope);
}
static inline sexp eval_flet(sexp expr,env *cur_env){
  /*syntax (flet ((var (arglist) def)*)(body ...))*/
  sexp vars=XCADR(expr);
  if(!CONSP(vars)){
    return error_sexp("empty let expression");
  }
  sexp cur_expr;
  local_symref cur_var=xmalloc(sizeof(local_symbol));
  local_symref last_var=cur_var;
  env *scope = xmalloc(sizeof(env));
  *scope=(env){.enclosing = cur_env,.head = {.local = cur_var},.tag=_local};
  //for each cons cell in the let expression
  while(CONSP(vars) && CONSP((cur_expr=XCAR(vars)))){
    //take the name from the car(assuming it's a symbol)
    cur_var->name=XCAR(cur_expr).val.var->name;//I think...
    PRINT_FMT("current let var is %s",cur_var->name);
    //set the value to eval(cdr)
    cur_var->val=eval(XCADR(cur_expr),cur_env);
    //allocate next variable
    cur_var->next=xmalloc(sizeof(local_symbol));
    last_var=cur_var;
    cur_var=cur_var->next;
    vars=XCDR(vars);
  }
  if(!NILP(vars)){
    return error_sexp("malformed let expression");
  }
  last_var->next=0;
  return eval(caddr(expr),scope);
}
/* equivalant to a for loop in c
 * syntax (do (var init step end) body)
 * bind var to init and repeat body, setting var to step each iteration
 * until end returns true. return result of last iteration
 * expands to (do (var (setq var init) (setq var step) 
 * for now body must be a single sexp(ie use explict progn)*/
static inline sexp eval_do(sexp expr,env *cur_env){
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
  cons memory[3];
  sexp loop_step_expr=cons_sexp(memory);
  XCAR(loop_step_expr)=spec_sexp(_setq);
  XCDR(loop_step_expr)=cons_sexp(memory+1);
  XCADR(loop_step_expr)=symref_sexp((symref)loop_var);
  XCDDR(loop_step_expr)=cons_sexp(memory+2);
  XCADDR(loop_step_expr)=XCADR(loop_params);
  XCDDDR(loop_step_expr)=NIL;
  eval(loop_step_expr,loop_scope);
  XCADDR(loop_step_expr)=XCADDR(loop_params);
  loop_params=XCDDR(loop_params);//(step end)
  while(isTrue(eval(XCADR(loop_params),loop_scope))){//test loop end condition
    retval=eval(XCADR(expr),loop_scope);//execute loop body
    //run loop step function
    eval(loop_step_expr,loop_scope);//I'm not sure if this will work
  }
  return retval;
}
/*simplified version of do for the most common case
 *set a variable to 0 and increment it by 1 a specified number of times
 *evaluating the body each iteration
 *syntax (dotimes (var times) body)
 *expands to (do (var 0 (++ var) (< var times)) body)
 *for now body must be a single sexp(ie use explict progn)*/
static inline sexp eval_dotimes(sexp expr,env *cur_env){

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
    retval=eval(XCADR(expr),loop_scope);//execute loop body
    //run loop step function
    eval(XCAR(loop_params),loop_scope);//I'm not sure if this will work
  }
  return retval;
}
/*iterate over a list with a variable bound to each element of the list in turn
 *syntax: (dolist (var list) body)*/
static sexp eval_dolist(sexp expr,env *cur_env){
  expr=cdr(expr);
  sexp loop_params=car(expr);//(var list)
  if(!CONSP(loop_params) || !CONSP(XCDR(loop_params))){
    return error_sexp("incomplete paramenter list for dolist expression");
  }
  //create environment for loop
  local_symref loop_var=alloca(sizeof(local_symbol));
  *loop_var=*(local_symref)XCAR(loop_params).val.var;
  loop_var->next=NULL;
  sexp loop_list;//=eval(XCDR(loop_params),cur_env);
  if(LISTP(XCADR(loop_params))){
    HERE();
    loop_list=XCADR(loop_params);
  } else {
    loop_list=eval(XCADR(loop_params),cur_env);
  }
  sexp retval=NIL;
  env *loop_scope=xmalloc(sizeof(env));
  *loop_scope=(env){.enclosing=cur_env,.head={.local=loop_var},.tag=_local};
  while(CONSP(loop_list)){
    loop_var->val=XCAR(loop_list);
    loop_list=XCDR(loop_list);
    retval=eval(XCADR(expr),loop_scope);//execute loop body
  }
  return retval;
}
/* evaluate a list of sexp's, if all are true return value of last sexp
 * otherwise return false
 * syntax: (and sexp*) */
static sexp eval_and(sexp expr,env *cur_env){

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
/* evaluate a list of sexp's, return the first value that evalueates to true
 * if all values are false return false
 * syntax: (or sexp*) */
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
/*internal procedure to bind function arguments to parameter names*/
#define setArg()                                        \
  args->args[j++].val=eval(XCAR(arglist),cur_env);      \
  arglist=XCDR(arglist)
function_args* getFunctionArgs(sexp arglist,function_args* args,env* cur_env){
  /*  if(args.tag != _funargs){
    handle_error();
    }*/
  int i,j=0;
  for(i=0;i<args->num_req_args;i++){
    if(!(CONSP(arglist))){
      format_error_str("not enough args");
      handle_error();
    } else {
      setArg();
    }
  }
  for(i=0;i<args->num_opt_args;i++){
    if(!CONSP(arglist)){
      goto ARGS_END;
    } else {
      setArg();
    }
  }
  for(i=0;i<args->num_keyword_args;i++){
    format_error_str("keyword args unimplemented");
    handle_error();
  }
  if(args->has_rest_arg){    
    if(CONSP(arglist)){
      cons* prev_arg;
      cons* cur_arg=args->args[j].val.val.cons=xmalloc(sizeof(cons));
      args->args[j].val.tag=_list;
      int i=j;
      while(CONSP(arglist)){
        j++;
        cur_arg->car=eval(XCAR(arglist),cur_env);
        arglist=XCDR(arglist);
        cur_arg->cdr.val.cons=xmalloc(sizeof(cons));
        prev_arg=cur_arg;
        cur_arg=cur_arg->cdr.val.cons;
      }
      prev_arg->cdr=NIL;
      args->args[i].val.len=(j-i);
    }
  }
  if(CONSP(arglist)){
    format_error_str("excess arguments passed");
    handle_error();
  }
 ARGS_END:
  if(j<args->max_args){
    while(j<args->max_args){
      args->args[j++].val=NIL;
    }
  }
  return args;
}
/* internal procedure to call a builtin c function*/
sexp call_builtin(sexp expr,env *cur_env){
  sexp curFun=car(expr).val.var->val;
  function_args *args=curFun.val.fun->args;
  args->args=alloca(sizeof(symbol)*args->max_args);
  args=getFunctionArgs(cdr(expr),args,cur_env);
  if(!args){
    handle_error();
  }
  //print_msg(print(expr));
  int i;
  long numargs=args->max_args;
  switch (numargs){
    case 0:
      if(!NILP(XCDR(expr))){
        format_error_str
          ("Arguments given to %r which takes no arguments",FLNAME(curFun));
        return handle_error();
      } else {
        return (CALL_PRIM(curFun).f0());
      }
    case 1:
      return (CALL_PRIM(curFun).f1(args->args[0].val));
    case 2:
      return (CALL_PRIM(curFun).f2(args->args[0].val,args->args[1].val));
    case 3:
      return (CALL_PRIM(curFun).f3
              (args->args[0].val,args->args[1].val,args->args[2].val));
    case 4:
      return (CALL_PRIM(curFun).f4(args->args[0].val,args->args[1].val,
                                  args->args[2].val,args->args[3].val));
    case 5:
      return (CALL_PRIM(curFun).f5
        (args->args[0].val,args->args[1].val,
         args->args[2].val,args->args[3].val,args->args[4].val));
    case 6:
      return (CALL_PRIM(curFun).f6
              (args->args[0].val,args->args[1].val,args->args[2].val,
               args->args[3].val,args->args[4].val,args->args[5].val));
    case 7:
      return (CALL_PRIM(curFun).f7
        (args->args[0].val,args->args[1].val,args->args[2].val,
         args->args[3].val,args->args[4].val,args->args[5].val,
         args->args[6].val));
    default:
      goto ERROR;
  }
 ERROR:
  return handle_error();
}
/* internal procedure to call a lisp function*/
sexp call_lambda(sexp expr,env *cur_env){
  PRINT_FMT("calling %r",car(expr).val.var->name);
  sexp curFun=car(expr).val.var->val;
  lambda* curLambda=curFun.val.fun->lam;
  function_args *args=curFun.val.fun->args;
  symbol *save_defaults=args->args;
  args->args=xmalloc(sizeof(symbol)*args->max_args);
  memcpy(args->args,save_defaults,(sizeof(symbol)*args->max_args));
  args=getFunctionArgs(cdr(expr),args,cur_env);
  if(!args){
    handle_error();
  }
  env lambda_env=(env){.enclosing=curLambda->env,.head={.function=args},
                       .tag=_funArgs};
  sexp retval = eval(curLambda->body,&lambda_env);
  PRINT_FMT("return value: %r",print(retval));
  args->args=save_defaults;
  return retval;
}
/* generate a new unnamed function
 * syntax (lambda (arglist) (body))
 * function can be bound to variable and called, but can not be recursive
 * i.e (def fact (lambda (n) (if (< n 1) 1 (* n (fact (-- n))))))
 * would fail, one would instead do (defun fact (n) ...)*/
static sexp eval_lambda(sexp expr,env *cur_env){
  sexp body;
  function_args *args=cadr(expr).val.funargs;
  CORD lambdaName;
  CORD_sprintf(&lambdaName,"#<lambda%06d>",lambda_counter++);
  body=caddr(expr);
  function *retval=xmalloc(sizeof(function));
  lambda *newLambda=xmalloc(sizeof(lambda));
  env* closure = xmalloc(sizeof(env));
  *closure=*cur_env;
  newLambda->env=closure;
  newLambda->body=body;
  *retval=(function){.args=args,.lname=lambdaName,.cname=lambdaName,
                     .lam=newLambda,.type=_lambda_fun};
  return (sexp){.tag=_fun,.val={.fun=retval}};
}
/* define a lisp macro, what exactly a lisp macro is and how it works
 * is not something that would eaisly fit into a simple comment
 * syntax: (defmacro name (args) (body))
 * where body is usually `(body)*/
static sexp eval_defmacro(sexp expr,env *cur_env){
  if(!CONSP(expr) || !(CONSP(XCDR(expr))) ||
     !(CONSP(XCDDR(expr))) || !(CONSP(XCDDDR(expr)))){
    return error_sexp("malformed defmacro expression");
  }
  HERE();
  PRINT_MSG(print(expr));
  macro *mac=xmalloc(sizeof(macro));
  expr=XCDR(expr);
  sexp macro_sym=XCAR(expr);
  mac->lname=macro_sym.val.var->name;
  mac->args=XCADR(expr).val.funargs;
  mac->body=XCDDR(expr);
  PRINT_MSG(print(mac->body));
  macro_sym.val.var->val=macro_sexp(mac);
  addSym(cur_env,macro_sym.val.var);
  PRINT_MSG(print(funargs_sexp(mac->args)));
  return macro_sym;
}
static sexp quote_sexp(sexp expr){
  expr.quoted++;
  return expr;
}
/*internal procedure to call a macro, eventually calls macroexpand*/
static sexp call_macro(sexp expr,env *cur_env){
  //expr is typed checked before we call this
  sexp cur_macro=car(expr).val.var->val;
  macro *mac=cur_macro.val.mac;
  PRINT_MSG(print(mac->body));
  function_args *args=mac->args;
  symbol *save_defaults=args->args;
  args->args=alloca(sizeof(symbol)*args->max_args);
  memcpy(args->args,save_defaults,(sizeof(symbol)*args->max_args));
  //really lazy hack, because I don't want rewrite getFunctionArgs
  //to get args w/o evaluating them
  sexp cons_pointer=XCDR(expr);
  while(CONSP(cons_pointer)){
    quote_sexp(XCAR(cons_pointer));
    cons_pointer=XCDR(cons_pointer);
  }
  args=getFunctionArgs(cdr(expr),args,cur_env);
  if(!args){
    handle_error();
  }
  env macro_env={.enclosing=cur_env,.head={.function=args},.tag=_funArgs};
  HERE();
  sexp expanded_macro=lisp_macroexpand(cur_macro,&macro_env);
  HERE();
  PRINT_MSG(print(expanded_macro));
  return eval(expanded_macro,cur_env);
}
/*external procudure to call a generic function(generic in that it doesn't
 *matter if the function was defined in lisp or c*/
sexp lisp_funcall(sexp fun,env *cur_env){
  if(!FUNCTIONP(fun)){
    return error_sexp("argument to funcall not a function");
  }
  if(FUNP(fun)){
    return call_builtin(fun,cur_env);
  } else if (LAMBDAP(fun)){
    return call_lambda(fun,cur_env);
  } else {
    return error_sexp("funcall unknown function type");
  }
}
/*expand a macro into lisp code*/
sexp lisp_macroexpand(sexp cur_macro,env *cur_env){
  macro* mac=cur_macro.val.mac;
  sexp macro_body=mac->body;
  long argnum;
  //just expand the macro, return an sexp to eval
  //assume cur_macro->args->args is filled out
  if(!MACROP(cur_macro)){
    return error_sexp("cannot macroexpand something which is not a macro");
  }
  //if the macro body is not quoted/backquoted
  if(!(mac->body.quoted)){
    if(SYMBOLP(mac->body)){
      //need to double check lisp macro semantics for these
      if((argnum=isFunctionArg
          ((function_env*)cur_env,mac->body.val.var->name))){
        return cur_env->head.function->args[argnum].val;
      } else {
        return mac->body.val.var->val;
      }
    } else if (!(CONSP(mac->body))){
      return mac->body;
    } else {
      //FIXME: this won't work, I'll fix it later
      return mac->body;
    }
  } else if (!(mac->body.has_comma)) {
    mac->body.quoted--;
    return mac->body;//quoted, but not quasiquoted
  } else {
    //quasi quoted
    mac->body.quoted--;
    while(CONSP(macro_body)){
      HERE();
      PRINT_MSG(print(macro_body));
      if(XCAR(macro_body).has_comma){
        if(XCAR(macro_body).meta == _splice_list){
          //(... ,@list next ...) save next in current_cdr
          //(... (car list) ... (last list)) (next gets cut off)
          //(... (car list) ... (car (last list)) next)
          //macro_body is set to next
          sexp list_to_splice=XCAR(macro_body);
          XCAR(macro_body).has_comma=0;
          sexp current_cdr=XCDR(macro_body);
          XCAR(macro_body)=XCAR(list_to_splice);
          XCDR(last(macro_body))=current_cdr;
          macro_body=current_cdr;
        } else if(SYMBOLP(XCAR(macro_body))){
          if((argnum=isFunctionArg
              ((function_env*)cur_env,XCAR(macro_body).val.var->name))){
            XCAR(macro_body)=//syref_sexp//args[argnum].val is a symbol
              (cur_env->head.function->args[argnum].val);
          } else {
            //I'm not sure what to do here
          }
          XCAR(macro_body).has_comma=0;
          macro_body=XCDR(macro_body);
        } else {
          XCAR(macro_body)=eval(XCAR(macro_body),cur_env);//maybe??
        }
      } else {
        macro_body=XCDR(macro_body);
      }
    }
    //we've changed this in the loop above
    return mac->body;
  }
}
sexp lisp_apply(sexp fun,sexp args,sexp environment){
  if(!FUNCTIONP(fun)){
    return error_sexp("first argument to apply should be a funciton");
  } 
  if(NILP(environment)){
    environment=env_sexp(topLevelEnv);
  }
  if(!ENVP(environment)){
    return error_sexp("last argument to apply should be an environment or nil");
  }
  if(!CONSP(args)){
    cons* one_arg=alloca(sizeof(cons));
    one_arg->car=args;
    one_arg->cdr=NIL;
    args=cons_sexp(one_arg);
  }
  cons *funcall_code=alloca(sizeof(cons));
  funcall_code->car=fun;
  funcall_code->cdr=XCAR(args);
  return lisp_funcall(cons_sexp(funcall_code),environment.val.cur_env);
}
