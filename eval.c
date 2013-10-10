/*****************************************************************
* Copyright (C) 2013 Tucker DiNapoli                            *
* SciLisp is Licensed under the GNU General Public License V3   *
****************************************************************/
#include "common.h"
#include "cons.h"
jmp_buf ERROR;
//static fuctions which are really just part of eval, but broken
//up into seperate functions to modularise eval, and make it 
//easier to write and understand, should be self explanitory
static sexp eval_special(sexp expr,env cur_env);
static sexp call_builtin(sexp expr,env cur_env);
static sexp call_lambda(sexp expr,env cur_env);
static sexp eval_def(sexp expr,env cur_env);
static sexp eval_defun(sexp expr,env cur_env);
static sexp eval_if(sexp expr,env cur_env);
static sexp eval_while(sexp expr,env cur_env);
static sexp eval_lambda(sexp expr,env cur_env);
//standard error handling function
static sexp handle_error(void){
  CORD_fprintf(stderr,error_str);fputs("\n",stderr);
  return NIL;
}
//evaluate the lisp expression expr in the environment cur_env
sexp eval(sexp expr,env cur_env){
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
          CORD_fprintf(stderr,"tag = %s\n",typeName(curFun));
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

  default:
    return expr;
  }
  ERROR:
  return handle_error();
}
static inline sexp call_builtin(sexp expr,env cur_env){
  sexp curFun=car(expr).val.var->val;
  int i;
  sexp cur_arg;
#define getArgs(numargs)                                                \
  cur_arg=cdr(expr);                                                    \
  if(NILP(cur_arg)){goto ARGS_ERR ## numargs;}                         \
  sexp args##numargs[numargs];                                          \
    for(i=0;i<numargs;i++){                                             \
      if(!CONSP(cur_arg)){                                              \
        ARGS_ERR ## numargs:                                            \
          CORD_sprintf(&error_str,"Too few Arguments given to %r",      \
                     FLNAME(curFun));                                   \
        goto ERROR;                                                     \
      } else {                                                          \
        args##numargs[i]=eval(XCAR(cur_arg),cur_env);                    \
          cur_arg=XCDR(cur_arg);                                         \
      }                                                                 \
    }
  switch (FMAX_ARGS(curFun)){
    case 0:
      if(!NILP(XCDR(expr))){
        CORD_sprintf(&error_str,"Arguments given to %r which takes no arguments",
                     FLNAME(curFun));
      } else {
        return F_CALL(curFun).f0();
      }
  case 1:
    if(!NILP(cddr(expr))){
      CORD_sprintf(&error_str,"Excess Arguments given to %r",
                   FLNAME(curFun));
      goto ERROR;
    } else {
      sexp args=eval(cadr(expr),cur_env);
      return F_CALL(curFun).f1(args);
    }
  case 2:
    getArgs(2);
    sexp retval=F_CALL(curFun).f2(args2[0],args2[1]);
    return retval;
  case 3:
    getArgs(3);
    return F_CALL(curFun).f3(args3[0],args3[1],args3[2]);
  case 4:
    getArgs(4);
    return F_CALL(curFun).f4(args4[0],args4[1],args4[2],args4[3]);
 }
 ERROR:
  return handle_error();
#undef getArgs
}
static inline sexp eval_special(sexp expr,env cur_env){
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
        //NEED TO MAKE GENERIC
        newSym=xmalloc(sizeof(global_symbol));
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
    case _do: return NIL;
    case _while:
      return eval_while(expr,cur_env);
    case _defun:
      return eval_defun(expr,cur_env);
  }
 error:
  return handle_error();
}
sexp eval_lambda(sexp expr,env cur_env){
  //for now assume expr is a sexp of the form
  //(lambda (args ...) (body ...))
  sexp args,body;
  env* cur_env_loc;
  //  PRINT_MSG(print(cdr(expr)));
  if(cur_env.enclosing != 0){
    cur_env_loc=xmalloc(sizeof(env));
    *cur_env_loc=cur_env;
  } else {
    cur_env_loc=&topLevelEnv;
  }
  PRINT_MSG(tag_name(cadr(expr).tag));
  local_env closure={.enclosing = cur_env_loc,.head=cadr(expr).val.lenv};
  int numargs=cadr(expr).len;
  body=caddr(expr);
  /*  while(CONSP(args)){
    if(!SYMBOLP(car(args))){
      format_error_str("argument %s is not a symbol",print(car(args)));
      handle_error();
    }
    HERE();
    cur_arg->name=car(args).val.var->name;
    HERE();
    cur_arg->val=UNBOUND;
    HERE();
    cur_arg->next=xmalloc(sizeof(local_symbol));
    HERE();
    args=cdr(args);
    numargs++;
    HERE();
    }*/
  lambda *retval=xmalloc(sizeof(lambda));
  retval->env=closure;
  retval->minargs=retval->maxargs=numargs;
  retval->body=body;
  return (sexp){.tag=_lam,.val={.lam = retval}}; /*  */
}
static inline sexp eval_def(sexp expr,env cur_env){
  //should i go with the lisp standard of define only assigning
  //to a value once or not?
  symref newSym;
  //PRINT_FMT("%s",typeName(cadr(expr)));  
  newSym=getSym(cur_env,cadr(expr).val.var->name);

  if(!newSym){
    newSym=xmalloc(sizeof(global_symbol));
    newSym->name=(cadr(expr).val.var->name);
    newSym=addSym(cur_env,newSym);
  }
  sexp symVal=eval(caddr(expr),cur_env);
  newSym->val=symVal;
  return (sexp){.tag = _sym,.val={.var = newSym}};
}
static inline sexp eval_defun(sexp expr,env cur_env){
  sexp temp_lambda;
  //expr=(defun sym arglist body)
  temp_lambda.val.cons=xmalloc(sizeof(cons));
  temp_lambda.tag=_cons;
  XCAR(temp_lambda)=(sexp){.tag=_special,.val={.special=_lambda}};
  XCDR(temp_lambda)=cddr(expr);
  //temp_lambda = (lambda arglist body)
  //temp_lambda=eval(temp_lambda,cur_env);
  XCDDR(expr)=temp_lambda;
  //expr = (defun sym temp_lambda)
  XCAR(expr)=(sexp){.tag=_special,.val={.special=_def}};
  //expr = (def sym temp_lambda)
  return eval(expr,cur_env);
}
static inline sexp eval_if(sexp expr,env cur_env){
  //car  cadr    caddr   car(cdddr)
  //(if .(cond . (then . (else .()))))
  if(cdr(cdddr(expr)).tag != _nil){
    CORD_sprintf(&error_str,"excess arguments to if expression\n");
    handle_error;
  } else {
    register sexp cond = eval(cadr(expr),cur_env);
    return (isTrue(cond) ? eval(caddr(expr),cur_env) 
            : eval(car(cdddr(expr)),cur_env));
  }
}
static inline sexp eval_while(sexp expr,env cur_env){
  register sexp cond=cadr(expr);
  register sexp body=caddr(expr);
  register sexp retval=NIL;
  while(isTrue(eval(cond,cur_env))){
    retval=eval(body,cur_env);
  }
  return retval;
}
static inline sexp call_lambda(sexp expr,env cur_env){
  lambda *cur_fun = XCAR(expr).val.var->val.val.lam;
  assert(cur_fun !=0);
  local_symref cur_param=cur_fun->env.head;
  assert(cur_param != 0);
  assert(cur_param == cur_fun->env.head);
  PRINT_FMT("cur_param = %#0x",cur_param);
  sexp args=cdr(expr);
  int minargs=cur_fun->minargs;
  int maxargs=cur_fun->maxargs;
  int i=0;
  while((i<minargs || CONSP(args)) && cur_param != 0){
    if(!CONSP(args)){
      format_error_str("not enough arguments passed to function");
      handle_error();
    }
    PRINT_MSG(print(args));
    cur_param->val=NIL;
    cur_param->val=eval(XCAR(args),cur_env);//set curent formal parameter
    i++;
    cur_param=cur_param->next;
    args=XCDR(args);//update values
  }
  cur_param=cur_fun->env.head;
  env closure={.enclosing=cur_fun->env.enclosing,
               .head={.local=cur_param},.tag=_local};
  sexp retval=eval(cur_fun->body,closure);
  PRINT_MSG(print(retval));
  //clear parameters(I think this is necessary)
  while(cur_param!=0){
    cur_param->val=NIL;
    cur_param=cur_param->next;
  }
  return retval;
}
