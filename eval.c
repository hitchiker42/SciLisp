/*****************************************************************
* Copyright (C) 2013 Tucker DiNapoli                            *
* SciLisp is Licensed under the GNU General Public License V3   *
****************************************************************/
#include "common.h"
#include "cons.h"
symref tempsym;
jmp_buf ERROR;
static sexp eval_special(sexp expr);
static sexp call_function(sexp curFun,sexp expr);
sexp handle_error(void){
  CORD_printf(error_str);
  return NIL;
}
sexp eval(sexp expr){
  switch(expr.tag){
    case _cons:
      if(SYMBOLP(car(expr))){
        sexp curFun=car(expr).val.var->val;
        if(!FUNP(curFun)){
          CORD_fprintf(stderr,"tag = %s\n",typeName(curFun));
          CORD_sprintf(&error_str,"%r is not a function or special form",
                       print(curFun));
          goto ERROR;
        } else {
          return call_function(curFun,expr);
        }
      } else if(SPECP(car(expr))){
        return eval_special(expr);
      } else {
        CORD_sprintf(&error_str,"car of unquoted list is not a function or special form");
        goto ERROR;
      }
    case _sym:
      getSym(expr.val.var->name,tempsym);
      if(tempsym){
        return eval(tempsym->val);
      } else {
        CORD_sprintf(&error_str,"undefined variable %r used",expr.val.var->name);
        goto ERROR;
      }
  default:
    return expr;
  }
  ERROR:
  return handle_error();
}
static inline sexp call_function(sexp curFun,sexp expr){
  int i;
  sexp cur_arg;
#define getArgs(numargs)                                       \
  cur_arg=cdr(expr);                                                  \
  sexp args##numargs[numargs];                                          \
    for(i=0;i<numargs;i++){                                             \
      if(!CONSP(cur_arg)){                                              \
        CORD_sprintf(&error_str,"Too few Arguments given to %r",        \
                     FLNAME(curFun));                                   \
        goto ERROR;                                                     \
      } else {                                                          \
        args##numargs[i]=eval(car(cur_arg));                   \
          cur_arg=cdr(cur_arg);                                         \
      }                                                                 \
    }
  switch (FMAX_ARGS(curFun)){
    case 0:
      if(!NILP(cdr(expr))){
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
      sexp args=eval(cadr(expr));
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
static inline sexp eval_special(sexp expr){
  //this is an internal only inline function, ie this function itself
  //won't be in the generated code, it's just used to git the source code
  //a bit more clarity
  //this is always called on a cons, no need to check
  sexp special_sexp=car(expr);
  symref newSym;
  switch(special_sexp.val.special){
    //for now focus on def,defun,if and do
    case _def:
      //should I go with the lisp standard of define only assigning
      //to a value once or not?
      PRINT_FMT("%s",typeName(cadr(expr)));
      getSym(cadr(expr).val.var->name,newSym);
      if(!newSym){
        CORD_sprintf(&error_str,"%s is not a symbol",print(cadr(expr)));
        goto ERROR;
      } else {
        sexp symVal=eval(caddr(expr));
        newSym->val=symVal;
        return (sexp){.tag = _sym,{.var = newSym}};
      }
    case _setq: 
      getSym(cadr(expr).val.var->name,newSym);
      if(!newSym){
        CORD_sprintf(&error_str,"%s is not a symbol",print(cadr(expr)));
        goto ERROR;
      } else {
        sexp symVal=eval(caddr(expr));
        newSym->val=symVal;
        HERE();
        return (sexp){.tag = _sym,{.var = newSym}};
      }
    case _lambda: return NIL;
    case _if: 
      //car  cadr    caddr   car(cdddr)
      //(if .(cond . (then . (else .()))))
      if(cdr(cdddr(expr)).tag != _nil){
        CORD_sprintf(&error_str,"excess arguments to if expression\n");
        goto ERROR;
      } else {
        register sexp cond = eval(cadr(expr));
        return (isTrue(cond) ? eval(caddr(expr)) : eval(car(cdddr(expr))));
      }
      
    case _do: return NIL;
    case _while:;
      register sexp cond=cadr(expr);
      register sexp body=caddr(expr);
      register sexp retval=NIL;
      while(isTrue(eval(cond))){
        retval=eval(body);
      }
      return retval;
  }
 ERROR:
  return handle_error();
}
