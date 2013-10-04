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
sexp internal_eval(sexp expr){
  HERE();
  switch(expr.tag){    
    case _cons:
      if(SYMBOLP(car(expr))){
        sexp curFun=car(expr).val.var->val;
        if(!FUNP(curFun)){
          CORD_fprintf(stderr,"tag = %s\n",toString_tag(curFun));
          CORD_sprintf(&error_str,"%r is not a function or special form",
                       print(curFun));
          goto ERROR;
        } else {
          call_function(curFun,expr);
        }
      } else if(SPECP(car(expr))){
        return eval_special(expr);
      }
    case _sym:
      getSym(expr.val.var->name,tempsym);
      if(tempsym){
        return internal_eval(tempsym->val);
      } else {
        CORD_sprintf(&error_str,"undefined variable %r used",expr.val.var->name);
        goto ERROR;
      }
  default:
    return expr;
  }
  ERROR:
  longjmp(ERROR,-1);
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
        args##numargs[i]=internal_eval(car(cur_arg));                   \
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
    if(!NILP(cddr(curFun))){
      CORD_sprintf(&error_str,"Excess Arguments given to %r",
                   FLNAME(curFun));
      goto ERROR;
    } else {
      sexp args=internal_eval(cdar(curFun));
      return F_CALL(curFun).f1(args);
    }
  case 2:              
    getArgs(2);
    return F_CALL(curFun).f2(args2[0],args2[1]);
  case 3:
    getArgs(3);
    return F_CALL(curFun).f3(args3[0],args3[1],args3[2]);
  case 4:
    getArgs(4);
    return F_CALL(curFun).f4(args4[0],args4[1],args4[2],args4[3]);
 }
 ERROR:
 longjmp(ERROR,-1);
#undef getArgs
}
static inline sexp eval_special(sexp expr){
  //this is an internal only inline function, ie this function itself
  //won't be in the generated code, it's just used to git the source code
  //a bit more clarity
  //this is always called on a cons, no need to check
  sexp special_sexp=car(expr);
  sexp newSym;
  switch(special_sexp.val.special){
    //for now focus on def,defun,if and do
    case _def:
      //should I go with the lisp standard of define only assigning
      //to a value once or not?
      newSym=cdar(expr);
      if(!SYMBOLP(newSym)){
        CORD_sprintf(&error_str,"%s is not a symbol",print(cdar(expr)));
      } else {
        sexp symVal=internal_eval(cddr(expr));
        newSym.val.var->val=symVal;
        return newSym;
      }
    case _defun: return NIL;
    case _if: return NIL;
    case _do: return NIL;
  }
}
/*    switch (yytag){
      case TOK_ID:        
        printf("Lexed id %s\n",yylval->val.string);
        getSym(yylval->val.string,tmpsym);
        if(tmpsym){
          printf("Symbol %s found in symbol table\n",tmpsym->name);
          sexp tempsexp=tmpsym->val;
          if(tempsexp.tag == _fun){
            yytag=yylex();
            double x,y;
            if(yytag==TOK_REAL){
              x=yylval->val.real64;
            }
            else if(yytag==TOK_INT){
              x=(long)yylval->val.int64;
            }
            else{continue;}
            yytag=yylex();
            if(yytag==TOK_REAL){
            } else if(yytag==TOK_INT){
              y=(long)yylval->val.int64;
            }
            else{continue;}
            double(*fp)(double,double)=tempsexp.val.fun;
            double result=fp(x,y);
            printf("%f %s %f = %f\n",x,tmpsym->name,y,result);
          } else if(tempsexp.tag==_double){
            printf("Symbol value is %f\n",tempsexp.val.real64);
          } else if(tempsexp.tag == _long){
            printf("Symbol value is %ld\n",tempsexp.val.int64);
          } else {
          printf("Symbol value is nil\n");*/
