/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
symref tempsym;
jmp_buf ERROR;
char* error_str;
static sexp eval_special(sexp expr);
sexp internal_eval(sexp expr){
  switch(expr.tag){    
    case _cons:
      if(SYMBOLP(car(expr))){
        sexp curFun=car(expr);
        if(!curFun.val.raw_fun){
          longjmp(ERROR,-1);
        }
        //assume maximu of a binary function, needs to be fixed asap
        //but if it gets me a vaguely working interpreter so be it
        //(name . (arg1 .(arg2 . ())))
        sexp arg1,arg2;
        if(CONSP(cdr(expr))){
          arg1=internal_eval(car(cdr(expr)));
        } else {}
        if (CONSP(cdr(cdr(expr)))){
          arg2=internal_eval(car(cdr(cdr(expr))));
        } else {}
        HERE();
        double(*fp)(double,double)=curFun.val.raw_fun;
        HERE();
        double retval=fp(getDoubleVal(arg1),getDoubleVal(arg2));
        HERE();
        return(sexp){_double,(data)(double)retval};
      } else if(SPECP(car(expr))){
        return eval_special(expr);
      }
  default:
    return expr;
  }
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
        asprintf(&error_str,"%s is not a symbol",print(cdar(expr)));
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
            printf("Symbol value is nil\n");
          }
        } else {
          symref* newsym = xmalloc(sizeof(symref));
          newsym->name=yylval->val.string;
          yytag=yylex();
          if (yytag == TOK_REAL){
            printf("yylval->real64 = %f\n",yylval->val.real64);
            newsym->val=(sexp){_double,yylval->val.real64};
          } else if (yytag == TOK_INT){
            printf("yylval->int64 = %ld\n",yylval->val.int64);
            newsym->val.tag=_long;
            newsym->val.val.int64=(long)yylval->val.int64;
          } else {
            newsym->val=NIL;
          }
          addSym(newsym);
          printf("added Symbol %s to symbol table\n",newsym->name);
        }
        break;*/
