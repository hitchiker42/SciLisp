#include "common.h"
#include "cons.h"
static sexp eval_special(sexp expr);
sexp internal_eval(sexp expr){
  switch(expr.tag){    
    case _cons:
      if(car(expr).tag == _fun){
        //assume maximu of a binary function, needs to be fixed asap
        //but if it gets me a vaguely working interpreter so be it
        //(name . (arg1 .(arg2 . ())))
        sexp arg1,arg2;
        if(cdr(expr).tag !=_nil){
          arg1=internal_eval(cdar(expr));
        } else {}
        if (cddr(expr).tag !=_nil){
          arg2=internal_eval(cddar(expr));
        } else {}
        double(*fp)(double,double)=symVal(car(expr)).fun;
        double retval=fp(getDoubleVal(arg1),getDoubleVal(arg2));
        return(sexp){_double,(data)(double)retval};
      } else if(car(expr).tag == _special){
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
  switch(special_sexp.val.special){
    //for now focus on def,defun,if and do
    case _def: return NIL;
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
