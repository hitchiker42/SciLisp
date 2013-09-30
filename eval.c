#include "common.h"
#include "cons.h"
sexp internal_eval(sexp expr){
  switch(expr.tag){
    case _cons:
      if(car(expr).tag == _sym){}        
    default:
      return expr;
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
        break;
      case TOK_REAL:
        printf("Lexed real %f\n",yylval->val.real64);
        break;
      case TOK_INT:
        printf("Lexed int %d\n",yylval->val.int64);       
      case TOK_LPAREN:
        puts("Lexed (\n");
        break;
      case TOK_RPAREN:
        puts("Lexed )\n");
        break;
      case TOK_TYPEINFO:
        printf("Lexed typename %s\n",yylval->val.string);
        break;
*/
