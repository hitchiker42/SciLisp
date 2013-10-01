#include "common.h"
#include "cons.h"
#include "lex.yy.h"
/*Memory is falling out of scope somewhere, need to find out where*/
#define nextTok() yytag=yylex()
#define CAR(cons_sexp) cons_sexp.val.cons->car
#define CDR(cons_sexp) cons_sexp.val.cons->cdr
//#define CAR car
//#define CDR cdr
sexp ast;//generated ast
cons* cur_pos;//pointer to current location in ast
symref* tmpsym;//for reading from hash table
char* error_str;//global error string
TOKEN yytag;//current token
jmp_buf ERROR;//location of error handling function
sexp parse_atom();
sexp parse_cons();
sexp yyparse(FILE* input){
  if(setjmp(ERROR)){
    PRINT_MSG("Jumped to error");
    if(error_str){
      fputs(error_str,stderr);
      free(error_str);
    }
    if(yylval){free(yylval);}
    return NIL;    
  } else{
    yyin=input;
    ast.tag=_cons;
    cons* cur_pos=ast.val.cons=xmalloc(sizeof(cons));
    cons* prev_pos;
    yylval=malloc(sizeof(sexp));
    while((nextTok()) != -1){
      if(yytag == TOK_LPAREN){
        nextTok();
        cur_pos->car=parse_cons();
        cur_pos->cdr.val.cons=xmalloc(sizeof(cons));
        cur_pos->cdr.tag=_cons;
        prev_pos=cur_pos;
        cur_pos=cur_pos->cdr.val.cons;
      } else {
        cur_pos->car=parse_atom();
        cur_pos->cdr.val.cons=xmalloc(sizeof(cons));
        cur_pos->cdr.tag=_cons;
        prev_pos=cur_pos;
        cur_pos=cur_pos->cdr.val.cons;
      }        
    }
    if(yylval){free(yylval);}
    prev_pos->cdr=NIL;
    PRINT_MSG(princ(ast));
    return ast;
  }
}
sexp parse_cons(){
  //sexp* result=xmalloc(sizeof(sexp)); 
  sexp result;
  result.tag=_cons;
  result.val.cons=xmalloc(sizeof(cons));
  cons* cons_pos=result.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
  //  sexp cons_pos=result.val.cons->cdr;
  //at this point there's no difference between any macro, special form or function
  if (yytag == TOK_SPECIAL){
    result.val.cons->car=*yylval;
  } else if(yytag==TOK_ID){
    getSym(yylval->val.string,tmpsym);
    if(tmpsym){
      PRINT_MSG("Found prim");
      result.val.cons->car=(sexp){_fun,(data)(symref*)tmpsym};
    } else {
    tmpsym=xmalloc(sizeof(symref));
    tmpsym->name=yylval->val.string;
    result.val.cons->car=(sexp){_fun,(data)(symref*)tmpsym};
    }
  } else {
    asprintf(&error_str,"Expecting a function or special form, got %s",princ(*yylval));
    longjmp(ERROR,-1);
  }
  //implicit progn basically, keep parsing tokens until we get a close parens
  sexp temp;
  while((nextTok())!=TOK_RPAREN){
    if(yytag == TOK_LPAREN){
      nextTok();
      cons_pos->car=parse_cons();
    } else {
      temp=parse_atom();
      cons_pos->car.val=temp.val;
      cons_pos->car.tag=temp.tag;
    }
    cons_pos->cdr.val.cons=xmalloc(sizeof(cons));
    cons_pos=cons_pos->cdr.val.cons;
  }
  cons_pos->cdr=NIL;
  return result;
}
sexp parse_atom(){
  sexp retval;
  switch(yytag){
    case TOK_EOF:
      my_abort("EOF found in the middle of input\n");
    case TOK_REAL:      
      //*yylval;
      return (sexp){_double,(data)(double)yylval->val.real64};
    case TOK_INT:
      return *yylval;
    case TOK_QUOTE:
      nextTok();
      //parse a literal list
      if(yytag==TOK_LPAREN){
        cons* cur_loc=xmalloc(sizeof(cons));
        cons* head=cur_loc;
        while(nextTok() != TOK_RPAREN){
          cur_loc->car=parse_atom();
          cur_loc->cdr.val.cons=xmalloc(sizeof(cons));
          cur_loc=cur_loc->cdr.val.cons;
        }
        cur_loc->cdr=NIL;
        sexp retval={_cons,(data)head};
        return retval;
      } else{
        return *yylval;//return anything else unevaluated
      }
    case TOK_ID:
      tmpsym=NULL;
      getSym(yylval->val.string,tmpsym);
      if(tmpsym){
        return tmpsym->val;
      } else {
        error_str="Unknown variable encountered";
        longjmp(ERROR,-1);
      }
    default:
      asprintf(&error_str,"Error, expected literal atom recieved %s\n",princ(*yylval));
      longjmp(ERROR,-1);
  }
}

  
     /*//uses a bucnch of global variables, bad form
sexp parse_sexp(){
  if(yytag==TOK_LPAREN){
    nextTok();
    switch(yytag){
      case TOK_DEF:
      nextTok();
      if(yytag != TOK_ID){error_str("Expected a symbol name after a define");goto ERROR;}
      else{
        symref* newsym=xmalloc(sizeof(symref));
        newsym->name=yylval->val.string;
        nextTok();
        newsym->val=parse_sexp();
        addSym(newsym);
        return newsym->val;
      }
   
 
      
          
    case TOK_ID://Aka a function call
  }
}        
sexp yyparse(FILE* input){
  yyin=input;
  yylval=malloc(sizeof(sexp));
  while((yytag=yylex())!=-1){
    if(yytag==TOK_LPAREN){parse_sexp();}
    else{parse_atom();}
    switch (yytag){
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
      case TOK_QUOTE:
        printf("Lexed a quote\n");
        break;
      default:
        printf("Lexed Unknown Token\n");
        break;;
    }
  }
  free(yylval);
  return ast;
 ERROR:
  fputs(stderr,error_str);
  free(yylval);
  return NIL;
}
*/
