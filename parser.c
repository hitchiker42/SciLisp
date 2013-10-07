/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
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
symref tmpsym;//for reading from hash table
TOKEN yytag;//current token
jmp_buf ERROR;//location of error handling function
sexp parse_atom();
sexp parse_cons();
sexp parse_sexp();
sexp yyparse(FILE* input){
  if(setjmp(ERROR)){
    PRINT_MSG("Jumped to error");
    if(error_str){
      CORD_fprintf(stderr,error_str);
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
    return ast;
  }
}
sexp parse_cons(){
  //sexp* result=xmalloc(sizeof(sexp));
  nextTok();
  sexp result;
  result.tag=_cons;
  result.val.cons=xmalloc(sizeof(cons));
  cons* cons_pos=result.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
  //  sexp cons_pos=result.val.cons->cdr;
  //at this point there's no difference between any macro, special form or function
  if (yytag == TOK_SPECIAL){
    result.val.cons->car=(sexp){_special,{.special = yylval->val.special}};
  } else if(yytag==TOK_ID){
    //PRINT_FMT("found id %s",yylval->val.cord);
    getSym(yylval->val.cord,tmpsym);
    if(tmpsym){
      //PRINT_FMT("Found prim %s",tmpsym->name);
      result.val.cons->car=(sexp){_sym,{.var =tmpsym}};
    } else {
    tmpsym=xmalloc(sizeof(symbol));
    tmpsym->name=yylval->val.string;
    tmpsym->val=UNBOUND;
    result.val.cons->car=(sexp){_sym,{.var =tmpsym}};
    }
  } else {
    sexp retval;
    retval.tag=_list;
    cons* cur_loc=retval.val.cons=xmalloc(sizeof(cons));
    cons* prev_loc=cur_loc;
    while(nextTok() != TOK_RPAREN){
      cur_loc->car=parse_sexp();
      cur_loc->cdr.val.cons=xmalloc(sizeof(cons));
      prev_loc=cur_loc;
      cur_loc=cur_loc->cdr.val.cons;
    }
    prev_loc->cdr=NIL;
    return retval;
  }
  //implicit progn basically, keep parsing tokens until we get a close parens
  sexp temp;
  cons* old_pos;
  while((nextTok())!=TOK_RPAREN){
    //PRINT_FMT("%d",yytag);
    if(yytag == TOK_LPAREN){
      cons_pos->car=parse_cons();
    } else {
      temp=parse_atom();
      cons_pos->car.val=temp.val;
      cons_pos->car.tag=temp.tag;
    }
    cons_pos->cdr.val.cons=xmalloc(sizeof(cons));
    old_pos=cons_pos;
    cons_pos=cons_pos->cdr.val.cons;
  }
  old_pos->cdr=NIL;
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
        sexp retval;
        retval.tag=_list;
        cons* cur_loc=retval.val.cons=xmalloc(sizeof(cons));
        cons* prev_loc=cur_loc;
        while(nextTok() != TOK_RPAREN){
          cur_loc->car=parse_sexp();
          cur_loc->cdr.val.cons=xmalloc(sizeof(cons));
          prev_loc=cur_loc;
          cur_loc=cur_loc->cdr.val.cons;
        }
        prev_loc->cdr=NIL;
        return retval;
      } else{
        return *yylval;//return anything else unevaluated
      }
    case TOK_ID:
      //need to change this
      tmpsym=NULL;
      getSym(yylval->val.string,tmpsym);
      if(tmpsym){
        return (sexp){_sym,{.var = tmpsym}};
      } else {
        tmpsym=xmalloc(sizeof(symbol));
        tmpsym->name=yylval->val.string;
        tmpsym->val=UNBOUND;
        addSym(tmpsym);
        return (sexp){_sym,{.var = tmpsym}};
      }
    default:
      CORD_sprintf(&error_str,"Error, expected literal atom recieved %r\n",print(*yylval));
      longjmp(ERROR,-1);
  }
}
sexp parse_sexp(){
  if(yytag == TOK_LPAREN){return parse_cons();}
  else{return parse_atom();}
}
sexp lispRead(CORD code){
  char* stringBuf = CORD_to_char_star(code);
  FILE* stringStream = fmemopen(stringBuf,CORD_len(code),"r");
  yyin=stringStream;
  nextTok();
  return parse_sexp();
}
