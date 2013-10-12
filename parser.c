 /*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include "lex.yy.h"
/*Memory is falling out of scope somewhere, need to find out where*/
#define nextTok() (yytag=yylex())
sexp ast;//generated ast
cons* cur_pos;//pointer to current location in ast
TOKEN yytag;//current token
jmp_buf ERROR;//location of error handling function
sexp parse_atom();
sexp parse_cons();
sexp parse_sexp();
static void handle_error() __attribute__((noreturn));
static void handle_error(){
  evalError=1;
  longjmp(ERROR,-1);
}
static inline sexp parse_list(){
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
static inline sexp parse_arg_list(){
  sexp retval;
  int i=0;
  retval.tag=_lenv;
  local_symref cur_sym=retval.val.lenv=xmalloc(sizeof(local_symbol));
  local_symref prev_sym=cur_sym;
  while(nextTok() != TOK_RPAREN){
    //PRINT_FMT("yytag = %d",yytag);
    if(yytag != TOK_ID){
      format_error_str("function arguments must be identifiers");
      handle_error();
    }
    cur_sym->name=yylval->val.cord;
    cur_sym->val=UNBOUND;
    cur_sym->next=xmalloc(sizeof(local_symbol));
    prev_sym=cur_sym;
    cur_sym=cur_sym->next;
    i++;
  };
  prev_sym->next=0;
  retval.len=i;
  return retval;
}
sexp yyparse(FILE* input){
  if(setjmp(ERROR)){
    PRINT_MSG("Jumped to error");
    evalError=1;
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
  handle_error();
}
sexp parse_cons(){
  //sexp* result=xmalloc(sizeof(sexp));
  nextTok();
  sexp result;
  symref tmpsym=0;
  result.tag=_cons;
  result.val.cons=xmalloc(sizeof(cons));
  cons* cons_pos=result.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
  //  sexp cons_pos=result.val.cons->cdr;
  //at this point there's no difference between any macro, special form or function
  if (yytag == TOK_SPECIAL){
    result.val.cons->car=(sexp){.tag=_special,.val={.special = yylval->val.special}};
  } else if(yytag==TOK_ID){
    //PRINT_FMT("found id %s",yylval->val.cord);
    tmpsym = (symref)getGlobalSym(yylval->val.cord);
    if(tmpsym){
      //PRINT_FMT("Found prim %s",tmpsym->name);
      result.val.cons->car=(sexp){.tag=_sym,.val={.var =tmpsym}};
    } else {
    tmpsym=xmalloc(sizeof(symbol));
    tmpsym->name=yylval->val.string;
    tmpsym->val=UNBOUND;
    result.val.cons->car=(sexp){.tag=_sym,.val={.var =tmpsym}};
    }
  } else if(yytag==TOK_LAMBDA){
    sexp retval;
    retval.val.cons=xmalloc(sizeof(cons));
    retval.tag=_cons;
    retval.val.cons->car=*yylval;
    sexp fake_retval=retval;
  TEST_ARG_LIST:if(nextTok() != TOK_LPAREN){
      if(retval.val.cons->car.val.special == _defun){
        retval.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
        fake_retval=XCDR(retval);*yylval;
        symref tempSym=xmalloc(sizeof(symbol));
        tempSym->name=yylval->val.string;
        tempSym->val=UNBOUND;
        XCAR(fake_retval)=(sexp){.tag=_sym,.val={.var=tempSym}};
        goto TEST_ARG_LIST;
      }  else {
      format_error_str("expected argument list following lambda or defun");
      handle_error();
      }
    }
    cons* temp;
    temp=fake_retval.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
    temp->car=parse_arg_list();
    //    PRINT_MSG(tag_name(temp->car.tag));
    temp->cdr.val.cons=xmalloc(sizeof(cons));
    temp=temp->cdr.val.cons;
    temp->cdr=NIL;
    HERE();
    if(nextTok() == TOK_LPAREN){
      temp->car=parse_list();
      temp->car.tag=_cons;
    } else {
      temp->car=parse_atom();
    }
    if(nextTok() != TOK_RPAREN){
      format_error_str("error, missing closing parentheses");
      handle_error();
    }
    //PRINT_MSG(print(cdr(retval)));
    return retval;
  } else {
    return parse_list();
  }
  //implicit progn basically, keep parsing tokens until we get a close parens
  sexp temp;
  cons* old_pos=result.val.cons;
  while((nextTok())!=TOK_RPAREN){
    //PRINT_FMT("%d",yytag);
    if(yytag == TOK_LPAREN){
      cons_pos->car=parse_cons();
    } else {
      temp=parse_atom();
      cons_pos->car=temp;
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
  symref tmpsym=0;
  switch(yytag){
    case TOK_EOF:
      format_error_str("EOF found in the middle of input\n");
      handle_error();
    case TOK_REAL:
      //*yylval;
      return (sexp){_double,.val={.real64=yylval->val.real64}};
    case TOK_INT:
      return *yylval;
    case TOK_QUOTE:
      nextTok();
      //parse a literal list
      if(yytag==TOK_LPAREN){
        return parse_list();
        /*        sexp retval;
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
        return retval;*/
      } else{
        return *yylval;//return anything else unevaluated
      }
    case TOK_ID:
      tmpsym=(symref)getGlobalSym(yylval->val.string);
      if(tmpsym){
        return (sexp){.tag=_sym,.val={.var = tmpsym}};
      } else { 
        tmpsym=xmalloc(sizeof(symbol));
        tmpsym->name=yylval->val.string;
        tmpsym->val=UNBOUND;
        return (sexp){.tag=_sym,.val={.var = tmpsym}};
      }
      /*parse arrays
        TODO: add per element checks(as of now I only check the first)
        TODO: allow expressions in arrays (eval them to a # before use)
      */
    case TOK_LBRACE:
      HERE();
      nextTok();
      sexp retval;
      int size=8,i=-1;
      data* arr=retval.val.array=xmalloc_atomic(size*sizeof(data));      
      retval.tag=_array;
      HERE();
      _tag arrType=yylval->tag;
      PRINT_MSG(tag_name(arrType));
      if (arrType != _double && arrType!=_long && arrType !=_char){
        HERE();
        CORD_sprintf(&error_str,
                     "Arrays of type %s are unimplemented\n",arrType);
        handle_error();
      }
      retval.meta=(arrType == _double? 1 :
                   (arrType == _long ? 2 :
                    //THIS DOESN'T WORK FOR MAKING UTF8 STRINGS
                    (arrType == _char? 3 : assert(0),0)));
      do{
        if(i++>=size){
          HERE();
          arr=retval.val.array=xrealloc(arr,(size*=2)*sizeof(data));
        }
        if(yytag !=TOK_REAL && yytag !=TOK_INT && yytag !=TOK_CHAR){
          handle_error();
        }
        switch (arrType){
          case _double:
            arr[i].real64=getDoubleVal(*yylval);break;
          case _long:
            arr[i].int64=yylval->val.int64;break;
          case _char:
            arr[i].utf8_char=yylval->val.utf8_char;break;
          default:
            my_abort("How'd you get here?");
        }
      } while(nextTok() != TOK_RBRACE);
      i++;//inc i so that i == len of array
      if(arrType == _char){
        if(i>=size){
          arr=retval.val.array=xrealloc(arr,(size+=1)*sizeof(data));
        }
        arr[i].utf8_char=L'\0';
      }
      retval.len=i;
      PRINT_FMT("len = %d",i);
      PRINT_MSG(print(retval));
      return retval;
    case TOK_STRING:
      return *yylval;
    case TOK_CHAR:
      return *yylval;
    case TOK_SPECIAL:
      return *yylval;//I dont' know how well this'll work
    default:
      CORD_sprintf(&error_str,"Error, expected literal atom recieved %r\n",print(*yylval));
      handle_error();
  }
}
sexp parse_sexp(){
  if(yytag == TOK_QUOTE){
    nextTok();
    sexp retval = parse_sexp();
    //retval.meta=MakeQuoted(retval.meta);
    return retval;
  }
  if(yytag == TOK_LPAREN){return parse_cons();}
  else{return parse_atom();}
}

sexp lispRead(CORD code) {
  PRINT_MSG(code);
  FILE* stringStream=tmpfile();
  CORD_fprintf(stringStream,code);
  fflush(stringStream);
  yyin=stringStream;
  nextTok();
  return parse_sexp();
}
