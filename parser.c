 /*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include "lex.yy.h"
#define nextTok() (yytag=yylex())
sexp ast;//generated ast
cons* cur_pos;//pointer to current location in ast
TOKEN yytag;//current token
jmp_buf ERROR;//location of error handling function
sexp parse_atom();
sexp parse_cons();
sexp parse_sexp();
sexp parse_macro();//really parse backtick but it's almost always used in macros
static sexp parse_list();
static sexp parse_arg_list();
static sexp parse_function_args();
sexp error_val=NIL_MACRO();
static void handle_error() __attribute__((noreturn));
static void handle_error(){
  evalError=1;
  longjmp(ERROR,-1);
}
//entry point
sexp yyparse(FILE* input){
  //error handling
  if(setjmp(ERROR)){
    fprintf(stderr,"Parse error\n");
    evalError=1;
    if(error_str){
      CORD_fprintf(stderr,error_str);
      fputs("\n",stderr);
    }
    if(yylval){free(yylval);}
    if(!NILP(error_val)){
      sexp temp=error_val;
      error_val=NIL;
      return temp;
    }
    longjmp(error_buf,-1);
    return NIL;
  } else{//normal parsing
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
  switch (yytag){
    case TOK_SPECIAL:{
      result.val.cons->car=(sexp){.tag=_special,.val={.special = yylval->val.special}};
      break;
    }
    case TOK_ID:{
      //    tmpsym = (symref)getGlobalSym(yylval->val.cord);
      //    if(tmpsym){
      //      result.val.cons->car=(sexp){.tag=_sym,.val={.var =tmpsym}};
      //    } else {
      tmpsym=xmalloc(sizeof(symbol));
      tmpsym->name=yylval->val.cord;
      tmpsym->val=UNBOUND;
      result.val.cons->car=(sexp){.tag=_sym,.val={.var =tmpsym}};
      break;
      //    }
    } 
    case TOK_LAMBDA:{//defun returns a TOK_LAMBDA as well
      sexp retval;
      retval.val.cons=xmalloc(sizeof(cons));
      retval.tag=_cons;
      XCAR(retval)=*yylval;
      sexp fake_retval=retval;
      //fake_retval because if we have, for a defun and lambda
      //retval=(defun . fake_retval= (var . (args) . (body)))
      //retval=fake_retval=(lambda . ( (args) . (body )))
      //this goto seems unecessary
    TEST_ARG_LIST:if(nextTok() != TOK_LPAREN){
        if(retval.val.cons->car.val.special == _defun){
          //because defun is (defun var (args) (body))
          //and lambda is (lambda (args) (body)) we need to parse
          //the var in defun seperately(that's what this is)
          retval.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
          fake_retval=XCDR(retval);
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
      //with the new args the only thing that changes is 
      cons* temp;
      temp=fake_retval.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
      //this, which becomes temp->car=parse_funcion_args();
      temp->car=parse_function_args();
      temp->cdr.val.cons=xmalloc(sizeof(cons));
      temp=temp->cdr.val.cons;
      temp->cdr=NIL;
      //parse the function body
      if(nextTok() == TOK_LPAREN){
        temp->car=parse_list();
        temp->car.tag=_cons;
      } else {
        temp->car=parse_atom();
      }
      if(nextTok() != TOK_RPAREN){
        error_val=error_sexp("error, missing closing parentheses in function defination");
        handle_error();
      }
      return retval;
    }
    case TOK_MACRO:{
      HERE();
      //(defmacro name (args) (body))
      sexp retval,location;
      retval.val.cons=xmalloc(sizeof(cons));
      retval.tag=_cons;
      XCAR(retval)=*yylval;
      if(nextTok() != TOK_ID){
        format_error_str("invaid macro name");
        handle_error();
      }
      XCDR(retval).val.cons=xmalloc(sizeof(cons));
      XCADR(retval)=parse_atom();
      if(nextTok() != TOK_LPAREN){
        format_error_str("macro defination is missing argument list");
        handle_error();
      }
      XCDDR(retval).val.cons=xmalloc(sizeof(cons));
      XCADDR(retval)=parse_function_args();
      XCDDDR(retval).val.cons=xmalloc(sizeof(cons));
      XCDDDDR(retval)=NIL;
      nextTok();
      switch(yytag){
        case TOK_LPAREN:
          XCADDDR(retval)=parse_list();
          XCADDDR(retval).tag=_cons;//since the list isn't quoted 
          break;
        case TOK_QUASI:
          HERE();
          XCADDDR(retval)=parse_macro();
          break;
        case TOK_QUOTE:
          XCADDDR(retval)=parse_list();
          break;
        default:
          XCADDDR(retval)=parse_atom();
          break;
      }
      return retval;
    }
    default: {//an unquoted list that's not a function call or special form
      //should probably be a parse error, but I'm not totally confidient
      //in things to do that yet
      return parse_list();
    } 
  }  
  //if we get here we have a function call or special form
  //implicit progn basically, keep parsing tokens until we get a close parens
  sexp temp;
  cons* old_pos=result.val.cons;
  while((nextTok())!=TOK_RPAREN){
    temp=parse_sexp();
    cons_pos->car=temp;
    /*    if(yytag == TOK_LPAREN){
      cons_pos->car=parse_cons();
    } else {
      temp=parse_atom();
      cons_pos->car=temp;
      }*/
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
    case TOK_LISP_TRUE:
      return LISP_TRUE;
    case TOK_LISP_FALSE:
      return LISP_FALSE;
    case TOK_KEYSYM:
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
      //      tmpsym=(symref)getGlobalSym(yylval->val.string);
      //      if(tmpsym){
      //        return (sexp){.tag=_sym,.val={.var = tmpsym}};
      //      } else {
        tmpsym=xmalloc(sizeof(symbol));
        tmpsym->name=yylval->val.string;
        tmpsym->val=UNBOUND;
        return (sexp){.tag=_sym,.val={.var = tmpsym}};
        //      }
      /*parse arrays
        TODO: add per element checks(as of now I only check the first)
        TODO: allow expressions in arrays (eval them to a # before use)
      */
    case TOK_LBRACE:
      //      HERE();
      nextTok();
      sexp retval;
      int size=8,i=-1;
      data* arr=retval.val.array=xmalloc_atomic(size*sizeof(data));
      retval.tag=_array;
      //      HERE();
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
          //  HERE();
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
            arr[i].uchar=yylval->val.uchar;break;
          default:
            my_abort("How'd you get here?");
        }
      } while(nextTok() != TOK_RBRACE);
      i++;//inc i so that i == len of array
      if(arrType == _char){
        if(i>=size){
          arr=retval.val.array=xrealloc(arr,(size+=1)*sizeof(data));
        }
        arr[i].uchar=L'\0';
      }
      retval.len=i;
      PRINT_FMT("len = %d",i);
      PRINT_MSG(print(retval));
      return retval;
    case TOK_STRING:
      PRINT_MSG(yylval->val.cord);
      return *yylval;
    case TOK_CHAR:
      return *yylval;
    case TOK_SPECIAL:
      return *yylval;//I dont' know how well this'll work
    default:
      CORD_sprintf(&error_str,"Error, expected literal atom recieved %r\n"
                   "Tag value recieved was %r\n",print(*yylval),token_name(yytag));
      handle_error();
  }
}
sexp parse_sexp(){
  switch(yytag){
    case TOK_QUOTE:{
      nextTok();
      sexp retval = parse_sexp();
      if(SYMBOLP(retval)){
        retval.val.var->val.quoted=1;
      }
      retval.quoted=1;
      return retval;
    }
    case TOK_QUASI:{
      return parse_macro();
    }
    case TOK_LPAREN:{
      return parse_cons();
    }
    default:{
      return parse_atom();
    }
  }
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
sexp parse_macro(){
  nextTok();
  if(yytag != TOK_LPAREN){
    sexp retval=parse_atom();
    retval.quoted=1;
    return retval;
  } else {
    sexp retval;
    //in this instance let comma+quoted == backquoted
    retval.quoted=1;
    retval.has_comma=1;
    cons* cur_loc=retval.val.cons=xmalloc(sizeof(cons));
    cons* prev_loc=cur_loc;
    do {
      switch(yytag){
        case TOK_LIST_SPLICE: {
          cur_loc->car.meta=_splice_list;
          /*fall through*/
        }
        case TOK_COMMA:{
          cur_loc->car=parse_sexp();
          cur_loc->car.has_comma=1;
          cur_loc->cdr.val.cons=xmalloc(sizeof(cons));
          prev_loc=cur_loc;
          cur_loc=cur_loc->cdr.val.cons;
          break;
        } 
        default: {
          cur_loc->car=parse_sexp();
          //          cur_loc->car.quoted+=1;
          cur_loc->cdr.val.cons=xmalloc(sizeof(cons));
          prev_loc=cur_loc;
          cur_loc=cur_loc->cdr.val.cons;
          break;
        }
      }
    } while (nextTok() != TOK_RPAREN && yytag != TOK_DOT);
    return retval;
  }
  handle_error();//should never get here
}
static inline sexp parse_list(){
  sexp retval;
  retval.tag=_list;
  cons* cur_loc=retval.val.cons=xmalloc(sizeof(cons));
  cons* prev_loc=cur_loc;
  while(nextTok() != TOK_RPAREN && yytag != TOK_DOT){
    cur_loc->car=parse_sexp();
    cur_loc->cdr.val.cons=xmalloc(sizeof(cons));
    prev_loc=cur_loc;
    cur_loc=cur_loc->cdr.val.cons;
  }
  if(yytag == TOK_DOT){
    nextTok();
    //some kind of error handling needs to go here
    prev_loc->cdr=parse_sexp();
    //how do I tag this?
    nextTok();
  } else {//yytag // TOK_RPAREN
    prev_loc->cdr=NIL;
  }
  return retval;
}
static inline sexp parse_function_args(){
  sexp retval;
  retval.val.funarg=xmalloc(sizeof(struct function_args));
  //just an inital guess, should be more than enough in most cases
  //at the moment I haven't actually put in checking for more that
  //8 args
  //I need this to store the argument names
  symbol* args=xmalloc(16*sizeof(symbol));
  *retval.val.funargs=(function_args)
    {.num_req_args=0,.num_opt_args=0,.num_keyword_args=0,
     .has_rest_arg=0,.args=args,.max_args=0};
  retval.tag=_funargs;
  while(nextTok() != TOK_RPAREN){
    if(yytag != TOK_ID){
    TYPE_ERROR:format_error_str("function arguments must be identifiers");
      handle_error();
    }
    //we get yylval->val.cord via CORD_from_char_star, so it shourd be a cstring
    if(yylval->val.cord[0] == '&'){
      if(!CORD_cmp(yylval->val.cord,"&optional")){
        while(yytag != TOK_RPAREN){
          switch(yytag){
            case TOK_LPAREN:
              nextTok();
              if(yytag != TOK_ID){goto TYPE_ERROR;}
              LAST_ARG(retval.val.funargs).name=yylval->val.cord;
              LAST_ARG(retval.val.funargs).val=UNBOUND;
              if(nextTok() != TOK_RPAREN){
                //default arg must be an atom
                LAST_ARG(retval.val.funargs).val=parse_atom();
                ADD_OPT_ARG(retval.val.funargs);
                if(nextTok() != TOK_RPAREN){
                  format_error_str
                    ("excess arguments in optional argument's default value");
                  handle_error();
                }
              }
              break;
            case TOK_ID:
              LAST_ARG(retval.val.funargs).name=yylval->val.cord;
              LAST_ARG(retval.val.funargs).val=NIL;
              ADD_OPT_ARG(retval.val.funargs);
              break;
            default:
              format_error_str
                ("optional argument must be id or cons of id and default val");
              handle_error();
          }
          if(nextTok() == TOK_ID){
            if(yylval->val.cord[0]=='&'){
              if(!CORD_cmp(yylval->val.cord,"&key")){
                goto KEYWORD_PARAM;
              }
              if(!CORD_cmp(yylval->val.cord,"&rest")||
                 !CORD_cmp(yylval->val.cord,"&body")){
                goto REST_PARAM;
              }
            }
          }
        }
      }
      if(!CORD_cmp(yylval->val.cord,"&key")){
      KEYWORD_PARAM:
        format_error_str ("keyword paramaters unimplimented");
        handle_error();
        nextTok();
      }
      if(!CORD_cmp(yylval->val.cord,"&rest")||
         !CORD_cmp(yylval->val.cord,"&body")){
      REST_PARAM:
        if(nextTok() != TOK_ID){goto TYPE_ERROR;}
        LAST_ARG(retval.val.funargs).name=yylval->val.cord;
        LAST_ARG(retval.val.funargs).val=UNBOUND;
        ADD_REST_ARG(retval.val.funargs);
        nextTok();
      }
      break;
    } else {
      LAST_ARG(retval.val.funargs).name=yylval->val.cord;
      LAST_ARG(retval.val.funargs).val=UNBOUND;
      ADD_REQ_ARG(retval.val.funargs);
    }
  }
  if(yytag != TOK_RPAREN){
    format_error_str("malformed argument list");
    handle_error();
  }
  return retval;
}
