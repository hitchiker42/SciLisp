 /*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//FIXME: this (assert-eq (1 (+ ` 1))) causes a segfault
//this is not acceptable, it's malformed input, but it shouldn't cause a segfault
//FIXME: PARSING BACKQUOTES HAS SOME SERIOUS ISSUES
#include "common.h"
#include "cons.h"
#include "lex.yy.h"
#define nextTok() (yytag=yylex())
#define parse_next_sexp()                       \
  nextTok();                                    \
  parse_sexp()
sexp ast;//generated ast
cons* cur_pos;//pointer to current location in ast
TOKEN yytag;//current token
int evalError=0;
static jmp_buf ERROR;//location of error handling function
static int inside_backquote=0;//global flag to determine how to handle a comma
static sexp parse_atom();
static sexp parse_cons();
sexp parse_sexp();
static sexp parse_backtick();
static sexp parse_list();
static sexp parse_arg_list();
static sexp parse_function_args();
static sexp parse_function(int has_name,int need_rparen);
sexp error_val=NIL_MACRO();
static void error_recovery();
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
    if(!NILP(error_val)){
      sexp temp=error_val;
      error_val=NIL;
      return temp;
    }
    longjmp(error_buf,-1);
    return NIL;
  } else{//normal parsing
    yyrestart(input);
    ast.tag=_cons;
    cons* cur_pos=ast.val.cons=xmalloc(sizeof(cons));
    cons* prev_pos=cur_pos;
    yylval=xmalloc(sizeof(sexp));
    while((nextTok()) != -1){
      cur_pos->car=parse_sexp();
      cur_pos->cdr.val.cons=xmalloc(sizeof(cons));
      cur_pos->cdr.tag=_cons;
      prev_pos=cur_pos;
      cur_pos=cur_pos->cdr.val.cons;
    }
    //    xfree(cur_pos);
    prev_pos->cdr.tag=_nil;
    prev_pos->cdr.val.meta=_nil;
    // PRINT_MSG(print(ast));
    return ast;
  }
  handle_error();
}
sexp parse_sexp(){
  switch(yytag){
    case TOK_QUOTE:{
      if(nextTok() == TOK_LPAREN){
        return parse_list();
      } else {
        sexp retval = parse_sexp();
        if(SYMBOLP(retval)){
          retval.val.var->val.quoted=1;
        }
        retval.quoted=1;
        return retval;
      }
    }
    case TOK_COMMA:{
      if(!inside_backquote){
        format_error_str("Error, Commma not inside a backquote");
        handle_error();
      } else {
        inside_backquote=0;
        int is_spliced_list=0;
        if(nextTok() == TOK_AROBASE){
          is_spliced_list=1;
          nextTok();
        }
        sexp retval=parse_sexp();
        retval.has_comma=1;
        if(is_spliced_list){
          retval.meta=_splice_list;
        }
        inside_backquote=1;
        return retval;
      }
    }
    case TOK_QUASI:{
      inside_backquote=1;
      //kind of a hack
      //sexp retval=parse_backtick();
      sexp retval = XCAR(parse_backtick());
      retval.has_comma=1;
      retval.quoted=1;
      inside_backquote=0;
      return retval;
    }
    case TOK_LPAREN:{
      return parse_cons();
    }
    case TOK_UNKN:{
      format_error_str("unknown token %lc recieved, discarding current input",
                       yylval->val.uchar);
      handle_error();
    }
    default:{
      return parse_atom();
    }
  }
}
sexp parse_cons(){
  //sexp* result=xmalloc(sizeof(sexp));
  if(nextTok()==TOK_RPAREN){
    format_error_str("invalid construct ()");
    handle_error();
  }
  sexp result;
  symref tmpsym=0;
  result.tag=_cons;
  result.is_ptr=1;
  result.val.cons=xmalloc(sizeof(cons));
  cons* cons_pos=result.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
  //  sexp cons_pos=result.val.cons->cdr;
  //at this point there's no difference between any macro, special form or function
  switch (yytag){
    case TOK_SPECIAL:{
      XCAR(result)=spec_sexp(yylval->val.special);
      break;
    }
    case TOK_ID:{
      tmpsym=xmalloc(sizeof(symbol));
      tmpsym->name=yylval->val.cord;
      tmpsym->val=UNBOUND;
      XCAR(result)=symref_sexp(tmpsym);
      break;
    }
      //this feels like a really bad hack is it?
    case TOK_LET:{
      XCAR(result)=*yylval;
      XCDR(result).val.cons=xmalloc(sizeof(cons));
      XCDDR(result).val.cons=xmalloc(sizeof(cons));
      if(XCAR(result).val.special==_flet){
        //I ought to fix this(empty let expressions should be ok)
        if(nextTok() != TOK_LPAREN){
          format_error_str("error, empty flet expression");
          handle_error();
        }
        sexp fun_bindings=NIL;
        cons *fun_list=fun_bindings.val.cons=xmalloc(sizeof(cons));
        cons *trail=fun_list;
        fun_bindings.tag=_cons;
        fun_bindings.is_ptr=1;
        while(nextTok() != TOK_RPAREN){
          if(yytag != TOK_LPAREN){
            format_error_str("error, malformed flet expression");
            handle_error();
          }
          fun_list->car=parse_function(1,1);
          fun_list->cdr.val.cons=xmalloc(sizeof(cons));
          trail=fun_list;
          fun_list=fun_list->cdr.val.cons;
        }
        trail->cdr=NIL;
        XCADR(result)=fun_bindings;
      } else {
        if(nextTok() != TOK_LPAREN){
          format_error_str("error, empty let expression");
          handle_error();
        }
        sexp var_bindings=NIL;
        cons *var_list=var_bindings.val.cons=xmalloc(sizeof(cons));
        cons *trail=var_list;
        var_bindings.tag=_cons;
        var_bindings.is_ptr=1;
        while(nextTok() != TOK_RPAREN){
          if(yytag != TOK_LPAREN){
            format_error_str("error, malformed flet expression");
            handle_error();
          }
          var_list->car=parse_sexp();
          var_list->cdr.val.cons=xmalloc(sizeof(cons));
          trail=var_list;
          var_list=var_list->cdr.val.cons;
        }
        trail->cdr=NIL;
        XCADR(result)=var_bindings;
      }
      if(nextTok() == TOK_RPAREN){
        format_error_str("missing bindings list in let expression");
        handle_error();
      }
      //for now let expressions need a progn
      XCADDR(result)=parse_sexp();
      XCDDDR(result)=NIL;
      if(nextTok() != TOK_RPAREN){
        format_error_str("excess args to let expression");
        handle_error();
      }
      return result;
    }
    case TOK_LAMBDA:{//defun returns a TOK_LAMBDA as well
      sexp retval;
      HERE();
      retval=cons_sexp(xmalloc(sizeof(cons)));
      //      retval.tag=_cons;
      //      retval.is_ptr=1;
      XCAR(retval)=*yylval;
      int is_defun=
        ((yylval->val.special == _defun)?1:0);
      XCDR(retval)=parse_function(is_defun,1);
      return retval;
      sexp fake_retval=retval;
      while(nextTok() != TOK_LPAREN){
        if(retval.val.cons->car.val.special == _defun){
          //because defun is (defun var (args) (body))
          //and lambda is (lambda (args) (body)) we need to parse
          //the var in defun seperately(that's what this is)
          retval.val.cons->cdr.val.cons=xmalloc(sizeof(cons));
          fake_retval=XCDR(retval);
          XCAR(fake_retval)=parse_sexp();
        }  else {
          format_error_str("expected argument list following lambda or defun");
          handle_error();
        }
      }
      cons* temp;
      temp=XCDR(fake_retval).val.cons=xmalloc(sizeof(cons));
      temp->car=parse_function_args();
      temp->cdr.val.cons=xmalloc(sizeof(cons));
      temp=temp->cdr.val.cons;
      temp->cdr=NIL;
      //parse the function body
      TOKEN body_start=nextTok();
      temp->car=parse_sexp();
      if(body_start == TOK_LPAREN){
        if(nextTok() != TOK_RPAREN){
          error_val=error_sexp
            ("error, missing closing parentheses in function defination");
          handle_error();
        }
      }
      return retval;
      /*      if(nextTok() == TOK_LPAREN){
        temp->car=parse_list();
        temp->car.tag=_cons;
      } else {
        temp->car=parse_atom();
      }

      return retval;*/
    }
    case TOK_MACRO:{
      //(defmacro name (args) (body))
      sexp retval,location;
      retval.val.cons=xmalloc(sizeof(cons));
      retval.tag=_cons;
      retval.is_ptr=1;
      XCAR(retval)=*yylval;
      XCDR(retval)=parse_function(1,0);
      return retval;
      /*      XCDR(retval).val.cons=xmalloc(sizeof(cons));
      nextTok();
      XCADR(retval)=parse_sexp();//macro name
      if(!SYMBOLP(XCADR(retval))){
        format_error_str("error, expected identifer for macro name");
        handle_error();
      }
      if(nextTok() != TOK_LPAREN){
        format_error_str("macro defination is missing argument list");
        handle_error();
      }
      XCDDR(retval).val.cons=xmalloc(sizeof(cons));
      XCADDR(retval)=parse_function_args();//macro args
      nextTok();
      XCDDDR(retval).val.cons=xmalloc(sizeof(cons));
      XCADDDR(retval)=parse_sexp();
      XCDDDDR(retval)=NIL;
      //      XCADDDR(retval)=parse_sexp();//macro body
      return retval;*/
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
      } else{
        return *yylval;//return anything else unevaluated
      }
    case TOK_ID:
        tmpsym=xmalloc(sizeof(symbol));
        tmpsym->name=yylval->val.string;
        tmpsym->val=UNBOUND;
        return symref_sexp(tmpsym);
        //      }
      /*parse arrays
        TODO: add per element checks(as of now I only check the first)
        TODO: allow expressions in arrays (eval them to a # before use)
      */
    case TOK_LBRACE:{
      int size=8,i=0;
      sexp retval;
      retval.tag=_array;
      retval.is_ptr=1;
      sexp *arr=retval.val.array=xmalloc(sizeof(sexp)*size);
      while(nextTok() != TOK_RBRACE){
        if(i>=size){
          arr=retval.val.array=xrealloc(arr,(size*=2)*sizeof(sexp));
        }
        arr[i]=parse_sexp();
        i++;
      }
      retval.len=i;
      return retval;
    }
    case TOK_DBL_LBRACE:{
      //      HERE();
      nextTok();
      sexp retval;
      int size=8,i=-1;
      data* arr=retval.val.typed_array=xmalloc_atomic(size*sizeof(data));
      retval.tag=_typed_array;
      //      HERE();
      _tag arrType=yylval->tag;
      PRINT_MSG(tag_name(arrType));
      if (arrType != _double && arrType!=_long && arrType !=_char){
        HERE();
        CORD_sprintf(&error_str,
                     "Arrays of type %s are unimplemented\n",arrType);
        handle_error();
      }
      retval.meta=(arrType == _double? _double :
                   (arrType == _long ? _long :
                    //THIS DOESN'T WORK FOR MAKING UTF8 STRINGS
                    (arrType == _char? _char : assert(0),0)));
      do{
        if(i++>=size){
          //  HERE();
          arr=retval.val.typed_array=xrealloc(arr,(size*=2)*sizeof(data));
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
      } while(nextTok() != TOK_DBL_RBRACE);
      i++;//inc i so that i == len of array
      if(arrType == _char){
        if(i>=size){
          arr=retval.val.typed_array=xrealloc(arr,(size+=1)*sizeof(data));
        }
        arr[i].uchar=L'\0';
      }
      retval.len=i;
      PRINT_FMT("len = %d",i);
      PRINT_MSG(print(retval));
      return retval;
    }
    case TOK_STRING:
    case TOK_MACRO:
    case TOK_CHAR:
    case TOK_SPECIAL:
    case TOK_LET:
      return *yylval;//I dont' know how well this'll work
    default:
      format_error_str("Error, expected literal atom recieved %r\n"
                       "Tag value recieved was %r\n",
                       print(*yylval),token_name(yytag));
      handle_error();
  }
}
sexp read_string(CORD code) {
  PRINT_MSG(code);
  FILE* stringStream=fmemopen(CORD_to_char_star(code),CORD_len(code),"r");
  push_jmp_buf(error_buf);
  if(setjmp(error_buf)){
    error_buf[0]=pop_jmp_buf();
    return error_sexp("read error,parsing failed");
  }
  return yyparse(stringStream);
}
sexp lisp_read(sexp code){
  if(!STREAMP(code)){
    return format_type_error("read","stream",code.tag);
  }
  push_jmp_buf(error_buf);
  if(setjmp(error_buf)){
    error_buf[0]=pop_jmp_buf();
    return error_sexp("read error,parsing failed");
  }
  sexp ast= yyparse(code.val.stream);
  if(!CONSP(ast)){
    return ast;
  } else if (NILP(XCDR(ast))){
    return XCAR(ast);
  } else {
    return ast;
  }
}
sexp lisp_read_string(sexp code){
  if(!STRINGP(code)){
    return format_type_error("read","string",code.tag);
  }
  sexp ast = read_string(code.val.cord);
  if(!CONSP(ast)){
    return ast;
  } else if (NILP(XCDR(ast))){
    return XCAR(ast);
  } else {
    return ast;
  }
}
sexp parse_backtick(){
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
    retval.tag=_cons;
    retval.is_ptr=1;
    cons* cur_loc=retval.val.cons=xmalloc(sizeof(cons));
    cons* prev_loc=cur_loc;
    while (yytag != TOK_RPAREN && yytag != TOK_DOT){
      HERE();
      switch(yytag){
        case TOK_COMMA:{
          int is_spliced_list=0;
          if(nextTok() == TOK_AROBASE){
            is_spliced_list=1;
            nextTok();
          }
          cur_loc->car=parse_sexp();
          if(is_spliced_list){
            cur_loc->car.meta=_splice_list;
          }
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
      nextTok();
    }
    if(yytag == TOK_DOT){
      if(nextTok() == TOK_RPAREN){
        format_error_str("Expected expression after '.', got ')'");
        handle_error();
      }
      //some kind of error handling needs to go here
      prev_loc->cdr=parse_sexp();
      //how do I tag this?
      nextTok();
    } else {
    /*      format_error_str("erorr, macro body must be a proper list");
      handle_error();
      }*/
    prev_loc->cdr=NIL;
    }
    return retval;
  }
  format_error_str("Shouldn't get here, end of parse macro");
  handle_error();//should never get here
}
static inline sexp parse_list(){
  sexp retval;
  int i=0;
  retval.tag=_list;
  retval.is_ptr=1;
  cons* cur_loc=retval.val.cons=xmalloc(sizeof(cons));
  cons* prev_loc=cur_loc;
  if(nextTok() == TOK_RPAREN){
    format_error_str("invalid construct ()");
    handle_error();
  }
  do{
    if(yytag == TOK_LPAREN){
      cur_loc->car=parse_list();
    } else {
      cur_loc->car=parse_sexp();
    }
    cur_loc->cdr.val.cons=xmalloc(sizeof(cons));
    prev_loc=cur_loc;
    cur_loc=cur_loc->cdr.val.cons;
    i++;
  }  while(nextTok() != TOK_RPAREN && yytag != TOK_DOT);
  if(yytag == TOK_DOT){
    if(nextTok() == TOK_RPAREN){
      format_error_str("Expected expression after '.', got ')'");
      handle_error();
    }
    //some kind of error handling needs to go here
    prev_loc->cdr=parse_sexp();
    //how do I tag this?
    nextTok();
  } else {//yytag // TOK_RPAREN
    prev_loc->cdr=NIL;
  }
  retval.len=i;
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
  retval.is_ptr=1;
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
        //keyword argument is id | ((id | (:key id)) [init_val])
        //by default the keyword is made from id, but it can be
        //specified by the :key parameter
        /* keywords should be something like,
           an alist of keyword varname pairs,
           if we get a keyword argument we'll scan this list for a matching
           key then just set varname to the right value, by default
           all variables will be nil, so if the function is a c function
           we can just use the variable's values without anything special
           needing to be done*/
        //how about
        LAST_ARG(retval.val.funargs).name="#keyargs";
        cons *cur_arg,*last_arg;
        symref cur_var;
        sexp keyarg_list;
        keyarg_list.tag=_list;keyarg_list.is_ptr=1;
        keyarg_list.val.cons=cur_arg=xmalloc(sizeof(cons));
        while(nextTok() != TOK_RPAREN){
          switch(yytag){
            case TOK_LPAREN:
              //do something
            case TOK_ID:
              cur_arg->car=cons_sexp((xmalloc(sizeof(cons))));
              XCAR(cur_arg->car)=getKeySymSexp(CORD_cat(":",yylval->val.cord));
              cur_var=xmalloc(sizeof(symbol));
              *cur_var=(symbol){.name=yylval->val.cord,.val=NIL};
              XCDR(cur_arg->car)=symref_sexp(cur_var);
              cur_arg->cdr.val.cons=xmalloc(sizeof(cons));
              last_arg=cur_arg;
              cur_arg=cur_arg->cdr.val.cons;
              break;
          }
          last_arg->cdr=NIL;
          ADD_KEYWORD_ARG(retval.val.funargs);
        }
          LAST_ARG(retval.val.funargs).val=keyarg_list;
          break;//what does this break out of ?
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
static sexp parse_function(int has_name,int need_rparen){
  cons* temp;
  sexp retval;
  temp=retval.val.cons=xmalloc(sizeof(cons));
  if(has_name){
    nextTok();
    temp->car=parse_sexp();
    if(!SYMBOLP(temp->car)){
      format_error_str
        ("error, expected variable name in function definition,token recieved was %r",token_name(yytag));
      handle_error();
    }
    temp->cdr.val.cons=xmalloc(sizeof(cons));
    temp=temp->cdr.val.cons;
  }
  if(nextTok() != TOK_LPAREN){
    format_error_str
      ("expected argument list following lambda or function declaration");
    handle_error();
  }
  temp->car=parse_function_args();
  temp->cdr.val.cons=xmalloc(sizeof(cons));
  temp=temp->cdr.val.cons;
  temp->cdr=NIL;
  //parse the function body
  TOKEN body_start=nextTok();
  temp->car=parse_sexp();
  if(need_rparen){
    if(nextTok() != TOK_RPAREN){
      format_error_str
        ("error, missing closing parentheses in function defination");
      handle_error();
    }
  }
  return retval;
}
#define mkTypeCase(hash,name)                 \
  case hash: return name
_tag parse_tagname(CORD tagname){
  return get_type_from_string(tagname).val.meta;
  uint64_t taghash=fnv_hash(tagname,CORD_len(tagname));
  switch(taghash){
    //    mkTypeCase(0x74b7d17aa973b4f5,_false);
    mkTypeCase(0xbca0591195d8188 , _cons);//typename == cons
    mkTypeCase(0xf9d0c88f42834344, _long);//typename == int64
    mkTypeCase(0xcde8c9ad70d16733, _long);//typename == long
    mkTypeCase(0x54d746c609966d93, _ulong);//typename == uint64
    //    mkTypeCase(0x27ff5150e0955ae4, _float);//
    mkTypeCase(0x5da874f342c0395f, _double);//typename == real64
    mkTypeCase(0xa0880a9ce131dea8, _double);//typename == double
    mkTypeCase(0x102f3138836b306a, _bigint);//typename == bigint
    mkTypeCase(0xf7de010e01156121, _bigfloat);//typename == bigfloat
    mkTypeCase(0xf2a393910b5b3ebd, _char);//typename == char
    mkTypeCase(0x704be0d8faaffc58, _str);//typename == string
    mkTypeCase(0x4f9e14b634c6b026, _array);//typename == array
    mkTypeCase(0x613e0b076b7b0259, _ustr);//typename ==  ustring
    mkTypeCase(0xbc35a7f3228f2b18, _regex);//typename == regex
    mkTypeCase(0x4f6a36d8e8907985, _stream);//typename == stream
    //    mkTypeCase(0xe85fdaf4496b4ebb,_matrix);
    mkTypeCase(0xbf779aad69748141, _list);//typename == list
    mkTypeCase(0x7fb88ddaf3474073, _dpair);//typename == dpair
    mkTypeCase(0x826b4caaf325324a, _fun);//typename == lambda
    mkTypeCase(0xe81b0096bc73f511, _sym);//typname == symbol
    mkTypeCase(0x12c837a52b5d72b3, _macro);//typename == macro
    //    mkTypeCase(0x28f23a5007ad556e,_type);
    //    mkTypeCase(0xedd5fa43656c3778,_lam);
    //    mkTypeCase(0x6b3f9c85427b90b9,_lenv);
    //    mkTypeCase(0x3daa8643932b5bc1,_env);
    //    mkTypeCase(0xfa31bb89bdf2b84d,_keyword);
    //    mkTypeCase(0x651f3fe3023c9cc9,_funarg);
    //    mkTypeCase(0x10b648bcccfe500e,_funargs);
    //    mkTypeCase(0x5436f750202bd962,_true);
    //    mkTypeCase(0x7967fcf0e2e57dd8,_obarray);
    default:
      return _error;
  }
}
