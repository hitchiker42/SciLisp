/* the c code that goes at the end of read.lex, inserted using include
              
   Copyright (C) 2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
//From this point on flex doesn't touch anything, so it's just normal C99
void *yyalloc(size_t bytes,void* yyscanner){
  return xmalloc(bytes);
}
void *yyrealloc(void *ptr,size_t bytes,void *yyscanner){
  return xrealloc(ptr,bytes);
}
void yyfree(void *ptr,void *yyscanner){
  xfree(ptr);
}
sexp lisp_read(sexp lisp_stream){
  //really need to write a cord stream/test the one I already wrote
  FILE *stream;
  if(STRINGP(lisp_stream)){
    stream=fmemopen(CORD_to_char_star(lisp_stream.val.string->cord),
                    lisp_stream.val.string->len,"r");
  } else if (STREAMP(lisp_stream)){
    stream=lisp_stream.val.stream;
  } else {
    raise_simple_error_fmt(Etype,"Invalid type passed to read, expected a stream");
  }
  return read_full(stream);
}
sexp read_full(FILE *stream){
  yyscan_t scanner;
  yylex_init(&scanner);
  yyset_in(stream,scanner);
  sexp *yylval=xmalloc(sizeof(sexp));
  return c_read(&scanner,yylval,NULL);
}
sexp c_read_safe(yyscan_t *scanner,register sexp *yylval){
  frame *read_error_frame=make_frame((uint64_t)Eread,simple_error_frame);
  push_frame(current_env,*read_error_frame);
  if(setjmp(read_error_frame->dest)){
    CORD_fprintf(stderr,read_error_frame->value.val.string->cord);
    return NIL;
  }
  sexp retval=c_read(scanner,yylval,NULL);
  pop_frame(current_env);
  return retval;
}
sexp read_list(yyscan_t *scanner,register sexp *yylval);
sexp read_untyped_array(yyscan_t *scanner,sexp *yylval);
sexp read_typed_array(yyscan_t *scanner,register sexp *yylval);
//reads one sexp, an error if c_read returns a non-zero value in last_tok
sexp c_read_sub(yyscan_t *scanner,register sexp *yylval){
  int last_tok;
  sexp retval=c_read(scanner,yylval,&last_tok);
  if(last_tok){
    raise_simple_error_fmt(Eread,"invalid read syntax %c",last_tok);
  }
  return retval;
}
sexp read_matrix(yyscan_t *scanner,register sexp *yylval);
//reads one sexp or token(i.e ',',''',')',']','}','.')
//so read list/vector/etc can be called as
/* int last_tok=0;
   while(!last_tok){
   c_read(scanner,yylval,&last_tok);
   do something...
   }
*/
//make sure I handle every possible token
//Wswitch-enum requires every enumerated value to be used explicity 
#pragma GCC diagnostic warning "-Wswitch-enum"
sexp c_read(yyscan_t *scanner,register sexp *yylval,int *last_tok){
#define get_tok() (yytag = yylex(yylval,scanner))
  register TOKEN yytag=0;
  register cons *ast=xmalloc(sizeof(cons));
  get_tok();
  while(1){
    switch(yytag){
      case TOK_RPAREN:
        ast->car=read_list(scanner,yylval);
        break;
      case TOK_RBRACE:
        ast->car=read_typed_array(scanner,yylval);
        break;
      case TOK_DBL_RBRACE:
        ast->car=read_untyped_array(scanner,yylval);
        break;
      case TOK_MAT_OPEN:
        ast->car=read_matrix(scanner,yylval);
        break;
      case TOK_INT:
      case TOK_STRING:
      case TOK_REAL:
      case TOK_CHAR:
      case TOK_ID:
      case TOK_KEYSYM:
        ast->car=*yylval;
        break;
      case TOK_QUOTE:{
        sexp value=c_read(scanner,yylval,last_tok);
        return c_list2(Qquote_sexp,value);
        break;
      }
      case TOK_HASH:
        /*used for special read syntax*/
      case TOK_LPAREN:
      case TOK_LBRACE:
      case TOK_DBL_LBRACE:
      case TOK_MAT_CLOSE:
        *last_tok=yytag;
        return NIL;
      case TOK_BACKQUOTE:
        backtick_flag=1;
        sexp value=c_read(scanner,yylval,last_tok);
          backtick_flag=0;
        return c_list2(Qbackquote_sexp,value);
      case TOK_COMMA:{
        if(!backtick_flag){
          raise_simple_error(Eread,"error comma not inside a backquote");
        }
        backtick_flag=0;
        sexp value=c_read(scanner,yylval,last_tok);
        backtick_flag=1;
        return c_list2(Qcomma_sexp,value);
      }
    }
    /*
      get_tok();
      if(yytag>0){
      ast->cdr=cons_sexp(xmalloc(sizeof(cons)));
      ast=ast->cdr;
      continue;
      } else {*/
    return cons_sexp(ast);
  }
}
#pragma GCC diagnostic ignored "-Wswitch-enum"
sexp read_list(yyscan_t *scanner,register sexp *yylval){
  sexp retval;
  sexp new_list=retval=cons_sexp(xmalloc(sizeof(cons)));
  int last_tok=0;
  while(!last_tok){
    XCAR(new_list)=c_read(scanner,yylval,&last_tok);
    XCDR(new_list)=cons_sexp(xmalloc(sizeof(cons)));
    new_list=XCDR(new_list);
  }
  switch(last_tok){
    case ')':
      new_list=NIL;
      return retval;
    case '.':
      new_list=c_read(scanner,yylval,&last_tok);
      c_read(scanner,yylval,&last_tok);
      if(last_tok != TOK_RPAREN){
        break;
      }
      return retval;
  }
  raise_simple_error(Eread,"read error, expected ')' or '.' at end of list");
}
/*read an untyped array  delimited by double braces and
  containing any sexp as an element, it's self quoting as a literal
  no multi dimensional array literals*/
sexp read_untyped_array(yyscan_t *scanner,sexp *yylval){
  /*we obviously don't know the size, but being an array we want a linear
    block of memory, so allocate 16 cells at first and scale by 2*/
  uint64_t size=16;
  uint64_t i=1;//we need the first 128 bits for header info
  int last_tok=0;
  sexp *arr=xmalloc(sizeof(sexp)*size);
  register sexp val;
  while(!last_tok){
    if(i>=size){
      size*=2;/*size <<=1*/
      arr=xrealloc(arr,sizeof(sexp)*size);
    }
    val=c_read(scanner,yylval,&last_tok);
    arr[i++]=val;
  }
  if(last_tok != ']'){
    raise_simple_error(Eread,"Read error expected ']' at end of array");
  }
  lisp_array *retval=(lisp_array*)arr;
  retval->len=i;
  retval->dims=1;
  retval->type=sexp_nil;
  //FIXME: This won't work for anyting but small simple vectors
  return array_sexp(retval);
}
/* typed array, delimited by braces, type is implicit based on first element
   if we get a type we can't implicicly convert to the type of the first element
   we raise an error
*/
sexp read_typed_array(yyscan_t *scanner,register sexp *yylval){
  uint64_t size=16;
  uint64_t i=2;/*we need the first 128 bits for header info*/
  int last_tok=0;
  data *arr=xmalloc_atomic(sizeof(data)*size);
  /*generally best not to call this directally, but this is special
    since we know anything involving a recursive call to read
    is an error*/
  TOKEN yytag=yylex(yylval,scanner);
  /* another low level hack, the TOKEN enum is layed out in such a way
     that literal tags are between 0 and 10*/
  if(!(yytag >=0 && yytag <= 10)){
    raise_simple_error(Eread,"Read error invalid token inside of typed array");
  }
  TOKEN array_type=yytag;
  arr[i++]=yylval->val;
  while(!last_tok){
    if(i>size){
      size*=2;
      if(size>=2048){
        /*assuming a page size of 4096 then an array of 2k will be off page
          50% of the time, which is high enough to be worth allocating specially*/
        void *large_arr=GC_malloc_atomic_ignore_off_page(size*sizeof(data));
        memcpy(large_arr,arr,size>>1);
        arr=large_arr;
      } else {
        arr=xrealloc(arr,size*sizeof(data));
      }
    }
    yytag=yylex(yylval,scanner);
    if(yytag != array_type){
      raise_simple_error(Eread,"Read error, multiple types in a typed array");
    }
    arr[i++]=yylval->val;
  }
  if(last_tok != TOK_DBL_RBRACE){
    raise_simple_error(Eread,"Read error, expected ']]' at the end of a typed array");
  }
  lisp_array *retval=(lisp_array*)arr;
  retval->len=i;
  retval->dims=1;
  switch(array_type){
    case TOK_INT:
      arr->type=sexp_int64;//for now
      break;
    case TOK_REAL:
      arr->type=sexp_real64;//for now
      break;
    case TOK_CHAR:
      arr->type=sexp_uchar;
      break;
    case TOK_STRING:
      arr->type=sexp_string;
      break;
  }
  //FIXME: This won't work for anyting but small simple vectors
  return array_sexp(retval);
}

//just a quick ste
