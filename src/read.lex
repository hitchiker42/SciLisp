/* -*- c-syntactic-indentation: nil; electric-indent-inhibit: t -*- */
%{
/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#define IN_LEXER
#include "common.h"
/*#include "prim.h"*/
#include "unicode.h"
//#include "read.h"
#ifdef YY_DECL
#undef YY_DECL
#endif
#define YY_DECL TOKEN yylex(sexp *yylval,yyscan_t yyscanner)
#define YYSTYPE sexp
static int comment_depth=0;
static thread_local int backtick_flag=0;
#define FORMAT_READ_ERR(format,args...)       \
  ({CORD read_err_string;                     \
    CORD_sprintf(&read_err_string,            \
    format,##args);                           \
    read_err_string;})
 /*seems redundant but allows me to change how I handle errors */
static inline void __attribute__((noreturn)) handle_read_error(){
  longjmp(read_err,-1);
#define raise_read_error(value) raise_simple_error(Eread,value)
#define raise_read_error_fmt(format,args...)                          \
  raise_simple_error(Eread,string_sexp(FORMAT_READ_ERR(format,args)))
}
%}
 /*FLEX COMMENT RULES:
   Comment needs to start after a space on a line;
   Comments can't be on the same line as a defination
   Need to use / * * / comments  */
 /*backslashes and quotes are refered to using ascii escapes
  \x5c == \ & \x22 == " & \x27 == '
  mostly so unmatched quotes don't fuck up syntax highlighing*/
 /*Unicode support, basic idea borrowed from the lexer for
  *the txr by kaz kylheku language*/
ASC     [\x00-\x7f]{-}[?\\] 
ASC_FULL [\x00-\x7f]
ASCSTR  [\x00-\x21\x23-\x7f]
 /*explaination of special characters:
  * : reserved for keywords, pacakge access and type annotations
  * ; reserved for comments
  * (){}[] reserved delimiers, () should be obvious, [] for arrays and {} for future use
  * ' " ' quotes, obviously
  * # for quoting and delimiting special symbols(#t,#f) and deliberately unreaable symbols
  * . reserved for pairs and package access
  * | nothing now but reserved for later (quoting ids?)
  * @ for ,@ in macros
  * , for macros
  * Starting special characters ? for characters, +-[0-9] for numbers
  */
DIGIT [0-9]
XDIGIT [0-9a-fA-F]
 /*                                  '   "    [  \   ]        space*/
ASC_ID  [\x00-\x7f]{-}[#|,.;:`(){}@\x27\x22\x5b\x5c\x5d\x20\t\n]
ASCN    [\x00-\t\v-\x7f]
U       [\x80-\xbf]
U2      [\xc2-\xdf]
U3      [\xe0-\xef]
U4      [\xf0-\xf4]
UANY    {ASC}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UANYN   {ASCN}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UONLY   {U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UCHAR "?"("\\"[?nt]|"\\\\"|"\\x"([[:xdigit:]]{1,2})|"\\u"([[:xdigit:]]{1,4})|"\\U"([[:xdigit:]]{1,8})|{UANY})
USTR    {ASCSTR}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UID {ASC_ID}|{UONLY}
ID ({UID}|("\\".))+
QUOTED_ID ("|"({UANY}|'\\\\'|'\\|')+"|")
KEYSYM ":"{ID}
TYPENAME "::"{ID}
QUALIFIED_ID {ID}":"{ID}
QUALIFIED_ID_ALL {ID}":."{ID}
%option noyywrap
   /*start condition for scanning nested comments*/
%x comment
%option noyyalloc noyyrealloc noyyfree
%option 8bit reentrant
%option header-file="read_lex.h" outfile="read.c"
%%   /*Literals*/
 /*numbers need to come first so something like +1 gets read as an integer
  but something like 1+ gets read as a symbol*/
[+\-]?{DIGIT}+ {LEX_MSG("lexing int");
  *yylval=long_sexp((long)strtol(yytext,NULL,0));return TOK_INT;}
[+\-]?"0"[xX][[:xdigit:]]+ {LEX_MSG("lexing hex int");
  *yylval=long_sexp((long)strtol(yytext,NULL,0));return TOK_INT;}
[+\-]?{DIGIT}+"."{DIGIT}* {LEX_MSG("lexing real");
  *yylval=real64_sexp(strtod(yytext,NULL));return TOK_REAL;}
[+\-]?{DIGIT}+"."?{DIGIT}*[eE][+-]?{DIGIT}+ {LEX_MSG("lexing real");
  *yylval=real64_sexp(strtod(yytext,NULL));return TOK_REAL;}
    /*String Literal a quote, followed by either a literal \"
   or anything that isnt a " repeated 1 or more times, followed by another quote.*/
"\""([\x00-0x7f]|"\\""\"")+"\"" {LEX_MSG("Lexing string");
  *yylval=cord_sexp(CORD_strdup(CORD_substr(yytext,1,CORD_len(yytext)-2)));
  return TOK_STRING;}
"\""([\x00-\x21\x23-\x7f]|{UONLY}|"\\""\"")+"\"" {LEX_MSG("Lexing multibyte string");
  *yylval=cord_sexp(CORD_strdup(CORD_substr(yytext,1,CORD_len(yytext)-2)));
  return TOK_STRING;}
"\"\"" {*yylval=cord_sexp(0);return TOK_STRING;}
{UCHAR} {LEX_MSG("lexing char");
    wint_t new_char;
    if(lex_char(yytext+1,&new_char)<0){
      return TOK_UNKN;
    } else {
      *yylval=uchar_sexp(new_char); return TOK_CHAR;
    }
  }
{ID} {LEX_MSG("lexing symbol");
    symbol *sym=c_intern(yytext,yyleng,NULL);
    *yylval=symref_sexp(sym);
    return TOK_SYMBOL;
   }
{KEYSYM} {LEX_MSG("lexing keyword symbol");CORD name=CORD_from_char_star(yytext);
  sexp temp=(sexp)getKeySymSexp(name);*yylval=temp;
  return TOK_KEYSYM;}
{QUALIFIED_ID} {LEX_MSG("lexing qualified id");
  /*know we've got a colon, so we can use rawmemchr*/
  char *colon=rawmemchr(yytext,':');
  symbol *package=c_intern(yytext,(uint32_t)(colon-yytext),NULL);
  if(!ENVP(package->val)){
    *yylval=format_error_sexp("unknown package %s",package->name.name);
    return TOK_ERR;
  }
  symbol_name sym_name(colon+1,(uint32_t)((yytext+yyleng)-(colon+1)),NULL);
  symbol *sym=obarray_lookup_sym(&sym_name,package->val.val.ob);
  if(!sym){
    FORMAT_READ_ERR("value %s not found in package %s",sym_name->name,
                                                  package->name->name);
    return handle_read_error();
  }
  if(sym->name->externally_visable == 2){
    FORMAT_READ_ERR
    ("value %s not externally visable in package %s (use :. to override visibility)",
    sym_name->name,package->name->name);
    return handle_read_error();
   }
   *yylval=symref_sexp(sym);
   return TOK_ID;
   }
{QUALIFIED_ID_ALL} {LEX_MSG("lexing qualified id");
  /*know we've got a colon, so we can use rawmemchr*/
  char *colon=rawmemchr(yytext,':');
  assert(*(colon+1) == '.');
  symbol *package=c_intern(yytext,(uint32_t)(colon-yytext),NULL);
  if(!ENVP(package->val)){
    FORMAT_READ_ERR("unknown package %s",package->name.name);
    return handle_read_error()
  }
  symbol_name sym_name(colon+2,(uint32_t)((yytext+yyleng)-(colon+2)),NULL);
  symbol *sym=obarray_lookup_sym(&sym_name,package->val.val.ob);
  if(!sym){
    FORMAT_READ_ERR("value %s not found in package %s",sym_name->name,
                                                  package->name->name);
    return handle_read_error();
  }
  *yylval=symref_sexp(sym);
  return TOK_ID;
  }
{TYPENAME} {LEX_MSG("lexing typename");*yylval=typeOfTag(parse_tagname(yytext+2));
  return TOK_TYPEINFO;}
  /*
{ID}{TYPENAME} {LEX_MSG("lexing typed id");
    char *colon=rawmemchr(yytext,':');
    assert(*(colon+1) == ':');
    symbol sym=c_intern(yytext,(uint32_t)(colon-yytext),NULL);
    }
    */
<comment,INITIAL>"#|" {LEX_MSG("lexing open comment");
  if(YY_START != comment){BEGIN(comment);}comment_depth+=1;}
<comment,INITIAL>"|#" {LEX_MSG("lexing close comment"); comment_depth-=1;
  if(comment_depth == 0){BEGIN(0);}}
<comment>[^#|]+
<comment>"#"[^|}|"|"[^#]
"quasiquote"|"`" {LEX_MSG("lexing backquote");return TOK_BACKQUOTE;}
"#" {LEX_MSG("HASH");return TOK_HASH;}
"#t" {*yyval=LISP_TRUE; return TOK_SYMBOL;}
"#f" {*yyval=LISP_FALSE; return TOK_SYMBOL;}
 /*uninterned symbol*/
"#:"{ID} {struct symbol_name *name=make_symbol_name(yytext+2,yyleng-2,0);
          struct symbol_new *sym=xmalloc(sizeof(struct symbol_new));
          sym->name=name;
          sym->val=UNDEFINED; /*define this at some point*/
          sym->next=NULL;
          sym->plist=Fcons(Quninterned,NIL);
          *yylval=symref_sexp(sym);
          return TOK_SYMBOL;
         }
 /*"#"{ID} {LEX_MSG("HASH-ID");return TOK_HASH;}*/
"(" {LEX_MSG("lexing (");return TOK_LPAREN;}
")" {LEX_MSG("lexing )");return TOK_RPAREN;}
  /*Maybe use the syntax #('['|'[['|'[|') for something*/
"[" {LEX_MSG("lexing [");return TOK_LBRACE;}
"[[" {LEX_MSG("lexing [[");return TOK_DBL_LBRACE;}
"[|" {LEX_MSG("lexing [|");return TOK_MAT_OPEN;}
"]" {LEX_MSG("lexing ]");return TOK_RBRACE;}
"]]" {LEX_MSG("lexing ]]");return TOK_DBL_RBRACE;}
"|]" {LEX_MSG("lexing |]");return TOK_MAT_CLOSE;}
"{" {LEX_MSG("lexing {");return TOK_LCBRACE;}
"}" {LEX_MSG("lexing }");return TOK_RCBRACE;}
"." {LEX_MSG("lexing .");return TOK_DOT;}
":" {LEX_MSG("lexing :");return TOK_COLON;}
"@" {LEX_MSG("lexing @");return TOK_STRUDEL;}
"'" {LEX_MSG("Lexing quote");return TOK_QUOTE;}
"," {LEX_MSG("Lexing comma");return TOK_COMMA;}
[ \t\n]+ /*whitespace*/
";"[^\n]* /*one line comments*/
<<EOF>> return -1;
. {LEX_MSG("unknown token");
  PRINT_FMT("Error, unknown token, value %d, character %c",
            yytext[0],yytext[0]);
  *yylval=uchar_sexp(btowc(yytext[0]));
  return TOK_UNKN;}

%%
//From this point on flex doesn't touch anything, so it's just normal C99
void *yyalloc(size_t bytes,void* yyscanner){
  return xmalloc(bytes);
}
void *yyrealloc(void *ptr,size_t bytes,void *yyscanner){
  return xrealloc(ptr,bytes);
}
void yyfree(void *ptr,void *yyscanner){
  return xfree(ptr);
}
sexp read_full(FILE *stream);
sexp c_read(yyscan_t *scanner,register sexp *yylval,int *last_tok);
sexp lisp_read(sexp lisp_stream){
  //really need to write a cord stream/test the one I already wrote
  FILE *stream;
  if(STRINGP(stream)){
    stream=fmemopen(CORD_to_char_star(code),CORD_len(code),"r");
  } else if (STREAMP(stream)){
    stream=lisp_stream.val.stream;
  } else {
    raise_simple_error_fmtf(Etype,"Invalid type passed to read, expected a stream");
  }
  return read_full(stream);
}
sexp read_full(FILE *stream){
  yyscan_t scanner;
  yylex_init(&scanner);
  yyset_in(scanner);
  sexp *yylval=xmalloc(sizeof(sexp));
  return c_read(&scanner,yylval,NULL);
}
sexp c_read_safe(yyscan_t *scanner,register sexp *yylval){
  frame *read_error_frame=make_frame(Eread,simple_error_frame);
  push_frame(current_env,read_error_frame);
  if(setjmp(read_error_frame->dest){
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
  return retval
}
sexp read_matrix(yyscan_t *scanner,register sexp *yylval,int *last_tok);
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
#define get_tok() (yytag = yylex(scanner,yylval,cur_env))
  register TOKEN yytag=0;
  register cons *ast=xmalloc(sizeof(cons));
  get_tok();
  while(1){
    switch(yytag){
      case TOK_RPAREN:
        ast->car=read_cons(scanner,yylval);
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
        sexp value=c_read(scanner,yylval,last_tok)
        return c_list2(Qquote,value);
        break;
      }
      case TOK_HASH:
        /*used for special read syntax*/
      case TOK_RPAREN:
      case TOK_RBRACE:
      case TOK_DBL_RBRACE:
      case TOK_MAT_CLOSE:
        *last_tok=yytag;
        return NIL;
      case TOK_BACKQUOTE:
        backtick_flag=1;
        sexp value=c_read(scanner,yylval,last_tok)
        backtick_flag=0;
        return c_list2(Qbackquote,value);
      case TOK_COMMA:{
        if(!backtick_flag){
          rasie_simple_error(Eread,"error comma not inside a backquote");
        }
        backtick_flag=0;
        sexp value=c_read(scanner,yylval,last_tok);
        backtick_flag=1;
        return c_list2(Qcomma,value);
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
    XCDR(new_list)=cons_sexp(xmalloc(sizeof(cons));
    newlist=XCDR(new_list);
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
  return handle_read_error();
  }
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
      arr=xrealloc(sizeof(sexp)*size);
    }
    val=c_read(scanner,yylval,&last_tok);
    arr[i++]=val;
  }
  if(last_tok != ']'){
    raise_simple_error(Eread,"Read error expected ']' at end of array");
    return handle_read_error();
  }
  lisp_array *retval=(lisp_array*)arr;
  retval->len=i;
  retval->dims=1;
  retval=type=_nil;
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
        arr=xrealloc(size*sizeof(data))
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
      arr->type=_int64;//for now
      break;
    case TOK_READ:
      arr->type=_real64;//for now
      break;
    case TOK_CHAR:
      arr->type=_uchar;
      break;
    case TOK_STRING:
      arr->type=_string;
      break;
  }
  return array_sexp(retval);
}

