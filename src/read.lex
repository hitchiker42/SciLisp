/* -*- c-syntactic-indentation: nil; electric-indent-inhibit: t -*- */
%{
/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#define IN_LEXER
#include "common.h"
#include "prim.h"
#include "unicode.h"
#ifdef YY_DECL
#undef YY_DECL
#endif
#define YY_DECL TOKEN yylex(sexp *yylval,yyscan_t yyscanner)
#define YYSTYPE sexp
static int comment_depth=0;
static thread_local backtick_flag=0;
%}
 /*\x5c == \ & \x22 == " & \x27 == '
  mostly so unmatched quotes don't fuck up syntax highlighing*/
 /*Unicode support, basic idea borrowed from the lexer for
  *the txr by kaz kylheku language*/
ASC     [\x00-\x7f]{-}[?\\]
ASCSTR  [\x00-\x21\x23-\x7f] /*anything not a "*/
 /*explaination of special characters:
  * : reserved for keywords, pacakge access and type annotations
  * ; reserved for comments
  * (){}[] reserved delimiers, () should be obvious, [] for arrays and {} for future use
  * ' " ' quotes, obviously
  * # for quoting and delimiting special symbols(#t,#f) and deliberately unreaable symbols
  * . reserved for pairs and package access
  * | nothing now but reserved for later
  * @ for ,@ in macros
  * , for macros
  * Starting special characters ? for characters, +-[0-9] for numbers
  */
DIGIT [0-9]
XDIGIT [0-9a-fA-F]
ASCSYM_REST  [\x00-\x7f]{-}[\][#|,.;:\x27\x22\x5c`(){}@]
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
ID ({ASCSYM_REST}*) /*just make sure numbers and characters get lexed first*/
KEYSYM ":"{ID}
TYPENAME "::"{ID}
QUALIFIED_ID {ID}":"{ID}
QUALIFIED_ID_ALL {ID}":."{ID}
%option noyywrap
   /*start condition for scanning nested comments*/
%x comment
%option noyyalloc noyyrealloc noyyfree
%option reentrant
%option header-file="lex.yy.h"
%%   /*Literals*/
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
{KEYSYM} {LEX_MSG("lexing keyword symbol");CORD name=CORD_from_char_star(yytext);
  sexp temp=(sexp)getKeySymSexp(name);*yylval=temp;
  return TOK_KEYSYM;}
{ID} {LEX_MSG("lexing symbol";
    struct symbol_new sym=c_intern(yytext,yyleng);
    *yylval=symref_sexp(sym);
    return TOK_SYMBOL;
  }
 /* new way to do symbols
  {SYM} {

   else //not sure what to do here{XCDR(cur_env)=cur_env;XCAR(cur_env)=lookup_

  everything below this except ` ' , and comments gets deleted;
 /*Special forms, generating function at end of file*/
 /* replace all the def(...) 's  with this at some point:*/
 /*not sure what to do about this*/
{TYPENAME} {LEX_MSG("lexing typename");*yylval=typeOfTag(parse_tagname(yytext+2));
  return TOK_TYPEINFO;}
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
          sym->plist=Cons(Quninterned,NIL);
          *yylval=symref_sexp(sym);
          return TOK_SYMBOL;
         }
"(" {LEX_MSG("lexing (");return TOK_LPAREN;}
")" {LEX_MSG("lexing )");return TOK_RPAREN;}
"[" {LEX_MSG("lexing [");return TOK_LBRACE;}
"[[" {LEX_MSG("lexing [[");return TOK_DBL_LBRACE;}
"]" {LEX_MSG("lexing ]");return TOK_RBRACE;}
"]]" {LEX_MSG("lexing ]]");return TOK_DBL_RBRACE;}
"{" {LEX_MSG("lexing {");return TOK_LCBRACE;}
"}" {LEX_MSG("lexing }");return TOK_RCBRACE;}
"." {LEX_MSG("lexing .");return TOK_DOT;}
":" {LEX_MSG("lexing :");return TOK_COLON;}
"@" {LEX_MSG("lexing @");return TOK_STRUDEL;}
'\''{LEX_MSG("Lexing quote");return TOK_QUOTE;}
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
sexp lisp_read(sexp lisp_stream){
  //really need to write a cord stream/test the one I already wrote
  FILE *stream;
  if(STRINGP(stream)){
    stream=fmemopen(CORD_to_char_star(code),CORD_len(code),"r");
  } else if (STREAMP(stream)){
    stream=lisp_stream.val.stream;
  } else {
    return format_type_error("read","stream",lisp_stream.tag);
  }
  return read_full(stream);
}
sexp read_full(FILE *stream){
  yyscan_t scanner;
  yylex_init(&scanner);
  yyset_in(scanner);
  sexp *yylval=xmalloc(sizeof(sexp));
  return c_read(&scanner,yylval);
}
sexp c_read(yyscan_t *scanner,register sexp *yylval,int *last_tok);
//reads one sexp, an error if c_read returns a non-zero value in last_tok
sexp c_read_sub(yyscan_t *scanner,register sexp *yylval){
  int last_tok;
  sexp retval=c_read(scanner,yylval,&last_tok);
  if(last_tok){
    return error_sexp("invalid read syntax");
  }
  return retval
}
//reads one sexp or token(i.e ',',''',')',']','}','.')
//so read list/vector/etc can be called as
/* int last_tok=0;
  while(!last_tok){
  c_read(scanner,yylval,&last_tok);
  do something...
  }
*/
sexp c_read(yyscan_t *scanner,register sexp *yylval,int *last_tok){
#define get_tok() (yytag = yylex(scanner,yylval,cur_env))
  register TOKEN yytag=0;
  register cons *ast=xmalloc(sizeof(cons));
  get_tok();
  while(1){
    switch(yytag){
      case TOK_RPAREN:
        ast->car=read_cons(scanner,cur_env,yylval);
        break;
      case TOK_RBRACE:
        ast->car=read_array(scanner,cur_env,yylval);
        break;
      case TOK_DBL_RBRACE:
        ast->car=read_matrix(scanner,cur_env,yylval);
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
        //...
      case TOK_RPAREN:
      case TOK_RBRACE:
      case TOK_DBL_RBRACE
        *last_tok=yytag;
        return NIL;
      case TOK_BACKQUOTE:
        backtick_flag=1;
        sexp value=c_read(scanner,yylval,last_tok)
        if(ERRORP(value)){return value;}
        return c_list2(Qbackquote,value);
      case TOK_COMMA:{
        sexp value=c_read(scanner,yylval,last_tok);
        
    }
    /*
    get_tok();
    if(yytag>0){
      ast->cdr=cons_sexp(xmalloc(sizeof(cons)));
      ast=ast->cdr;
      continue;
    } else {*/
      return cons_sexp(ast);
//    }
  }
}
sexp read_list(yyscan_t *scanner,register sexp *yylval){
  sexp retval;
  sexp new_list=retval=cons_sexp(xmalloc(sizeof(cons)));
  int last_tok=0;
  while(!last_tok){
    XCAR(new_list)=read(scanner,yylval,&last_tok);
    XCDR(new_list)=cons_sexp(xmalloc(sizeof(cons));
    newlist=XCDR(new_list);
  }
  switch(last_tok){
    case ')':
      new_list=NIL;
      return retval;
    case '.':
      new_list=read(scanner,yylval,&last_tok);
      read(scanner,yylval,&last_tok);
      if(last_tok != TOK_RPAREN){
        break;
      }
      return retval;
  }
  return error_sexp("read error, expected ')' or '.' at end of list");
  }
}
