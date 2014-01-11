/* -*- c-syntactic-indentation: nil; -*- */
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

lisp_read(sexp lisp_stream){
  //really need to write a cord stream/test the one I already wrote
  FILE *stream;
  if(STRINGP(stream)){
    stream=fmemopen(CORD_to_char_star(code),CORD_len(code),"r");
  } else if (STREAMP(stream)){
    stream=lisp_stream.val.stream;
  } else {
    return format_type_error("read","stream",lisp_stream.tag);
  }
  return read_init(stream);
}
read_init(FILE *stream){
  yyscan_t scanner;
  yylex_init(&scanner);
  yyset_in(scanner);
  sexp *yylval=xmalloc(sizeof(sexp));
  return read(&scanner,yylval);
  }
sexp read_sub(yyscan_t *scanner,register sexp *yylval,int *last_tok);

sexp read_sub(yyscan_t *scanner,register sexp *yylval,int *last_tok){
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
      case TOK_QUOTE:
        ast->car=Qquote;
        break;
        case TOK_HASH:
        //...
      case TOK_RPAREN:
      case TOK_RBRACE:
      case TOK_DBL_RBRACE
        *last_tok=yytag;
        return NIL;
    }
    get_tok();
    if(yytag>0){
      ast->cdr=cons_sexp(xmalloc(sizeof(cons)));
      ast=ast->cdr;
      continue;
    } else {
      return cons_sexp(ast);
    }
  }
}
sexp read_list(yyscan_t *scanner,register sexp *yylval,int *last_tok){}
struct matrix {
  uint8_t ndims;//imposes a cap on dimensions of 256
  uint8_t element_size;
  uint16_t padding;
  enum {
    dbl_matrix,
    flt_matrix,
    dbl_complex_matrix,
    flt_complex_matrix,
    int8_matrix,
    int16_matrix,
    int32_matrix,
    int64_matrix,
    uint8_matrix,
    uint16_matrix,
    uint32_matrix,
    uint64_matrix,
  } element_type;
  //no indirecton for vectors or matrices, but allow higher dimensions
  union {
    struct {
      uint32_t rows;
      uint32_t cols;
    };
    uint32_t *dims;
    uint64_t length;//for vectors
  };
  void *data;//put at end, because it's eaiser that way
}
sexp read_matrix(yyscan_t *scanner,register sexp *yylval,int *last_tok)
