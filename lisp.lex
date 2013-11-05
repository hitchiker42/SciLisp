%{
/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#define YY_DECL TOKEN yylex(void)
  //woo non portable
union utf8_hack{
  char bytes[2];
  wchar_t wchar;
};
union utf8_hack utf8_escape={.wchar=L'\0'};
static wchar_t lex_char(char* cur_yytext){
  wchar_t result[1];
  mbstate_t state;
  size_t len,nbytes;
  char* cvt_str;
  memset(&state,'\0',sizeof(state));
  /* this is so horrendously non portable it's kinda funny,
     it relies on the size of chars,specific input and output encoding
     and little endian byte ordering*/
  if(cur_yytext[1]=='\\'){
    if(cur_yytext[2]=='?'){return '?';}
    if(cur_yytext[2]=='x'){
      //without this if you gave \x0000 you'd segfault
      char byte[3]={cur_yytext[3],cur_yytext[4],'\0'};
      utf8_escape.bytes[1]=0x00;
      utf8_escape.bytes[0]=(unsigned char)strtol(byte,NULL,16);
    } else if(cur_yytext[2]=='u'){
      char byte1[3]={cur_yytext[3],cur_yytext[4],'\0'};
      char byte2[3]={cur_yytext[5],cur_yytext[6],'\0'};
      utf8_escape.bytes[1]=(unsigned char)strtol(byte1,NULL,16);
      utf8_escape.bytes[0]=(unsigned char)strtol(byte2,NULL,16);
    }
    /*} else {
    utf8_escape.bytes[1]=0x00;
    utf8_escape.bytes[0]=cur_yytext[1];
    }*/
    PRINT_FMT("%lc",utf8_escape.wchar);
    return utf8_escape.wchar;
  } else {
      cvt_str=cur_yytext+1;
  } 
  if(0<=mbrtowc(result,cvt_str,strlen(cvt_str),&state)){
    return (wchar_t)result[0];
  } else {
    fprintf(stderr,"error lexing char, returning null\n");
    return (wchar_t)L'\0';
  }
}
 static int comment_depth=0;
%}
DIGIT [0-9]
HEX_DIGIT [0-9a-fA-f]
/*identifiers explicitly aren't # | : ; . , ' ` ( ) { } [ ]*/
ID [A-Za-z%+*!\-_^$/<>=/&][A-Za-z%+*!?\-_^$&<>0-9=/]*
TYPENAME "::"[A-z_a-z][A-Z_a-z0-9]*
QUOTE "'"|quote
UCHAR "?"("\\?"|"\\\\"|"\\x"([0-9a-fA-F]{2})|"\\u"([0-9a-fA-F]{4})|[^?\\])
KEYSYM ":"{ID}
/*
union data {
  double real64;
  long int64;
  wchar utf8_char;
  char* string;
  cons* pair;
  symref* var;//incldues functions
};
*/
/*%option bison-bridge*/
%option header-file="lex.yy.h"
%option noyywrap
   /*start conditon for scanning nested comments*/
%x comment
   /*start condition for scaning "`" quoted sexps*/
%x quasiquote 
%%
   /*Literals*/
[+\-]?{DIGIT}+ {LEX_MSG("lexing int");yylval->tag=_long;
  yylval->val.int64 = (long)strtol(yytext,NULL,10);return TOK_INT;}
[+\-]?"0"[xX]{HEX_DIGIT}+ {LEX_MSG("lexing hex int");yylval->tag=_long;
  yylval->val.int64=(long)strtol(yytext,NULL,16);return TOK_INT;}
[+\-]?{DIGIT}+"."{DIGIT}* {LEX_MSG("lexing real");yylval->tag=_double;
  yylval->val.real64 = strtod(yytext,NULL);
  return TOK_REAL;}
[+\-]?{DIGIT}+"."?{DIGIT}*[eE][+-]?{DIGIT}+ {LEX_MSG("lexing real");
  yylval->tag=_double;yylval->val.real64=strtod(yytext,NULL);
  return TOK_REAL;}
    /*String Literal a quote, followed by either a literal \"
   or anything that isnt a " repeated 1 or more times, followed by another quote.*/
"\""([^\"]|\/"\"")+"\"" {LEX_MSG("Lexing string");yylval->tag=_str;
  yylval->val.cord=CORD_strdup(CORD_substr(yytext,1,CORD_len(yytext)-2));
                               return TOK_STRING;}
{UCHAR} {LEX_MSG("lexing char");yylval->tag=_char;
  yylval->val.uchar=(wchar_t)lex_char(yytext);return TOK_CHAR;}
{KEYSYM} {LEX_MSG("lexing keyword symbol");CORD name=CORD_from_char_star(yytext);
  sexp temp=(sexp)getKeySymSexp(name);yylval->tag=temp.tag;yylval->val=temp.val;return TOK_KEYSYM;}
 /*Special forms, generating function at end of file*/
def(ine)? {LEX_MSG("lexing define");
  yylval->tag=_special;yylval->val.special=_def;return TOK_SPECIAL;}
defun {LEX_MSG("lexing defun");
  yylval->tag=_special;yylval->val.special=_defun;return TOK_LAMBDA;}
defvar {LEX_MSG("lexing defvar");
  yylval->tag=_special;yylval->val.special=_def;return TOK_SPECIAL;}
setq {LEX_MSG("lexing setq");
  yylval->tag=_special;yylval->val.special=_setq;return TOK_SPECIAL;}
datatype {LEX_MSG("lexing datatype");
  yylval->tag=_special;yylval->val.special=_datatype;return TOK_SPECIAL;}
union {LEX_MSG("lexing union");
  yylval->tag=_special;yylval->val.special=_union;return TOK_SPECIAL;}
enum {LEX_MSG("lexing enum");
      yylval->tag=_special;yylval->val.special=_enum;return TOK_SPECIAL;}
struct {LEX_MSG("lexing struct");
  yylval->tag=_special;yylval->val.special=_struct;return TOK_SPECIAL;}
go {LEX_MSG("lexing go");
  yylval->tag=_special;yylval->val.special=_go;return TOK_SPECIAL;}
tagbody {LEX_MSG("lexing tagbody");
  yylval->tag=_special;yylval->val.special=_tagbody;return TOK_SPECIAL;}
lambda {LEX_MSG("lexing lambda");
  yylval->tag=_special;yylval->val.special=_lambda;return TOK_LAMBDA;}
progn {LEX_MSG("lexing progn");
  yylval->tag=_special;yylval->val.special=_progn;return TOK_SPECIAL;}
prog1 {LEX_MSG("lexing prog1");
  yylval->tag=_special;yylval->val.special=_prog1;return TOK_SPECIAL;}
if {LEX_MSG("lexing if");
  yylval->tag=_special;yylval->val.special=_if;return TOK_SPECIAL;}
let {LEX_MSG("lexing let");
  yylval->tag=_special;yylval->val.special=_let;return TOK_SPECIAL;}
do {LEX_MSG("lexing do");
  yylval->tag=_special;yylval->val.special=_do;return TOK_SPECIAL;}
dolist {LEX_MSG("lexing dolist");
  yylval->tag=_special;yylval->val.special=_dolist;return TOK_SPECIAL;}
while {LEX_MSG("lexing while");
  yylval->tag=_special;yylval->val.special=_while;return TOK_SPECIAL;}
eval {LEX_MSG("lexing eval");
  yylval->tag=_special;yylval->val.special=_eval;return TOK_SPECIAL;}
main {LEX_MSG("lexing mainl");
  yylval->tag=_special;yylval->val.special=_main;return TOK_SPECIAL;}
defmacro {LEX_MSG("lexing defmacro");
  yylval->tag=_special;yylval->val.special=_defmacro;return TOK_MACRO;}
or {LEX_MSG("lexing or");
  yylval->tag=_special;yylval->val.special=_or;return TOK_SPECIAL;}
and {LEX_MSG("lexing and");
  yylval->tag=_special;yylval->val.special=_and;return TOK_SPECIAL;}
{QUOTE} {LEX_MSG("Lexing quote");
  yylval->tag=_special;yylval->val.special=_quote;return TOK_QUOTE;}
"quasiquote"|"`" {LEX_MSG("lexing quasiquote");
  yylval->tag=_special;yylval->val.special=_quasi;return TOK_QUASI;}
"," {LEX_MSG("Lexing comma");
  yylval->tag=_special;yylval->val.special=_comma;return TOK_COMMA;}
{TYPENAME} {LEX_MSG("lexing typename");yylval->tag=_str;
  yylval->val.cord=CORD_strdup(&yytext[2]);
  return TOK_TYPEINFO;}
<comment,INITIAL>"#|" {LEX_MSG("lexing open comment");
  if(YY_START != comment){BEGIN(comment);}comment_depth+=1;}
<comment,INITIAL>"|#" {LEX_MSG("lexing close comment"); comment_depth-=1;
  if(comment_depth == 0){BEGIN(0);}}
<comment>[^#|]+
<comment>"#"[^|}|"|"[^#]
"#t" {LEX_MSG("lexing true literal");return TOK_LISP_TRUE;}
"#f" {LEX_MSG("lexing false literal");return TOK_LISP_FALSE;}
{ID} {LEX_MSG("lexing ID");yylval->tag=_str;
  yylval->val.cord=CORD_strdup(yytext);return TOK_ID;}
",@" {LEX_MSG("lexing ,@");return TOK_LIST_SPLICE;}
"(" {LEX_MSG("lexing (");return TOK_LPAREN;}
")" {LEX_MSG("lexing )");return TOK_RPAREN;}
"[" {LEX_MSG("lexing [");return TOK_LBRACE;}
"]" {LEX_MSG("lexing ]");return TOK_RBRACE;}
"{" {LEX_MSG("lexing {");return TOK_LCBRACE;}
"}" {LEX_MSG("lexing }");return TOK_RCBRACE;}
"." {LEX_MSG("lexing .");return TOK_DOT;}
":" {LEX_MSG("lexing :");return TOK_COLON;}
"@" {LEX_MSG("lexing @");return TOK_AROBASE;}
    /*because really what else could I use, atsign, atmark, really?*/
[ \t\n]+ /*whitespace*/
";"[^\n]* /*one line comments*/
<<EOF>> return -1;

 /*(defun special (name) (insert (format "\n%s {LEX_MSG(\"lexing %s\");
   yylval->tag=_special;yylval->val.string=\"%s\"
   return TOK_%s;}" (downcase name) (downcase name) (downcase name) (upcase name))))
(dolist (name '("define" "defun" "setq" "datatype" "union" "enum" "struct" "go" "tagbody" "lamdba" "progn" "if" "let" "do" "quasiquote" "eval" "defmacro")) (special name))*/
