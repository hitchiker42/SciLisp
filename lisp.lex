%{
/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "prim.h"
#include "unicode.h"
#define YY_DECL TOKEN yylex(void)
#if 0
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
#endif
 static int comment_depth=0;
%}
DIGIT [0-9]
/*identifiers explicitly aren't # | : ; . , ' ` ( ) { } [ ]*/
ID [A-Za-z%+*!\-_^$/<>=/&][A-Za-z%+*!?\-_^$&<>0-9=/]*
TYPENAME "::"{ID}
QUALIFIED_ID {ID}":"{ID}
QUALIFIED_ID_ALL {ID}":."{ID}
INVALID_ID_TOKENS [\][#|:;.2''`(){}]
QUOTE "'"|quote
/*this is kinda special, note that the catchall case of this is a negated 
  character class, this means any raw bytes (128-255) will be matched, letting
  us scan unicode characters*/
ASC     [\x00-\x7f]{-}[?\\]
ASCN    [\x00-\t\v-\x7f]
U       [\x80-\xbf]
U2      [\xc2-\xdf]
U3      [\xe0-\xef]
U4      [\xf0-\xf4]
UANY    {ASC}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UANYN   {ASCN}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U} 
UONLY   {U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UCHAR "?"("\\?"|"\\\\"|"\\x"([[:xdigit:]]{2})|"\\u"([[:xdigit:]]{4})|{UANY})
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
   /*start condition for scanning nested comments*/
%x comment
   /*start condition for matching typenames*/ 
%x typename
%%
   /*Literals*/
[+\-]?{DIGIT}+ {LEX_MSG("lexing int");yylval->tag=_long;
  yylval->val.int64 = (long)strtol(yytext,NULL,0);return TOK_INT;}
[+\-]?"0"[xX][[:xdigit:]]+ {LEX_MSG("lexing hex int");yylval->tag=_long;
  yylval->val.int64=(long)strtol(yytext,NULL,0);return TOK_INT;}
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
 /* replace all the def(...) 's  with this at some point:
    def(ine|fun|macro|var|const)? {LEX_MSG("lexing define");
    yylval->tag=special;
    switch(yytext[4]){
    case 'i': 
    case 'v':
    case '\0':
      yylval->val.special=_def;
      return TOK_SPECIAL;
    case 'c':
    yylval->val.special=_defconst:/*need to add this somehow/
    return TOK_SPECIAL;
    case 'f':
      yylval->val.special=_defun;
      return TOK_LAMBDA;
    case 'm':
      yylval->val.special=_defmacro:
      return TOK_MACRO;
    }
    }*/
def(ine)? {LEX_MSG("lexing define");
  yylval->tag=_special;yylval->val.special=_def;return TOK_SPECIAL;}
defun {LEX_MSG("lexing defun");
  yylval->tag=_special;yylval->val.special=_defun;return TOK_LAMBDA;}
defvar {LEX_MSG("lexing defvar");
  yylval->tag=_special;yylval->val.special=_def;return TOK_SPECIAL;}
defmacro {LEX_MSG("lexing defmacro");
  yylval->tag=_special;yylval->val.special=_defmacro;return TOK_MACRO;}
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
  yylval->tag=_special;yylval->val.special=_let;return TOK_LET;}
flet {LEX_MSG("lexing flet");
  yylval->tag=_special;yylval->val.special=_flet;return TOK_LET;}
do {LEX_MSG("lexing do");
  yylval->tag=_special;yylval->val.special=_do;return TOK_SPECIAL;}
dolist {LEX_MSG("lexing dolist");
  yylval->tag=_special;yylval->val.special=_dolist;return TOK_SPECIAL;}
while {LEX_MSG("lexing while");
  yylval->tag=_special;yylval->val.special=_while;return TOK_SPECIAL;}
main {LEX_MSG("lexing mainl");
  yylval->tag=_special;yylval->val.special=_main;return TOK_SPECIAL;}
                   /*or {LEX_MSG("lexing or");
  yylval->tag=_special;yylval->val.special=_or;return TOK_SPECIAL;}
                   and {LEX_MSG("lexing and");
                     yylval->tag=_special;yylval->val.special=_and;return TOK_SPECIAL;}*/
return {LEX_MSG("lexing return");
  yylval->tag=_special;yylval->val.special=_return;return TOK_SPECIAL;}
dotimes {LEX_MSG("lexing dotimes");
  yylval->tag=_special;yylval->val.special=_dotimes;return TOK_SPECIAL;}
{QUOTE} {LEX_MSG("Lexing quote");
  yylval->tag=_special;yylval->val.special=_quote;return TOK_QUOTE;}
"quasiquote"|"`" {LEX_MSG("lexing quasiquote");
  yylval->tag=_special;yylval->val.special=_quasi;return TOK_QUASI;}
"," {LEX_MSG("Lexing comma");
  yylval->tag=_special;yylval->val.special=_comma;return TOK_COMMA;}
{TYPENAME} {LEX_MSG("lexing typename");*yylval=typeOfTag(parse_tagname(yytext+2));
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
                   /*",@" {LEX_MSG("lexing ,@");return TOK_LIST_SPLICE;}*/
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
"@" {LEX_MSG("lexing @");return TOK_AROBASE;}
    /*because really what else could I use, atsign, atmark, really?*/
[ \t\n]+ /*whitespace*/
";"[^\n]* /*one line comments*/
<<EOF>> return -1;
. {LEX_MSG("unknown token");PRINT_MSG("Error, unknown token");return TOK_UNKN;}

 /*(defun special (name) (insert (format "\n%s {LEX_MSG(\"lexing %s\");
   yylval->tag=_special;yylval->val.string=\"%s\"
   return TOK_%s;}" (downcase name) (downcase name) (downcase name) (upcase name))))
(dolist (name '("define" "defun" "setq" "datatype" "union" "enum" "struct" "go" "tagbody" "lamdba" "progn" "if" "let" "do" "quasiquote" "eval" "defmacro")) (special name))*/
