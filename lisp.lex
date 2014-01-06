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
 /*#define YY_DECL TOKEN yylex(void)*/
#define YYSTYPE sexp
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
UCHAR "?"("\\"[?nt]|"\\\\"|"\\x"([[:xdigit:]]{2})|"\\u"([[:xdigit:]]{4})|{UANY})
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
%option noyywrap
   /*start condition for scanning nested comments*/
%x comment
   /*start condition for matching typenames*/ 
%x typename
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
"\""([^\"]|\/"\"")+"\"" {LEX_MSG("Lexing string");
  *yylval=cord_sexp(CORD_strdup(CORD_substr(yytext,1,CORD_len(yytext)-2)));
  return TOK_STRING;}
{UCHAR} {LEX_MSG("lexing char");
  *yylval=uchar_sexp((wchar_t)lex_char(yytext));return TOK_CHAR;}
{KEYSYM} {LEX_MSG("lexing keyword symbol");CORD name=CORD_from_char_star(yytext);
  sexp temp=(sexp)getKeySymSexp(name);*yylval=temp;
  return TOK_KEYSYM;}
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
  *yylval=spec_sexp(_def);return TOK_SPECIAL;}
defun {LEX_MSG("lexing defun");
  *yylval=spec_sexp(_defun);return TOK_LAMBDA;}
defvar {LEX_MSG("lexing defvar");
  *yylval=spec_sexp(_def);return TOK_SPECIAL;}
defmacro {LEX_MSG("lexing defmacro");
  *yylval=spec_sexp(_defmacro);return TOK_MACRO;}
setq {LEX_MSG("lexing setq");
  *yylval=spec_sexp(_setq);return TOK_SPECIAL;}
 /* datatype {LEX_MSG("lexing datatype");
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
 yylval->tag=_special;yylval->val.special=_tagbody;return TOK_SPECIAL;}*/
lambda {LEX_MSG("lexing lambda");
  *yylval=spec_sexp(_lambda);return TOK_LAMBDA;}
progn {LEX_MSG("lexing progn");
  *yylval=spec_sexp(_progn);return TOK_SPECIAL;}
prog1 {LEX_MSG("lexing prog1");
  *yylval=spec_sexp(_prog1);return TOK_SPECIAL;}
if {LEX_MSG("lexing if");
  *yylval=spec_sexp(_if);return TOK_SPECIAL;}
let {LEX_MSG("lexing let");
  *yylval=spec_sexp(_let);return TOK_LET;}
flet {LEX_MSG("lexing flet");
  *yylval=spec_sexp(_flet);return TOK_LET;}
do {LEX_MSG("lexing do");
  *yylval=spec_sexp(_do);return TOK_SPECIAL;}
dolist {LEX_MSG("lexing dolist");
  *yylval=spec_sexp(_dolist);return TOK_SPECIAL;}
while {LEX_MSG("lexing while");
  *yylval=spec_sexp(_while);return TOK_SPECIAL;}
main {LEX_MSG("lexing mainl");
  *yylval=spec_sexp(_main);return TOK_SPECIAL;}
                   /*or {LEX_MSG("lexing or");
  yylval->tag=_special;yylval->val.special=_or;return TOK_SPECIAL;}
                   and {LEX_MSG("lexing and");
                     yylval->tag=_special;yylval->val.special=_and;return TOK_SPECIAL;}*/
return {LEX_MSG("lexing return");
  *yylval=spec_sexp(_return);return TOK_SPECIAL;}
dotimes {LEX_MSG("lexing dotimes");
  *yylval=spec_sexp(_dotimes);return TOK_SPECIAL;}
{QUOTE} {LEX_MSG("Lexing quote");
  *yylval=spec_sexp(_quote);return TOK_QUOTE;}
"quasiquote"|"`" {LEX_MSG("lexing quasiquote");
  *yylval=spec_sexp(_quasi);return TOK_QUASI;}
"," {LEX_MSG("Lexing comma");
  *yylval=spec_sexp(_comma);return TOK_COMMA;}
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
{ID} {LEX_MSG("lexing ID");
  *yylval=cord_sexp(CORD_strdup(yytext));return TOK_ID;}
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
. {LEX_MSG("unknown token");
  PRINT_FMT("Error, unknown token, value %d, character %c",
            yytext[0],yytext[0]);
  *yylval=uchar_sexp(btowc(yytext[0]));
  return TOK_UNKN;}

 /*(defun special (name) (insert (format "\n%s {LEX_MSG(\"lexing %s\");
   yylval->tag=_special;yylval->val.string=\"%s\"
   return TOK_%s;}" (downcase name) (downcase name) (downcase name) (upcase name))))
(dolist (name '("define" "defun" "setq" "datatype" "union" "enum" "struct" "go" "tagbody" "lamdba" "progn" "if" "let" "do" "quasiquote" "eval" "defmacro")) (special name))*/
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
