%{
/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#define YY_DECL TOKEN yylex(void)
%}
DIGIT [0-9]
HEX_DIGIT [0-9a-fA-f]
/*identifiers explicitly aren't # | : ; . , ' ` ( ) { } [ ]*/
ID [A-Za-z%+*!?\-_^$/&<>=/][A-Za-z%+*!?\-_^$&<>0-9=/]*
TYPENAME "::"[A-z_a-z][A-Z_a-z0-9]*
QUOTE "'"|quote
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
  yylval->val.cord=CORD_strdup(yytext);return TOK_STRING;}
"#"("\\|"|"\\#"|"\\\""|[^|#]) {LEX_MSG("lexing char");yylval->tag=_char;
    yylval->val.utf8_char=(wchar_t)(yytext[1]);return TOK_CHAR;}
 /*Special forms, generating function at end of file*/
def(ine)? {LEX_MSG("lexing define");
  yylval->tag=_special;yylval->val.special=_def;return TOK_SPECIAL;}
defun {LEX_MSG("lexing defun");
  yylval->tag=_special;yylval->val.special=_defun;return TOK_LAMBDA;}
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
if {LEX_MSG("lexing if");
  yylval->tag=_special;yylval->val.special=_if;return TOK_SPECIAL;}
let {LEX_MSG("lexing let");
  yylval->tag=_special;yylval->val.special=_let;return TOK_SPECIAL;}
do {LEX_MSG("lexing do");
  yylval->tag=_special;yylval->val.special=_do;return TOK_SPECIAL;}
while {LEX_MSG("lexing while");
  yylval->tag=_special;yylval->val.special=_while;return TOK_SPECIAL;}
quasiquote|"`" {LEX_MSG("lexing quasiquote");
  yylval->tag=_special;yylval->val.special=_quasi;return TOK_SPECIAL;}
eval {LEX_MSG("lexing eval");
  yylval->tag=_special;yylval->val.special=_eval;return TOK_SPECIAL;}
main {LEX_MSG("lexing mainl");
  yylval->tag=_special;yylval->val.special=_main;return TOK_SPECIAL;}
defmacro {LEX_MSG("lexing defmacro");
  yylval->tag=_special;yylval->val.special=_defmacro;return TOK_SPECIAL;}
or {LEX_MSG("lexing or");
  yylval->tag=_special;yylval->val.special=_or;return TOK_SPECIAL;}
and {LEX_MSG("lexing and");
  yylval->tag=_special;yylval->val.special=_and;return TOK_SPECIAL;}
{QUOTE} {LEX_MSG("Lexing quote");
  yylval->tag=_special;yylval->val.special=_quote;return TOK_QUOTE;}
"," {LEX_MSG("Lexing comma");
  yylval->tag=_special;yylval->val.special=_comma;return TOK_SPECIAL;}
{TYPENAME} {LEX_MSG("lexing typename");yylval->tag=_str;
  yylval->val.cord=CORD_strdup(&yytext[2]);
  return TOK_TYPEINFO;}
"#|" {LEX_MSG("lexing open comment");return TOK_COMMENT_START;}
"|#" {LEX_MSG("lexing close comment");return TOK_COMMENT_END;}
{ID} {LEX_MSG("lexing ID");yylval->tag=_str;
  yylval->val.cord=CORD_strdup(yytext);return TOK_ID;}
"(" {LEX_MSG("lexing (");return TOK_LPAREN;}
")" {LEX_MSG("lexing )");return TOK_RPAREN;}
"[" {LEX_MSG("lexing [");return TOK_LBRACE;}
"]" {LEX_MSG("lexing ]");return TOK_RBRACE;}
"{" {LEX_MSG("lexing {");return TOK_LCBRACE;}
"}" {LEX_MSG("lexing }");return TOK_RCBRACE;}
"." {LEX_MSG("lexing .");return TOK_DOT;}
":" {LEX_MSG("lexing :");return TOK_COLON;}
[ \t\n]+ /*whitespace*/
";"[^\n]* /*one line comments*/
<<EOF>> return -1;

 /*(defun special (name) (insert (format "\n%s {LEX_MSG(\"lexing %s\");
   yylval->tag=_special;yylval->val.string=\"%s\"
   return TOK_%s;}" (downcase name) (downcase name) (downcase name) (upcase name))))
(dolist (name '("define" "defun" "setq" "datatype" "union" "enum" "struct" "go" "tagbody" "lamdba" "progn" "if" "let" "do" "quasiquote" "eval" "defmacro")) (special name))*/
