%{
  #include "common.h"
  #include "lisp.tab.h"
%}
DIGIT [0-9]
ID [A-Za-z%+*!?\-_^$][A-Za-z%+*!?\-_^$0-9]*
TYPENAME [A-z_a-z][A-Z_a-z0-9]*
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
%option bison-bridge
%%
{DIGIT}+ yylval.int64 = strtol(yytext,NULL,10);return TOK_INT;
{DIGIT}+"."{DIGIT}* yylval.real64 = strtod(yytext,NULL);return TOK_REAL;
{ID} yylval.string=strdup(yylval);return TOK_ID;
def|define return TOK_DEF;
::?{TYPENAME} yylval.string=strdup(yylval[3]);return TOK_TYPEINFO;
"(" return TOK_LPAREN;
")" return TOK_RPAREN
[ \t\n]+
";"[^\n]*
