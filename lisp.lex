%{
  #include "common.h"
  #define YYSTYPE data
  #include "lisp.tab.h"

%}
DIGIT [0-9]
ID [A-Za-z%+*!?\-_^$/][A-Za-z%+*!?\-_^$0-9]*
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
%option bison-bridge
%option header-file="lex.yy.h"
%option noyywrap
%%
{DIGIT}+ {PRINT_MSG("lexing int");yylval->int64 = strtol(yytext,NULL,10);return TOK_INT;}
{DIGIT}+"."{DIGIT}* {PRINT_MSG("lexing real");yylval->real64 = strtod(yytext,NULL);
  return TOK_REAL;}
def(ine)? PRINT_MSG("lexing define");return TOK_DEF;
"#". {PRINT_MSG("lexing char");yylval->utf8_char=(wchar_t)(yytext[1]); 
  return TOK_CHAR;}
{TYPENAME} {PRINT_MSG("lexing typename");yylval->string=strdup(&yytext[2]);
  return TOK_TYPEINFO;}
{QUOTE} {PRINT_MSG("Lexing quote");return TOK_QUOTE;}
{ID} PRINT_MSG("lexing ID");yylval->string=strdup(yytext);return TOK_ID;
"(" return TOK_LPAREN;
")" return TOK_RPAREN;
[ \t\n]+
";"[^\n]*
<<EOF>> return -1;





