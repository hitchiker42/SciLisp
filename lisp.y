%{
#include "common.h"
#define YYSTYPE sexp
  /*union data {
  double real64;
  long int64;
  wchar utf8_char;
  char* string;
  cons* pair;
  symref* var;//incldues functions
  };*/
%}
%token <real64> TOK_REAL
%token <int64> TOK_INT
%token <string> TOK_ID
%token <string> TOK_TYPEINFO
%token TOK_LPAREN "("
%token TOK_RPAREN ")"
%token TOK_DEF
%defines
%%
input:
  %empty
 | '\n' input
%%
