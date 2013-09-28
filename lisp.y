%{
#include "common.h"
#define YYSTYPE data
#define __USING_BISON__
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
%token <utf8_char> TOK_CHAR
%token TOK_LPAREN "("
%token TOK_RPAREN ")"
%token TOK_QUOTE "'"
%token TOK_DEF
%type <sexp> nil
%type <sexp> atom
%type <sexp> SEXP
%type <sexp> SEXPS
%type <sexp> CONS
%type <sexp> LIST
%type <sexp> input
%defines
%%
input:
%empty {$$=NIL;}
| SEXP input {$$=mklist($1,$2);}
SEXP:  
atom {$$=$1;}
| LIST {$$=$1;}
nil:
'('')' {sexp temp=NIL;$$=temp;}
atom:
TOK_REAL {sexp temp=(sexp){_double,(data)$1};$$=temp;}
|TOK_INT {sexp temp=(sexp){_long,(data)$1};$$=temp;}
|TOK_CHAR {sexp temp=(sexp){_utf8_char,(data)$1};$$=temp;}
CONS:
nil
| '(' SEXP '.' SEXP ')' {cons* temp_val=xmalloc(sizeof(cons));
  temp_val->car=$2,temp_val->cdr=$4;
  sexp temp=(sexp){_cons,(data)temp-val};$$=temp;}
LIST:
CONS {$$=$1;}
| '(' SEXPS ')' {$$=$2;}
SEXPS:
SEXP {$$=$1}
| SEXPS '.' SEXP {$$=mkImproper($1,NIL,$3);}
| SEXPS SEXP {$$=mklist($1,$2,NIL);}
%%
