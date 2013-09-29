%{
#include "common.h"
#include "lex.yy.h"
#define YYSTYPE sexp
#define __USING_BISON__
  void yyerror(sexp* ast,const char* s);
  /*union data {
  double real64;
  long int64;
  wchar utf8_char;
  char* string;
  cons* pair;
  symref* var;//incldues functions
  };*/
%}
%token TOK_REAL
%token TOK_INT
%token TOK_ID
%token TOK_TYPEINFO
%token TOK_CHAR
%token TOK_LPAREN "("
%token TOK_RPAREN ")"
%token TOK_QUOTE "'"
%token TOK_DEF
%parse-param {sexp *ast}
%defines
%%
input:
%empty {$$=NIL;}
| SEXP input {PRINT_MSG("Parsing input");*ast=mklist($1,$2);}
SEXP:  
atom {HERE();$$=$1;}
| LIST {HERE();$$=$1;}
nil:
'('')' {sexp temp=NIL;$$=temp;}
atom:
TOK_REAL {$$=$1;}//{sexp temp=(sexp){_double,(data)$1};$$=temp;}
|TOK_INT {$$=$1;}//{sexp temp=(sexp){_long,(data)$1};$$=temp;}
|TOK_CHAR {$$=$1;}//{sexp temp=(sexp){_utf8_char,(data)$1};$$=temp;}
CONS:
nil
| '(' SEXP '.' SEXP ')' {PRINT_MSG("Parsing a cons cell");
  cons* temp_val=xmalloc(sizeof(cons));
  temp_val->car=$2,temp_val->cdr=$4;
  sexp temp=(sexp){_cons,(data)temp_val};$$=temp;}
LIST:
CONS {$$=$1;}
| '(' SEXPS ')' {$$=$2;}
SEXPS:
SEXP {$$=$1;}
| SEXPS '.' SEXP {$$=mkImproper($1,NIL,$3);}
| SEXPS SEXP {$$=mklist($1,$2,NIL);}
%%
void yyerror(sexp* ast,const char* s){
  PRINT_MSG("Running yyerror");
  const char* temp=princ(*ast);
  fprintf(stderr,"%s\n",temp);  
  PRINT_MSG("yyerror run");
}
