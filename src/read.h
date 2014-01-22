#ifndef _READ_H
#define _READ_H
#ifndef IN_LEXER
#include "read_lex.h"
extern TOKEN yylex(sexp *yylval,yyscan_t yyscanner);
#endif
#ifdef YY_DECL
#undef YY_DECL
#endif
#define YY_DECL TOKEN yylex(sexp *yylval,yyscan_t yyscanner)
#define YYSTYPE sexp
wchar_t parse_char(char *input);
int lex_char(char *input,wint_t *new_char);
wchar_t parse_escape(char *input);
int parse_escape_internal(char *input,wchar_t *output);
#endif
