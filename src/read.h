#ifndef _READ_H
#define _READ_H
//prevent the old generated header being included in the lexer
#ifndef IN_LEXER
#define YY_DECL TOKEN yylex(sexp *yylval,yyscan_t yyscanner)
#include "read_lex.h"
extern TOKEN yylex(sexp *yylval,yyscan_t yyscanner);
#endif
wchar_t parse_char(char *input);
int lex_char(char *input,wint_t *new_char);
wchar_t parse_escape(char *input);
int parse_escape_internal(char *input,wchar_t *output);
sexp read_full(FILE *stream);
sexp c_read(yyscan_t *scanner,register sexp *yylval,int *last_tok);
sexp lisp_read(sexp lisp_stream);
sexp read_full(FILE *stream);
sexp c_read_safe(yyscan_t *scanner,register sexp *yylval);
#endif
