/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef UNICODE_H
#define UNICODE_H
#include "common.h"
wchar_t lex_char(char* cur_yytext);
sexp lisp_char_to_string(sexp lisp_char);
sexp lisp_string_to_char(sexp lisp_str);
#endif
