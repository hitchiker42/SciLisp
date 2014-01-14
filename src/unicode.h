/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef UNICODE_H
#define UNICODE_H
#include "common.h"
#define make_string(len,type,string_val) (lisp_string)
/*structure of strings in lisp,
  strings are generally immutable but can be mutable if desired,
  in general strings are kept internally in utf-8 encoding (ie multibyte)
  but we need to convert that into a wide character string before printing it
  so if necessary we can hold a wide character string as well*/
struct lisp_string {
  uint32_t len;//length in chars or wchars depending
  enum {
    str_string,
    str_mbstring,
    str_cord,
  } string_type;
  union {
    const char *string;
    CORD cord;
  };
}
int lex_char(char* cur_yytext,wint_t *new_char);
sexp lisp_char_to_string(sexp lisp_char);
sexp lisp_string_to_char(sexp lisp_str);
//pretty much a wrapper to wcrtomb which returns a vaild c string
c_string c_wchar_to_string(wchar_t lisp_char);
//pretty much a simplified wrapper to mbsrtowcs, which deals
//with memory allocation internally (using gc_malloc_atomic)
wchar_t* lisp_mbsrtowcs(char *restrict str,mbstate_t *restrict state){
#endif
