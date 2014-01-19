/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef UNICODE_H
#define UNICODE_H
#include "common.h"
#define make_string(len,type,string_val) (lisp_string){.len=len,.type=type,.val={.string=string_val}}
#define isascii(char) (char>=0x00 && char <=0x7f)
#define isutf8_1(char) (char>=0x80 && char <=0xbf)
#define isutf8_2(char) (char>=0xc2 && char <=0xdf)
#define isutf8_3(char) (char>=0xe0 && char <=0xef)
#define isutf8_4(char) (char>=0xf0 && char <=0xf4)
#define isutf8(str)                                                     \
  (str && (isascii(str[0]) ||                                           \
           str+1 && ((isutf8_2(str[0]) && isutf8_1(str[1])) ||          \
                     str+2 && ((isutf8_3(str[0]) && isutf8_1(str[1])    \
                                && isutf8_1(str[2])) ||                 \
                               str+3 && ((isutf8_4(str[0]) && isutf8_1(str[1]) \
                                          && isutf8_1(str[2])) && isutf8_1(str[3])))))
/*structure of strings in lisp,
  strings immutable, we use cords for actions that would normally use mutable strings
  ie sprintf, concatenation, modifying substrings etc
  strings are kept internally in utf-8 encoding (ie multibyte)*/
struct lisp_string {
  uint32_t len;//length in chars or wchars depending
  enum {
    str_string,
    str_mbstring,
    str_cord,
  } string_type;
  //kinda a silly union since a CORD is technically a typedef for const char*
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
