/* Header file for unicode support 

   Copyright (C) 2013-2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
#ifndef UNICODE_H
#define UNICODE_H
#include "common.h"
//#define make_string(len,type,string_val) (lisp_string){.len=len,.type=type,.val={.string=string_val}}
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

int lex_char(char* cur_yytext,wint_t *new_char);
sexp lisp_char_to_string(sexp lisp_char);
sexp lisp_string_to_char(sexp lisp_str);
//pretty much a wrapper to wcrtomb which returns a vaild c string
const char *c_wchar_to_string(wchar_t lisp_char);
//destructively convert intput, return value is NULL on error
//and input otherwise
char *wcsrtombs_destructive(wchar_t *restrict input,mbstate_t *restrict state);
//convert a lisp_string to a c wide character string
wchar_t* lisp_string_to_wcs(lisp_string *string,int *out_len);
//pretty much a simplified wrapper to mbsrtowcs, which deals
//with memory allocation internally (using gc_malloc_atomic)
wchar_t* lisp_mbsrtowcs(char *restrict str,mbstate_t *restrict state);
#endif
