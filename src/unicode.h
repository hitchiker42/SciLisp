/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
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

static inline lisp_string *make_string(const char *str){
  lisp_string *retval=xmalloc(sizeof(lisp_string));
  if(str[0] == '\0'){
    *retval=(lisp_string){.cord=str,.len=(CORD_len(str))}
  } else {
    *retval=(lisp_string){.string=str,.len=(strlen(str))}
  }
  return retval;
}
int lex_char(char* cur_yytext,wint_t *new_char);
sexp lisp_char_to_string(sexp lisp_char);
sexp lisp_string_to_char(sexp lisp_str);
//pretty much a wrapper to wcrtomb which returns a vaild c string
const char *c_wchar_to_string(wchar_t lisp_char);
//destructively convert intput, return value is NULL on error
//and input otherwise
char *wcsrtombs_destructive(wchar *restrict input,mbstate *restrict state);
//convert a lisp_string to a c wide character string
wchar_t* lisp_string_to_wcs(lisp_string *string,int *out_len);
//pretty much a simplified wrapper to mbsrtowcs, which deals
//with memory allocation internally (using gc_malloc_atomic)
wchar_t* lisp_mbsrtowcs(char *restrict str,mbstate_t *restrict state);
#endif
