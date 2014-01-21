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
enum string_type {
    str_string,
    str_mbstring,
    str_cord,
};
struct lisp_string {
  union {
    const char *string;
    CORD cord;
  };
  uint32_t len;//length in bytes (i.e. for multibyte strings not the length in chars)
  uint8_t type;
  //kinda a silly union since a CORD is technically a typedef for const char*
  //but it makes code clearer in places

};
static inline lisp_string *make_string(const char *str){
  lisp_string retval=xmalloc(sizeof(lisp_string));
  if(str[0] == '\0'){
    *retval=(lisp_string){.cord=str,.len=CORD_len(str),.type=str_cord};
  } else {
    *retval=(lisp_string){.string=str,.len=strlen(str),.type=str_string};
  }
  return retval;
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
