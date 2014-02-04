#ifndef LISP_STRINGS_H
#define LISP_STRINGS_H
#include "common.h"
/*structure of strings in lisp,
  strings are generally immutable but can be mutable if desired,
  in general strings are kept internally in utf-8 encoding (ie multibyte)
  but we need to convert that into a wide character string before printing it
  so if necessary we can hold a wide character string as well*/
struct lisp_string {
  union {
    const char *string;
    const wchar_t *wstring;
    char *mut_string;
  };
  uint32_t len;//length in chars or wchars depending
  enum {
    str_string,
    str_wstring,
    str_mut_string,
  } string_type;
};
//make a cord out of a stream of bytes
static char* CORD_from_mem(size_t i,void *client_data){
  return ((char*)client_data)[i];
}
#endif
