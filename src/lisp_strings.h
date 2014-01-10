#ifndef LISP_STRINGS_H
#define LISP_STRINGS_H
#include "common.h"
struct lisp_string {
  union {
    const char *string;
    const wchar_t *wstring;
    char *mut_string;
  };
  uint32_t len;
  enum {
    str_string,
    str_wstring,
    str_mut_string,
  };
}
#endif
