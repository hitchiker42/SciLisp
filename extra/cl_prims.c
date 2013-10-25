#include "common.h"
#define even_oddp(e_or_o,not)                      \
  sexp e_or_o##p(sexp num){                     \
  if(!LONGP(num)){                               \
  return error_sexp("argument to" #e_or_o"p must be an integer");       \
  }                                                                     \
    if( not (num.val.int64 % 2)){                                       \
      return LISP_TRUE;                                                 \
    }\
    return LISP_FALSE;                          \
  }
even_oddp(even,)
even_oddp(odd,!)
