#ifndef _SEQUENCE_H
#define _SEQUENCE_H
#include "cons.h"
#include "array.h"
#define make_sequence_function(name)                    \
  sexp sequence_##name(sexp seq){                       \
  if(!SEQUENCEP(seq)){                                  \
    return format_type_error(#name,"sequence",seq.tag); \
  }                                                     \
    switch(seq.tag){                                    \
    case _array:                                        \
      return array_##name(seq);                         \
    case _list:                                         \
    case _cons:                                         \
      return cons_##name(seq);                          \
    }                                                   \
    }
#define make_sequence_function2(name)                    \
  sexp sequence_##name(sexp seq,sexp obj2){              \
    if(!SEQUENCEP(seq)){                                 \
    return format_type_error(#name,"sequence",seq.tag); \
    }                                                   \
    switch(seq.tag){                                    \
    case _array:                                        \
      return _array_##name(seq,obj2);                   \
    case _list:                                         \
    case _cons:                                         \
      return cons_##name(seq,obj2);                     \
    }                                                   \
    }
#define make_sequence_function3(name)                    \
  sexp sequence_##name(sexp seq,sexp obj2,sexp obj3){    \
    if(!SEQUENCEP(seq)){                                 \
    return format_type_error(#name,"sequence",seq.tag); \
    }                                                   \
    switch(seq.tag){                                    \
    case _array:                                        \
      return array_##name(seq,obj2,obj3);               \
    case _list:                                         \
    case _cons:                                         \
      return cons_##name(seq,obj2,obj3);                \
    }                                                   \
    }
static sexp _array_qsort(sexp arr,sexp comp_fun){
  return array_qsort(arr,comp_fun,NIL);
}
sexp sequence_qsort(sexp seq,sexp sort_fn);
sexp sequence_reverse(sexp seq);
sexp sequence_nreverse(sexp seq);
sexp lisp_length(sexp obj);
sexp sequence_length(sexp obj);
#endif
