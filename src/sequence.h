/* Header file for generic sequence functions

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
sexp lisp_iota(sexp start,sexp stop,sexp step,sexp arrayorlist,sexp rnd);
sexp sequence_merge_sort(sexp seq,sexp sort_fn);
#endif
