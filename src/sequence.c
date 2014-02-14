/* Generic sequence functions, act on conses, vectors(1-D arrays) and strings

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
#include "common.h"
#include "sequence.h"
sexp lisp_length(sexp obj){
  if (CONSP(obj)){
    return cons_length(obj);
  } else if (STRINGP(obj)) {
    return int64_sexp(obj.val.str->len);
  } else if (ARRAYP(obj)){
    return int64_sexp(obj.val.array->len);
  } else
    raise_simple_error(Etype,"Invalid typed passed to length expected a sequence");
  }
}
make_sequence_function2(qsort);
make_sequence_function(reverse);
make_sequence_function(nreverse);
sexp sequence_length(sexp obj){
  return lisp_length(obj);
}
sexp lisp_iota(sexp start,sexp stop,sexp step,sexp arrayorlist,sexp rnd){
  if(NILP(arrayorlist)){
    return list_iota(start, stop, step);
  } else {
    return array_iota(start,stop,step,rnd);
  }
}
sexp lisp_sequencep(sexp seq){
  if(CONSP(seq) || ARRAYP(seq)){
    return LISP_TRUE;
  } else {
    return LISP_FALSE;
  }
}
sexp lisp_sort(sexp seq,sexp fun){
  if(!SEQUENCEP(seq)){
    return format_type_error("sort","sequence",seq.tag);
  }
  switch(seq.tag){
    case _list:
    case _cons:
      return cons_merge_sort(seq,fun);
    case _array:
      return array_qsort(seq,fun,NIL);
  }
}
sexp sequence_merge_sort(sexp seq,sexp sort_fn){
  if(!SEQUENCEP(seq)){
    return format_type_error("merge-sort","sequence",seq.tag);
  }
  if(CONSP(seq)){
    return cons_merge_sort(seq,sort_fn);
  }
}
#if 0
/*CORD API functions to iterate across cords*/
/* Function to iteratively apply to individual characters in cord.      */
typedef int (* CORD_iter_fn)(char c, void * client_data);

/* Function to apply to substrings of a cord.  Each substring is a      */
/* a C character string, not a general cord.                            */
typedef int (* CORD_batched_iter_fn)(const char * s, void * client_data);
#define CORD_NO_FN ((CORD_batched_iter_fn)0)

/* Apply f1 to each character in the cord, in ascending order,          */
/* starting at position i. If                                           */
/* f2 is not CORD_NO_FN, then multiple calls to f1 may be replaced by   */
/* a single call to f2.  The parameter f2 is provided only to allow     */
/* some optimization by the client.  This terminates when the right     */
/* end of this string is reached, or when f1 or f2 return != 0.  In the */
/* latter case CORD_iter returns != 0.  Otherwise it returns 0.         */
/* The specified value of i must be < CORD_len(x).                      */
CORD_API int CORD_iter5(CORD x, size_t i, CORD_iter_fn f1,
                        CORD_batched_iter_fn f2, void * client_data);

/* A simpler version that starts at 0, and without f2:  */
CORD_API int CORD_iter(CORD x, CORD_iter_fn f1, void * client_data);
#define CORD_iter(x, f1, cd) CORD_iter5(x, 0, f1, CORD_NO_FN, cd)

/* Similar to CORD_iter5, but end-to-beginning. No provisions for       */
/* CORD_batched_iter_fn.                                                */
CORD_API int CORD_riter4(CORD x, size_t i, CORD_iter_fn f1, void * client_data);

/* A simpler version that starts at the end:    */
CORD_API int CORD_riter(CORD x, CORD_iter_fn f1, void * client_data);
#endif
