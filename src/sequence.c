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
