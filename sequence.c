#include "common.h"
#include "sequence.h"
sexp lisp_length(sexp obj){
  if(obj.len > 0){
    return (sexp){.tag=_long,.val={.int64 = obj.len}};
  } else if (CONSP(obj)){
    //    HERE();
    return cons_length(obj);
  } else {
    return error_sexp("object does not have a meaningful length field");
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
      return merge_sort(seq,fun);
    case _array:
      return array_qsort(seq,fun,NIL);
  }
}
