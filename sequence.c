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
