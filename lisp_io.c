#include "common.h"
#include "cons.h"
#include "prim.h"
#include "regex.h"
sexp lisp_strcat(sexp cords){
  if(!CONSP(cords)){
    if(NILP(cords)){
      return string_sexp("");
    }
    return format_type_error_opt("concat-str",cords.tag);
  }
  CORD retval="";
  while(CONSP(cords)){
    if(!STRINGP(XCAR(cords))){
      return format_type_error("concat-str","string",XCAR(cords).tag);
    }
    retval=CORD_cat(retvar,XCAR(cords));
    cords=XCDR(cords);
  }
  return string_sexp(retval);
}
sexp lisp_substr(sexp lisp_cord,sexp start,sexp end){
  if(!STRINGP(lisp_cord)||!INTP(start)||!INTP(end)){
    return format_type_error("substr","string",lisp_cord.tag,
                             "integer",start.tag,"integer",end.tag);
  } else {
    return string_sexp
      (CORD_substr(lisp_cord.val.cord,start.val.int64,
                   end.val.int64-start.val.int64));
  }
}                                  
