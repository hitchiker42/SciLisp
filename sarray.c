/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "array.h"
#include "prim.h"
sexp saref(sexp obj,sexp ind){
  if(!SARRAYP(obj) || !INTP(ind)){
    return format_type_error2("aref","array",obj.tag,"integer",ind.tag);
  } else if(ind.val.int64 > obj.len || ind.val.int64<0){
    return error_sexp("aref bounds error");
  } else {
    return obj.val.sarray[ind.val.int64];
  }
}
sexp sarray_iota(sexp start,sexp stop,sexp step,sexp should_round){
  int i=0,k;
  double j,dstep;
  if(!NUMBERP(start)){
    return format_type_error("array_iota","number",start.tag);
  }
  if(NILP(stop)){
    stop=start;
    start=long_sexp(0);
  } else if(!NUMBERP(stop)){
    format_type_error_opt("array_iota","number",stop.tag);
  }
  double range=getDoubleValUnsafe(lisp_sub(stop,start));
  if(NILP(step)){
    step=(range < 0)?long_sexp(-1):long_sexp(1);
  } else if(!NUMBERP(step)){
    format_type_error_opt("array_iota","number",step.tag);
  }
  dstep=getDoubleValUnsafe(step);
  int imax=ceil(fabs(range/dstep));
  sexp* newarray=xmalloc(sizeof(sexp)*imax+1);
  j=getDoubleValUnsafe(start);
  int rnd=!NILP(should_round);
  for(i=0;i<imax;i++){
    if(rnd){
      newarray[i].val.int64=round(j);
      newarray[i].tag=_int64;
    } else {
      newarray[i].val.real64=j;
      newarray[i].tag=_real_64;
    }
    j+=dstep;
  }
  if(rnd){
    return sarray_sexp(newarray,imax);
  } else {
    return sarray_sexp(newarray,imax);
  }
}
sexp sarray_from_list(sexp ls){
  if(NILP(ls)){
    return NIL;
  }
  int len=cons_length(ls).val.int64;//since list is a rest arge this should be easy
  //assuming I calculate the length of rest args, if not I need to
  sexp *new_array=xmalloc(sizeof(sexp)*len);
  int i=0;
  while(CONSP(ls)){
      new_array[i]=XCAR(ls);
      ls=XCDR(ls);
    }
  }
  return sarray_sexp(new_array,len);
}
