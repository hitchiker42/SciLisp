/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef _ARRAY_H_
#define _ARRAY_H_
#include "common.h"
#include "cons.h"
//unsafe array access in C
#define XAREF(obj,ind)                                          \
  (sexp){.tag = (_tag)obj.meta,.val = (data)obj.val.array[ind]}
sexp aref(sexp obj,sexp ind)__attribute__((pure,hot));
static sexp array_to_list(sexp obj)__attribute__((pure));
/*static sexp aref(sexp obj,sexp ind){
  if(!ARRAYP(obj)){
    return error_sexp("aref type error");
  } else if(ind.val.int64 > obj.len || ind.val.int64<0){
      return error_sexp("aref bounds error");
  } else {
    return XAREF(obj,ind.val.int64);
  }
  }*/
static sexp aref_unsafe(sexp obj,sexp ind){
  if(!ARRAYP(obj)){
    return error_sexp("aref type error");
  } else { return XAREF(obj,ind.val.int64);}
}
static sexp array_to_list(sexp obj){
  if(!ARRAYP(obj)){
    return error_sexp("array->list type error");
  } else {
    int i,imax=obj.len;
    sexp retval;
    retval.tag=_list;
    cons* newlist = retval.val.cons =  xmalloc(imax*sizeof(cons));    
    for(i=0;i<imax;i++){
      newlist[i].car=XAREF(obj,i);
      newlist[i].cdr=(sexp){.tag=_cons,.val={.cons=&newlist[i+1]}};
    }
    newlist[i-1].cdr=NIL;
    return retval;
  }
}
sexp array_iota(sexp start,sexp stop,sexp step,sexp rnd);
sexp array_from_list(sexp ls);
#endif
