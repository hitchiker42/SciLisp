/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef _ARRAY_H_
#define _ARRAY_H_
#include "common.h"
#include "cons.h"
//unsafe array access in C
#define TYPED_AREF(obj,ind)                                             \
  (sexp){.tag = (_tag)obj.meta,.val = (data)obj.val.typed_array[ind]}
#define XAREF(obj,ind)                          \
  obj.val.array[ind]
//find nerest power of two greater than len
static inline int __attribute__((const)) ARR_SIZE_ACC(int64_t len){
  if(len&(len-1)){
    return ARR_SIZE_ACC(len&(len-1));
  } else {
    return len<<1;
  }
}
static inline int __attribute__((pure))ARR_SIZE_ALLOCATED(sexp arr){
  return ARR_SIZE_ACC(arr.len);
}  
sexp typed_aref(sexp obj,sexp ind)__attribute__((pure,hot));
sexp aref(sexp obj,sexp ind)__attribute__((pure,hot));
sexp typed_array_to_list(sexp obj)__attribute__((pure));
sexp typed_array_iota(sexp start,sexp stop,sexp step,sexp rnd);
sexp array_iota(sexp start,sexp stop,sexp step,sexp rnd);
sexp typed_array_from_list(sexp ls);
sexp array_from_list(sexp ls);
sexp typed_array_to_list(sexp arr);
sexp array_to_list(sexp arr);
sexp array_qsort(sexp arr,sexp comp_fun,sexp in_place);
sexp array_reverse(sexp arr);
sexp array_nreverse(sexp arr);
sexp typed_array_qsort(sexp arr,sexp comp_fun,sexp in_place);
sexp array_map(sexp arr,sexp map_fn);
sexp array_nmap(sexp arr,sexp map_fn);
sexp array_reduce(sexp arr,sexp red_fn,sexp init);
sexp rand_array(sexp len,sexp type);
static int array_typecheck(int len,sexp arr,sexp_meta elem_type){
  if(TYPED_ARRAYP(arr)){
    if(len==0 || arr.len == len){//allow for dynamic arrays or something
      if(arr.meta == elem_type){
        return 1;
      }
    }
  }
  return 0;
}
static sexp typed_aref_unsafe(sexp obj,sexp ind){
  if(!TYPED_ARRAYP(obj)){
    return error_sexp("aref type error");
  } else { return TYPED_AREF(obj,ind.val.int64);}
}
//this is so much of a hack
#define static_array_typecheck_env(_0_length_0_,_0_type_0_)     \
  static_array_typecheck
#define static_array_typecheck(_arr_)                   \
  array_typecheck(_0_length_0_,_arr_,_0_type_0_)
#endif
