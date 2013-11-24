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
static data temp;
static int sarray_qsort_partition
(sexp *arr,int left,int right,int pivot_ind,sexp(*f)(sexp,sexp)){
  sexp pivot=arr[pivot_ind];
  //reuse space for pivot_ind for current index pointer
  arr[pivot_ind]=arr[right];
  int i;
  pivot_ind=left;
  for(i=left;i<right-1;i++){
    if(isTrue(f(arr[i],pivot))){
      temp=arr[i];
      arr[i]=arr[pivot_ind];
      arr[pivot_ind++]=temp;
    }
  }
  arr[right]=arr[pivot_ind];
  arr[pivot_ind]=pivot;
  return pivot_ind;
}
static void sarray_qsort_inplace(sexp *arr,int left,int right,
                                sexp(*f)(sexp,sexp)){
  if(left < right){
    int pivot= right-left;//I have no clue what the pivot should be
    pivot=array_qsort_partition(arr,left,right,pivot,f);
    array_qsort_inplace(arr,left,pivot-1,f);
    array_qsort_inplace(arr,pivot+1,right,f);
  }
  return;
}
sexp sarray_qsort(sexp arr,sexp comp_fun,sexp in_place){
  if(!(SARRAYP(arr)) || !(FUNCTIONP(comp_fun))){
    return format_type_error2
      ("sarray-qsort","sarray",arr.tag,"function",comp_fun.tag);
  }
  sexp *sorted_array;
  sexp(*f)(sexp,sexp);
  if(isTrue(in_place)){
    sorted_sarray=arr.val.sarray;
  } else {
    sorted_array=xmalloc(sizeof(sexp)*arr.len);
    memcpy(sorted_array,arr.val.sarray,arr.len);
  }
  sarray_qsort_inplace(sorted_array,0,arr.len,f);
  return sarray_sexp(sorted_array,arr.tag,arr.len);
}
