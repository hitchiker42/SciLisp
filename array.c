/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "array.h"
#include "prim.h"
sexp aref(sexp obj,sexp ind){
  if(!ARRAYP(obj)){
    format_error_str("aref type error, expected array, got %s",typeName(obj));
    return error_sexp(error_str);
  } else if(ind.val.int64 > obj.len || ind.val.int64<0){
      return error_sexp("aref bounds error");
  } else {
    return XAREF(obj,ind.val.int64);
  }
}
/*#define iota_loop(sign,cmp,type,tag)         \
      for(i=0;i cmp imax;i sign##sign){        \
        newarray[sign i].tag=(type)i;          \
        }*/
sexp array_iota(sexp start,sexp stop,sexp step,sexp should_round){
  int i=0,k;
  double j,dstep;
  if(!NUMBERP(start)){goto TYPE_ERROR;}
  if(NILP(stop)){
    stop=start;
    start=long_sexp(0);
  } else if(!NUMBERP(stop)){goto TYPE_ERROR;}
  double range=getDoubleValUnsafe(lisp_sub(stop,start));
  if(NILP(step)){
    step=(range < 0)?long_sexp(-1):long_sexp(1);
  } else if(!NUMBERP(step)){goto TYPE_ERROR;}
  dstep=getDoubleValUnsafe(step);
  int imax=ceil(fabs(range/dstep));
  data* newarray=xmalloc(sizeof(data)*imax+1);
  j=getDoubleValUnsafe(start);
  int rnd=!NILP(should_round);
  for(i=0;i<imax;i++){
    if(rnd){
      newarray[i].int64=round(j);
    } else {
      newarray[i].real64=j;
    }
    j+=dstep;
  }
  if(rnd){
    //array_sexp(newarray,_long,imax)
    return (sexp){.tag=_array,.meta=_long_array,.val={.array=newarray},.len=imax};
  } else {
        //array_sexp(newarray,_double,imax)
    return (sexp){.tag=_array,.meta=_double_array,.val={.array=newarray},.len=imax};
  }
  TYPE_ERROR:
    return error_sexp("iota type error, arguments must be numbers");
}
sexp array_from_list(sexp ls){
  if(NILP(ls)){
    return NIL;
  }
  _tag array_type=XCAR(ls).tag;
  int len=cons_length(ls).val.int64;//since list is a rest arge this should be easy
  //assuming I calculate the length of rest args, if not I need to
  data *new_array=xmalloc(sizeof(data)*len);
  int i=0;
  while(CONSP(ls)){
    if(XCAR(ls).tag != array_type){
      return error_sexp("type error, all array elements must be of the same type");
    } else {
      new_array[i]=XCAR(ls).val;
      ls=XCDR(ls);
    }
  }
  return array_sexp(new_array,array_type,len);
}
static data temp;
static int array_qsort_partition
(data *arr,int left,int right,int pivot_ind,int(*f)(data,data)){
  data pivot=arr[pivot_ind];
  //reuse space for pivot_ind for current index pointer
  arr[pivot_ind]=arr[right];
  int i;
  pivot_ind=left;
  for(i=left;i<right-1;i++){
    if(f(arr[i],pivot)){
      temp=arr[i];
      arr[i]=arr[pivot_ind];
      arr[pivot_ind++]=temp;
    }
  }
  arr[right]=arr[pivot_ind];
  arr[pivot_ind]=pivot;
  return pivot_ind;
}
static void array_qsort_inplace(data *arr,int left,int right,int(*f)(data,data)){
  if(left < right){
    int pivot= right-left;//I have no clue what the pivot should be 
    pivot=array_qsort_partition(arr,left,right,pivot,f);
    array_qsort_inplace(arr,left,pivot-1,f);
    array_qsort_inplace(arr,pivot+1,right,f);
  }
  return;
}
sexp array_qsort(sexp arr,sexp comp_fun,sexp in_place){
  if(!(ARRAYP(arr)) || !(FUNCTIONP(comp_fun))){
    return format_type_error2
      ("array-qsort","array",arr.tag,"function",comp_fun.tag);
  }
  data *sorted_array;
  int(*f)(data,data);
  if(isTrue(in_place)){
    sorted_array=arr.val.array;
  } else {
    sorted_array=xmalloc(sizeof(data)*arr.len);
    memcpy(sorted_array,arr.val.array,arr.len);
  }
  array_qsort_inplace(sorted_array,0,arr.len,f);
  return array_sexp(sorted_array,arr.tag,arr.len);
}
/*sexp array_iota(sexp start,sexp stop,sexp step){
  long i,imax;
  double dstep;
  data* newarray;
  if(!NUMBERP(start)){
    return error_sexp("iota type error, arguments must be numbers");
  }
  if(NILP(stop)){//only got one argument,so use that as stop
    if(FLOATP(start)){
      double fstop=AS_DOUBLE(start);
      long sign=(signbit(fstop));
      imax=lround(fstop);
      newarray=xmalloc(sizeof(data)*(abs(imax)));
      if(sign){
        iota_loop(-,>,double,real64);
      } else {        
        iota_loop(+,<,double,real64);
      }
      return (sexp){.tag=_array,.val={.array=newarray},
          .len=abs(imax),.meta=_double_array};      
    } else {
      long lstop=AS_LONG(start);
      long sign=AS_LONG(start) & 0x8;
      imax=lstop;
      newarray=xmalloc(sizeof(data)*abs(imax));
      if(sign){
        iota_loop(-,>,long,int64);
      } else {
        iota_loop(+,<,long,int64);
      }
      return (sexp){.tag=_array,.val={.array=newarray},
          .len=abs(imax),.meta=_long_array};
    }
  } else if(!NUMBERP(stop)){
    return error_sexp("iota type error, arguments must be numbers");
  } else {
    imax=ceil(fabs(getDoubleVal(lisp_sub(stop,start))/dstep));
    long sign=signbit(getDoubleVal(lisp_sub(stop,start))/dstep);
    if(NILP(step)){
      dstep=(sign?-1.0:1.0);
    } else {
      if(!NUMBERP(step)){
        return error_sexp("iota type error, arguments must be numbers");
      }
      dstep=getDoubleVal(step);
    }
    newarray=xmalloc(sizeof(data)*imax);
    double j=getDoubleVal(start);
    for(i=0;i<imax;i++){
      newarray[i].real64=j;
      j+=dstep;
    }
    return (sexp){.tag=_array,.val={.array=newarray},
        .len=imax,.meta=_double_array};
  }
  }    */
