/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "array.h"
#include "prim.h"
#define aref_generic(name,test,type,macro)                              \
  sexp name(sexp obj,sexp ind){                                         \
    if(!test(obj) || !INTP(ind)){                                       \
      return format_type_error2(#name,#type,obj.tag,"integer",ind.tag); \
    } else if(ind.val.int64 >= obj.len || ind.val.int64<0){             \
      return error_sexp(#name" bounds error");                          \
    } else {                                                            \
      return macro(obj,ind.val.int64);                                  \
    }                                                                   \
  }
aref_generic(aref,ARRAYP,array,XAREF)
aref_generic(typed_aref,TYPED_ARRAYP,typed_array,TYPED_AREF)
#define array_iota_generic(name,type,subtype,ret_block)                 \
  sexp name(sexp start,sexp stop,sexp step,sexp should_round){          \
  int i=0,k;                                                            \
  double j,dstep;                                                       \
  if(!NUMBERP(start)){                                                  \
    return format_type_error(#name,"number",start.tag);                 \
  }                                                                     \
  if(NILP(stop)){                                                       \
    stop=start;                                                         \
    start=long_sexp(0);                                                 \
  } else if(!NUMBERP(stop)){                                            \
    format_type_error_opt(#name,"number",stop.tag);                     \
  }                                                                     \
  double range=getDoubleValUnsafe(lisp_sub(stop,start));                \
  if(NILP(step)){                                                       \
    step=(range < 0)?long_sexp(-1):long_sexp(1);                        \
  } else if(!NUMBERP(step)){                                            \
    format_type_error_opt(#name,"number",step.tag);                     \
  }                                                                     \
  dstep=getDoubleValUnsafe(step);                                       \
  int imax=ceil(fabs(range/dstep));                                     \
  subtype* newarray=xmalloc(sizeof(subtype)*imax+1);                    \
  j=getDoubleValUnsafe(start);                                          \
  int rnd=!NILP(should_round);                                          \
  for(i=0;i<imax;i++){                                                  \
    if(rnd){                                                            \
      type##_int_macro(i,j);                                            \
    } else {                                                            \
      type##_real_macro(i,j);                                           \
    }                                                                   \
    j+=dstep;                                                           \
  }                                                                     \
  ret_block                                                             \
}
#define typed_array_int_macro(i,j)              \
  newarray[i].int64=round(j)
#define typed_array_real_macro(i,j)             \
  newarray[i].real64=j
#define array_int_macro(i,j)                    \
  newarray[i]=long_sexp(round(j))
#define array_real_macro(i,j)                   \
  newarray[i]=double_sexp(j)
#define typed_array_iota_retblock                       \
  if(rnd){                                              \
    return typed_array_sexp(newarray,_double,imax);     \
  } else {                                              \
    return typed_array_sexp(newarray,_double,imax);     \
  }
#define array_iota_retblock                                    \
  return array_sexp(newarray,imax);
array_iota_generic(array_iota,array,sexp,array_iota_retblock);
array_iota_generic(typed_array_iota,typed_array,data,typed_array_iota_retblock);
sexp typed_array_from_list(sexp ls){
  if(NILP(ls)){
    return NIL;
  }
  _tag array_type=XCAR(ls).tag;
  int len=cons_length(ls).val.int64;
  data *new_array=xmalloc(sizeof(data)*len);
  int i=0;
  while(CONSP(ls)){
    if(XCAR(ls).tag != array_type){
      return error_sexp("type error, all typed_array elements must be of the same type");
    } else {
      new_array[i]=XCAR(ls).val;
      ls=XCDR(ls);
    }
  }
  return typed_array_sexp(new_array,array_type,len);
}
sexp typed_array_to_list(sexp obj){
  if(!TYPED_ARRAYP(obj)){
    return format_type_error("typed-array->list","typed-array",obj.tag);
  } else {
    int i,imax=obj.len;
    cons* newlist =xmalloc(imax*sizeof(cons));
    for(i=0;i<imax;i++){
      newlist[i].car=TYPED_AREF(obj,i);
      newlist[i].cdr=cons_sexp(newlist+i+1);
    }
    newlist[i-1].cdr=NIL;
    return cons_sexp(newlist);
  }
}
sexp array_from_list(sexp ls){
  if(NILP(ls)){
    return NIL;
  }
  int len=cons_length(ls).val.int64;
  sexp *new_array=xmalloc(sizeof(sexp)*len);
  int i=0;
  while(CONSP(ls)){
    new_array[i]=XCAR(ls);
    ls=XCDR(ls);
  }
  return array_sexp(new_array,len);
}
sexp array_to_list(sexp obj){
  if(!ARRAYP(obj)){
    return format_type_error("array->list","array",obj.tag);
  } else {
    sexp *old_array=obj.val.array;
    cons *new_list=xmalloc(sizeof(cons)*obj.len);
    int i;
    for(i=0;i<obj.len;i++){
      new_list[i].car=old_array[i];
      new_list[i].cdr=cons_sexp(new_list+i);
    }
    new_list[i-1].cdr=NIL;
    return cons_sexp(new_list);
  }
}
sexp array_map(sexp arr,sexp map_fn,sexp inplace){
  if(!ARRAYP(arr)||!(FUNP(map_fn))){
    return format_type_error2("array-map","array",arr.tag,
                              "function",map_fn.tag);
  }
  sexp *new_array;
  sexp(*f)(sexp)=map_fn.val.fun->comp.f1;
  if(isTrue(inplace)){
    new_array=arr.val.array;
  } else {
    new_array=xmalloc(sizeof(sexp)*arr.len);
    memcpy(new_array,arr.val.array,arr.len*sizeof(sexp));
  }
  int i;
  for(i=0;i<arr.len;i++){
    new_array[i]=f(new_array[i]);
  }
  return array_sexp(new_array,arr.len);
}
sexp array_reduce(sexp arr,sexp red_fn,sexp init){
  if(!ARRAYP(arr)||!(FUNP(red_fn))){
    return format_type_error2("array-reduce","array",arr.tag,
                              "function",red_fn.tag);
  }
  if(arr.len == 0){
    return NIL;
  }
  sexp(*f)(sexp,sexp)=red_fn.val.fun->comp.f2;
  sexp *arr_data=arr.val.array;
  sexp acc;
  if(!NILP(init)){
    acc=f(init,arr_data[0]);
  } else {
    acc = arr_data[0];
  }
  int i;
  for(i=1;i<arr.len;i++){
    acc=f(init,arr_data[i]);
  }
  return acc;
}
static data temp;
static int typed_array_qsort_partition
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
static void typed_array_qsort_inplace(data *arr,int left,int right,int(*f)(data,data)){
  if(left < right){
    int pivot= right-left;//I have no clue what the pivot should be
    pivot=typed_array_qsort_partition(arr,left,right,pivot,f);
    typed_array_qsort_inplace(arr,left,pivot-1,f);
    typed_array_qsort_inplace(arr,pivot+1,right,f);
  }
  return;
}
sexp typed_array_qsort(sexp arr,sexp comp_fun,sexp in_place){
  if(!(TYPED_ARRAYP(arr)) || !(FUNCTIONP(comp_fun))){
    return format_type_error2
      ("typed-array-qsort","typed-array",arr.tag,"function",comp_fun.tag);
  }
  data *sorted_array;
  int(*f)(data,data);
  if(isTrue(in_place)){
    sorted_array=arr.val.typed_array;
  } else {
    sorted_array=xmalloc(sizeof(data)*arr.len);
    memcpy(sorted_array,arr.val.typed_array,arr.len);
  }
  typed_array_qsort_inplace(sorted_array,0,arr.len,f);
  return typed_array_sexp(sorted_array,arr.tag,arr.len);
}
static sexp stemp;
#ifdef swap
#undef swap
#endif
#define swap(i,j,arr)                           \
  stemp=arr[i];                                  \
  arr[i]=arr[j];                                \
  arr[j]=stemp
static int array_qsort_partition
(sexp *arr,int left,int right,int pivot,sexp(*f)(sexp,sexp)){
  sexp pivot_val=arr[pivot];
  swap(pivot,right,arr);
  int i;
  int store=left;
  for(i=left;i<right;i++){
    if(isTrue(f(arr[i],pivot_val))){
      swap(i,store,arr);
      store++;
    }
  }
  swap(store,right,arr);
  return store;
}
static void array_qsort_inplace(sexp *arr,int left,int right,
                                sexp(*f)(sexp,sexp)){ 
  if(left < right){
    int pivot=left+(((right-left)/2) + ((right-left)%2));
    //I have no clue what the pivot should be
    int pivot_new=array_qsort_partition(arr,left,right,pivot,f);
    array_qsort_inplace(arr,left,pivot_new-1,f);
    array_qsort_inplace(arr,pivot_new+1,right,f);
  }
  return;
}
sexp array_qsort(sexp arr,sexp comp_fun,sexp in_place){
  if(!(ARRAYP(arr)) || !(FUNCTIONP(comp_fun))){
    return format_type_error2
      ("array-qsort","array",arr.tag,"function",comp_fun.tag);
  }
  sexp *sorted_array;
  sexp(*f)(sexp,sexp)=comp_fun.val.fun->comp.f2;
  if(!NILP(in_place)){
    sorted_array=arr.val.array;
  } else {
    sorted_array=xmalloc(sizeof(sexp)*arr.len);
    sorted_array=memcpy(sorted_array,arr.val.array,arr.len*sizeof(sexp));
  }
  PRINT_MSG(print(array_sexp(sorted_array,arr.len)));
  array_qsort_inplace(sorted_array,0,arr.len-1,f);  
  PRINT_MSG(print(array_sexp(sorted_array,arr.len)));
  return array_sexp(sorted_array,arr.len);
}
#undef swap
/* Heap
   -tree with compairson function f
   -the tree satisfies the headp property
   -that is, f(node,left-child) and f(node,right-child) are both true
   -each subtree also satisfies the heap property
   -implemented as an array.
   f(node)=[left-child,right-child];
   [root,f(root),f(root-left-child),f(root-right-child),...f(root-nth-child)]
   -i.e the array holds the elements of the tree as if they were being reads
   from left-right, top-bottom
*/
