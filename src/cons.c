/* Functions on lists/cons cells

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
#include "cons.h"
//#include "prim.h"
//I tried to simplify this a bit, but it broke things, so it stays like it is
/*
sexp Fcons(sexp car_cell,sexp cdr_cell){
  sexp retval;
  cons *new_cell=xmalloc(sizeof(cons));
  *new_cell=(cons){.car=car_cell,.cdr=cdr_cell};
  return cons_sexp(new_cell);
}
sexp Fcons_2(sexp car_cell,sexp cadr_cell,sexp cddr_cell){
  sexp retval;
  cons *new_cell=xmalloc(sizeof(cons)*2);
  new_cell->car=car_cell;
  new_cell->cdr=cons_sexp(new_cell+1);
  XCAR(new_cell->cdr)=cadr_cell;
  XCDR(new_cell->cdr)=cddr_cell;
  return cons_sexp(new_cell);
}
*/
sexp mklist(sexp head,...){
  va_list ap;
  sexp retval,cur_loc;
  retval.tag=sexp_cons;
  cons *next=retval.val.cons=xmalloc(sizeof(cons));
  cons *trail=next;
  next->car=head;
  va_start(ap,head);
  while(!NILP((cur_loc=va_arg(ap,sexp)))){
    next->car=cur_loc;
    next->cdr=cons_sexp(xmalloc(sizeof(cons)));
    trail=next;
    next=next->cdr.val.cons;
  }
  trail->cdr=NIL;
  retval.is_ptr=1;
  return retval;
}
sexp mkImproper(sexp head,...){
  PRINT_MSG("Making an improper list");
  cons* next=xmalloc(sizeof(cons));
  cons* trail=next;
  next->car=head;
  va_list ap;
  sexp retval,cur_loc;
  int i;
  va_start(ap,head);
  while(!NILP((cur_loc=va_arg(ap,sexp)))){
    next->car=cur_loc;
    next->cdr.val.cons=xmalloc(sizeof(cons));
    trail=next;
    next=next->cdr.val.cons;
    i++;
  }
  cur_loc=va_arg(ap,sexp);
  next->cdr=cur_loc;
  retval.len=i;
  retval.is_ptr=1;
  return retval;
}
static inline sexp _cons_reverse(sexp ls){
  sexp cons_ptr=cons_sexp(xmalloc(sizeof(cons)));
  //  sexp trail=cons_ptr;
  XCDR(cons_ptr)=NIL;
  while(1){
    XCAR(cons_ptr)=XCAR(ls);
    ls=XCDR(ls);
    if(!CONSP(ls)){
      //don't know what to do for improper lists
      break;
    }
    cons_ptr=cons_sexp(xmalloc(sizeof(cons)));
    //    XCDR(cons_ptr)=trail;
    //    trail=cons_ptr;
  }
  return cons_ptr;
}
sexp c_cons_reverse(sexp ls){
  return _cons_reverse(ls);
}
sexp cons_reverse(sexp ls){
  if(!CONSP(ls)){
    return format_type_error("reverse","cons",ls.tag);
  }
  return _cons_reverse(ls);
}

sexp c_cons_split(sexp ls,sexp num){
  int64_t i=num.val.int64;
  if(i==0){
    return(Fcons(NIL,ls));
  }
  if(i<0 || i > cons_length(ls).val.int64){
    return error_sexp("error in split, index out of bounds");
  }
  sexp left,left_retval;
  left=left_retval=cons_sexp(xmalloc(sizeof(cons)));
  while(i>0 && CONSP(ls)){
    XCAR(left)=XCAR(ls);
    XCDR(left)=cons_sexp(xmalloc(sizeof(cons)));
    left=XCDR(left);
    ls=XCDR(ls);
    i--;
  }
  left=NIL;
  return(Fcons(left_retval,ls));
}
sexp cons_split(sexp ls,sexp num){
  if(!CONSP(ls)){
    return format_type_error("split","list",ls.tag);
  } if (NILP(num)){
    num=long_sexp(cons_length(ls).val.int64/2);//actually >>1,but gcc can do that
  }
  if(!INTP(num)){
    return format_type_error("split","integer",num.tag);
  }
  return c_cons_split(ls,num);
}
sexp nreverse(sexp ls){
  sexp cur_cell,last_cell,next_cell;
  cur_cell=ls;
  next_cell=XCDR(ls);
  last_cell=NIL;
  while(CONSP(next_cell)){
    XCDR(cur_cell)=last_cell;
    last_cell=cur_cell;
    cur_cell=next_cell;
    next_cell=XCDR(next_cell);
  }
  XCDR(cur_cell)=last_cell;
  return cur_cell;
}
sexp cons_nreverse(sexp ls){
  if(!CONSP(ls)){
    return format_type_error("reverse!","cons",ls.tag);
  }
  return nreverse(ls);
}
sexp nappend(sexp conses){
  if(!CONSP(XCAR(conses))){
    return format_type_error("append!","list of sequences",conses.tag);
  }
  cons* retval=XCAR(conses).val.cons;
  cons* cur_cell=retval;
  while(CONSP(XCDR(conses))){
    XCDR(last(XCAR(conses)))=XCAR(XCDR(conses));
    conses=XCDR(conses);
  }
  return cons_sexp(retval);
}
sexp cons_reduce(sexp ls,sexp reduce_fn,sexp start){
  if(!CONSP(ls) || !SUBRP(reduce_fn)){
    return format_type_error2("reduce","list",ls.tag,"function",reduce_fn.tag);
  }
  if(NILP(start)){
    start=long_sexp(0);
  }
  sexp result=start;
  //NEEDS TO BE FIXED
  sexp(*f)(sexp,sexp);
  f=reduce_fn.val.fun->comp.f2;
  while(CONSP(ls)){
    result=f(XCAR(ls),result);
    ls=XCDR(ls);
  }
  return result;
}
sexp mapcar(sexp ls,sexp map_fn){
  if(!CONSP(ls) || !FUNCTIONP(map_fn)){
    return format_type_error2("mapcar","list",ls.tag,"function",map_fn.tag);
  }
  //NEEDS TO BE FIXED
  sexp result;
  int have_closure=0;
  void **closure;
  cons* cur_cell=result.val.cons=xmalloc(sizeof(cons));
  result.tag=sexp_cons;
  sexp(*f)(sexp);
  if(FUNP(map_fn)){
    f=map_fn.val.fun->comp.f1;
  } else if(LAMBDAP(map_fn)){
    closure=make_closure(map_fn,env_sexp(cur_env_ptr),1);
    if(!closure){
      return error_sexp("error constructing ffi_closure");
    }
    f=(sexp(*)(sexp))(closure[0]);
  }
  while(!NILP(XCDR(ls))){
    cur_cell->car=f(XCAR(ls));
    cur_cell->cdr=cons_sexp(xmalloc(sizeof(cons)));
    cur_cell=cur_cell->cdr.val.cons;
    ls=XCDR(ls);
  }
  cur_cell->car=f(XCAR(ls));
  cur_cell->cdr=NIL;
  if(have_closure){
    ffi_closure_free(closure[1]);
  }
  return result;
}
static __attribute__((pure,leaf,nothrow)) sexp len_acc(sexp ls,long n){
  if(!CONSP(ls)){
    //PRINT_FMT("length = %d",n);
    return long_sexp(n);
  } else {
    return len_acc(XCDR(ls),++n);
  }
}
sexp __attribute__((pure,leaf,nothrow)) cons_length(sexp ls) {
  return len_acc(ls,0);
}
sexp cons_take(sexp ls,sexp num){
  sexp retval = cons_split(ls,num);
  if(!CONSP(retval)){
    return retval;//presumably retval is an error
  } else {
    return XCAR(retval);
  }
}
sexp cons_drop(sexp ls,sexp num){
  if(!CONSP(ls) || !INTP(num)){
    raise_simple_error(Etype,format_type_error2("drop","list",ls.tag,
                                                "integer",num.tag));
  } else {
    int64_t i=num.val.int64;
    while(i>0 && CONSP(ls)){
      ls=XCDR(ls);
      i--;
    }
    if(i>0){
      raise_simple_error(Ebounds,"error in drop, index out of bounds");
    } else {
      return ls;
    }
  }
}
static inline sexp int_iota_helper(int64_t j,int64_t jstep,int64_t imax){
  int64_t i;
  cons *newlist=xmalloc(sizeof(cons)*imax+1);
  for(i=0;i<=imax;i++){
    newlist[i].car=int64_sexp(j);
    newlist[i].cdr=(sexp){.tag=_list,.val={.cons=&newlist[i+1]}};
    j+=jstep;
  }
  newlist[i-1].cdr=NIL;
  return (sexp){.tag=sexp_cons,.val={.cons=newlist}};
}
sexp list_int_iota(sexp start,sexp stop,sexp step){
  int64_t i,j,imax,jstep;
  if(NILP(stop)){
    imax=start.val.int64;
    jstep=1;
    j=0;
  } else {
    j=start.val.int64;
    int direction=(stop.val.int64-start.val.int64)<0;
    if(NILP(step)){
      jstep=(direction?-1:1);
      imax=abs(stop.val.int64-start.val.int64);
    } else {
      jstep=step.val.int64;
      imax=abs((stop.val.int64-start.val.int64)/jstep);
      if(j>>63 != direction){//check the sign bit
        return error_sexp
          ("error in iota, sign of step not equal to sign of stop-start");
      }
    }
  }
  return int_iota_helper(j,jstep,imax);
}
//lisp declaration would be
//(defun iota (start &optional stop step))
sexp list_iota(sexp start,sexp stop,sexp step){
  int i=0;
  int istep;
  double dstep;
  if(!NUMBERP(start)){
    return format_type_error("iota","number",start.tag);
  }
  if(NILP(stop)){
    int64_t jmax=lrint(get_double_val(start));
    int64_t j=0;
    int64_t jstep=(jmax>>63?-1:1);
    return int_iota_helper(j,jstep,abs(jmax));
  } else if(NILP(step)){
    step.tag=_int64;
    if(isTrue(lisp_numlt(stop,start))){
      dstep=-1;
    } else {
      dstep=1;
    }
  } else {
    dstep=getDoubleVal(step);
    if(dstep == 0) return NIL;
  }
  if(start.tag==stop.tag && stop.tag==step.tag && step.tag == sexp_int64){
  int64_t j=start.val.int64;
  int64_t jstep=(int64_t)dstep;
  int64_t imax=abs((stop.val.int64-j)/jstep);
    return int_iota_helper(j,jstep,imax);
  }
  int imax=ceil(fabs(get_double_val(lisp_sub_num(stop,start))/dstep));
  cons* newlist=xmalloc(sizeof(cons)*imax+1);
  double j=getDoubleVal(start);
  for(i=0;i<=imax;i++){
    newlist[i].car=double_sexp(j);
    newlist[i].cdr=cons_sexp(&newlist[i+1]);
    j+=dstep;
  }
  newlist[i-1].cdr=NIL;
  //  PRINT_MSG(print((sexp){.tag=_list,.val={.cons=newlist},.len=i}));
  return cons_sexp(newlist);
}
static sexp qsort_acc(sexp ls,sexp(*f)(sexp,sexp)){
  //find a way to use length somehow
  if(!CONSP(ls)){
    return ls;
  }
  else if(NILP(XCDR(ls))){
    return XCAR(ls);
  } else {
    sexp rhs=NIL,lhs=NIL,pivot,cur_cell;
    pivot=XCAR(ls);//cost of finding a better pivot outweights the benifits
    ls=XCDR(ls);
    while(CONSP(ls)){
      cur_cell=XCAR(ls);
      if(is_true(f(cur_cell,pivot))){
        lhs=Fcons(cur_cell,lhs);
      } else {
        rhs=Fcons(cur_cell,rhs);
      }
      ls=XCDR(ls);
    }
    lhs=qsort_acc(lhs,f);
    rhs=qsort_acc(rhs,f);
    sexp pivot_cell;
    pivot_cell=list_sexp(xmalloc(sizeof(cons)));
    XCAR(pivot_cell)=pivot;
    XCDR(pivot_cell)=NIL;
    if(CONSP(lhs)){
      XCDR(last(lhs))=pivot_cell;
    } else if (!NILP(lhs)) {
      lhs=Fcons(lhs,pivot_cell);
    } else {
      lhs=pivot_cell;
    }
    if(CONSP(rhs)){
      XCDR(pivot_cell)=rhs;
    } else if (!NILP(rhs)) {
      XCDR(pivot_cell)=Fcons(rhs,NIL);
    }
    lhs.tag=sexp_cons;
    return lhs;
  }
}
static sexp merge_sort_acc(sexp ls,sexp(*f)(sexp,sexp),int len);
static sexp merge_sort_merge(sexp left,sexp right,sexp(*f)(sexp,sexp));
//as is this will only work for proper lists
static inline sexp _merge_sort(sexp ls,sexp sort_fn){
  sexp(*f)(sexp,sexp);
  int have_closure=0;
  void **closure;
  make_function_pointer(f,sort_fn,2);
  /*  if(FUNP(sort_fn)){
    f=sort_fn.val.fun->comp.f2;
  } else if (LAMBDAP(sort_fn)){
    closure=make_closure(sort_fn,env_sexp(cur_env_ptr),2);
    if(!closure){return error_sexp("error constructing ffi_closure");}
    f=(sexp(*)(sexp,sexp))closure[0];
    }*/
  sexp retval=merge_sort_acc(ls,f,cons_length(ls).val.int64);
  if(have_closure){
    ffi_closure_free(closure[1]);
  }
  return retval;
}
sexp c_merge_sort(sexp ls,sexp sort_fn){
  return _merge_sort(ls,sort_fn);
}
sexp cons_merge_sort(sexp ls,sexp sort_fn){
  if(!CONSP(ls) || !FUNCTIONP(sort_fn)){
    raise_simple_error(Etype,format_type_error2("merge sort","list",
                                                ls.tag,"funciton",sort_fn.tag));
  }
  return _merge_sort(ls,sort_fn);
}
sexp cons_qsort(sexp ls,sexp sort_fn){
  if(!CONSP(ls) || !FUNCTIONP(sort_fn)){
    raise_simple_error(Etype,"qsort sort_fn type error");
  }
  sexp(*f)(sexp,sexp);
  int have_closure=0;
  void **closure;
  make_function_pointer(f,sort_fn,2);
  /*  if(FUNP(sort_fn)){
      f=sort_fn.val.fun->comp.f2;
  } else if(LAMBDAP(sort_fn)){
    closure=make_closure(sort_fn,env_sexp(cur_env_ptr),2);
    if(!closure){return error_sexp("error constructing ffi_closure");}
    f=(sexp(*)(sexp,sexp))closure[0];
    have_closure=1;
    }*/
  sexp retval=qsort_acc(ls,f);
  if(have_closure){
    ffi_closure_free(closure[1]);
  }
  return retval;
}
sexp merge_sort_acc(sexp ls,sexp(*f)(sexp,sexp),int len){
  if(!CONSP(ls)){
    if(NILP(ls)){
      return NIL;
    }
    raise_simple_error(Etype,"merge-sort type error, expected a cons cell");
    return ls;
  } else if(len <= 1){
    return ls;
  } else {
    int mid=len/2;
    sexp split_list=c_cons_split(ls,long_sexp(mid));
    if(ERRORP(split_list)){return split_list;}
    sexp left=XCAR(split_list);
    //I think this should work, but I'm not sure
    sexp right=XCDR(split_list);
    return merge_sort_merge(merge_sort_acc(left,f,mid),merge_sort_acc(right,f,len-mid),f);
  }
}
sexp merge_sort_merge(sexp left,sexp right,sexp(*f)(sexp,sexp)){
  if(NILP(left)){
    right.tag=cons_sexp;
    return right;
  } else if (NILP(right)){
    left.tag=cons_sexp;
    return left;
  } else {
    if(isTrue(f(XCAR(left),XCAR(right)))){
      return Fcons(XCAR(left),merge_sort_merge(XCDR(left),right,f));
    } else {
      return Fcons(XCAR(right),merge_sort_merge(left,XCDR(right),f));
    }
  }
}

sexp assoc(sexp obj,sexp ls,sexp eq_fn){
  sexp(*eq_fxn)(sexp,sexp)=eq_fn.val.fun->comp.f2;
  while(CONSP(ls)){
    if(is_true(eq_fxn(XCAR(ls),obj))){
      return XCAR(ls);
    }
    ls=XCDR(ls);
  }
  return NIL;
}
sexp lisp_assoc(sexp obj,sexp ls,sexp eq_fn){
  if(!CONSP(ls)){
    return format_type_error_key("assoc","ls","list",ls.tag);
  }
  if(NILP(eq_fn)){
    eq_fn=function_sexp(&lisp_eq_call);
  }
  return assoc(obj,ls,eq_fn);
}
sexp assq(sexp ls, sexp obj){
  return assoc(ls,obj,function_sexp(&lisp_eq_call));
}
sexp lisp_assq(sexp ls, sexp obj){
  return lisp_assoc(ls,obj,NIL);
}
sexp lisp_nth(sexp ls,sexp n){
  if(!CONSP(ls) || !(INTP(n))){
    return error_sexp("type error in nth");
  } else {
    return nth(ls,n.val.int64);
  }
}
//(defun last (list))
sexp lisp_last(sexp ls){
  if(!CONSP(ls)){
    return error_sexp("last type error, expected a cons cell");
  } else {
    return last(ls);
  }
}
sexp insertion_sort_cons(sexp list,sexp comp_fn){
  if(NILP(list)||NILP(XCDR(list))){
    return list;
  }
  //type checking and such
  sexp(*f)(sexp,sexp)=comp_fn.val.fun->comp.f2;
  sexp cur_pos=list;

  sexp start=list;
  sexp *trail;
  while(CONSP(list)){
    cur_pos=list;
    list=XCDR(list);
    trail=&start;
    while(!NILP((*trail)) && !(isTrue(f(*trail,cur_pos)))){
      trail=&(XCDR((*trail)));
    }
    XCDR(cur_pos)=*trail;
    *trail=cur_pos;
  }
  return start;
}
//this needs to be rethought
//(defun list (&rest args))
sexp lisp_list(uint64_t numargs,sexp *args){
  cons *new_list=xmalloc(numargs*sizeof(cons));
  sexp retval=cons_sexp(new_list);
  cons *ptr=retval;
  int i;
  for(i=0;i<numargs-1;i++){
    ptr->car=args[i];
    ptr->cdr=cons_sexp(ptr+1);
    ptr=ptr->cdr.val.cons;
  }
  ptr->car=args[i];
  ptr->cdr=NIL;
  return retval;
}
//recursively copy a list
//any non list/cons data structures are not
//copied (well, they're shallow copied)
static sexp unsafe_copy_cons(sexp ls){
  sexp retval;
  retval=ls;//shallow copy, to copy metadata
  sexp copy=cons_sexp(xmalloc(sizeof(cons)));
  while(CONSP(ls)){
    if(CONSP(XCAR(ls))){
      XCAR(copy)=unsafe_copy_cons(XCAR(ls));
    } else if(IS_POINTER(XCAR(ls))){
      XCAR(copy)=XCAR(ls);
      void *mem=xmalloc(sizeof(*XCAR(ls).val.opaque));
      XCAR(copy)=opaque_sexp(memcpy(mem,XCAR(ls).val.opaque,
                                    sizeof(*XCAR(ls).val.opaque)));
    } else {
      XCAR(ls)=XCAR(ls);
    }
    XCDR(copy)=cons_sexp(xmalloc(sizeof(cons)));
    copy=XCDR(copy);
    ls=XCDR(ls);
  }
  copy=ls;//not nil so the same thing will work for improper lists too
  return retval;
}
//copy ls but don't copy the actual values in ls
sexp c_shallow_copy_cons(sexp ls){
  sexp retval;
  sexp copy=retval=cons_sexp(xmalloc(sizeof(cons)));
  while(CONSP(ls)){
    XCAR(copy)=XCAR(ls);
    XCDR(copy)=cons_sexp(xmalloc(sizeof(cons)));
    copy=XCDR(copy)
    ls=XCDR(ls);
  }
  copy=ls;
  return retval;
}
sexp lisp_shallow_copy_cons(sexp ls){
  if(!CONSP(ls)){
    return format_type_error("shallow-copy-cons","cons cell",ls.tag);
  }
  c_shallow_copy_cons(ls);
}
sexp copy_cons(sexp ls){
  if(!CONSP(ls)){
    return format_type_error("copy-cons","cons cell",ls.tag);
  }
  unsafe_copy_cons(ls);
}
sexp cons_equal(sexp ls1,sexp ls2){
  if(!CONSP(ls1) || !CONSP(ls2)){
    if(is_true(lisp_equal(ls1,ls2))){
      return LISP_TRUE;
    } else {
      return LISP_FALSE;
    }
  } else if (isTrue(lisp_equal(XCAR(ls1),XCAR(ls2)))){
    return cons_equal(XCDR(ls1),XCDR(ls2));
  } else {
    return LISP_FALSE;
  }
}
//TODO: Rewrite this using mersenne twister
sexp rand_list(sexp len,sexp type){
  if(!INTP(len)){
    return format_type_error("rand-list","integer",len.tag);
  } else {
    int i;
    cons *ls=xmalloc(sizeof(cons)*len.val.int64);
    cons *ret_cons=ls;
    if(NILP(type) || KEYWORD_COMPARE(":int64",type)){
      for(i=0;i<len.val.int64-1;i++){
        ls->car=long_sexp(mrand48());
        ls->cdr=list_sexp(ls+1);
        ls=ls->cdr.val.cons;
      }
      ls->car=long_sexp(mrand48());
      ls->cdr=NIL;
      sexp retval=list_sexp(ret_cons);
      retval.tag=_list;
      retval.len=len.val.int64;
      return retval;
    }
    if(KEYWORD_COMPARE(":real64",type)){
      for(i=0;i<len.val.int64-1;i++){
        ls->car=double_sexp(drand48());
        ls->cdr=list_sexp(ls+1);
        ls=ls->cdr.val.cons;
      }
      ls->car=double_sexp(drand48());
      ls->cdr=NIL;
      sexp retval=list_sexp(ret_cons);
      retval.len=len.val.int64;
      return retval;
    }
    return error_sexp("invalid keyword passed to rand-list");
  }
}
//initial_contents is an &rest arg
sexp make_queue(sexp initial_contents){
  sexp queue=cons_sexp(xmalloc(sizeof(cons)));
  if(NILP(initial_contents)){
    XCAR(queue)=XCDR(queue)=NIL;
    return queue;
  } else {
    XCAR(queue)=initial_contents;
    XCDR(queue)=last(initial_contents);
    return queue;
  }
}
void enqueue(sexp val,sexp queue){
  cons *node=xmalloc(sizeof(cons));
  node->car=val;
  node->cdr=NIL;
  sexp new_node=cons_sexp(node);
  if(!NILP(XCDR(queue))){
    XCDDR(queue)=new_node;
  } else {
    XCAR(queue)=new_node;
  }
  XCDR(queue)=new_node;
}
sexp lisp_enqueue(sexp val,sexp queue){
  if(!CONSP(queue)){
    return format_type_error("enqueue","queue (aka list)",queue.tag);
  } else {
    enqueue(val,queue);
    return queue;
  }
}
sexp dequeue(sexp queue){
  sexp retval=XCAAR(queue);
  XCAR(queue)=XCDAR(queue);
  if(NILP(XCAR(queue))){
    XCDR(queue)=NIL;
  }
  return retval;
}
sexp lisp_dequeue(sexp queue,sexp noerror){
  if(!CONSP(queue)){
    return format_type_error("dequeue","queue(aka list)",queue.tag);
  }
  if(C_QUEUE_EMPTY(queue)){
    if(is_true(noerror)){
      return NIL;
    } else {
      return error_sexp("can't dequeue a value from an empty queue");
    }
  } else {
    return dequeue(queue);
  }
}
sexp queue_empty(sexp queue){
  return (NILP(XCAR(queue))? LISP_TRUE : LISP_FALSE);
}
sexp queue_peek(sexp queue){
  if(!CONSP(queue)){
    return format_type_error("queue-peek","queue (aka list)",queue.tag);
  } else {
    return XCAAR(queue);
  }
}
//probably faster than iterative version, but uses unbounded stack space
sexp flatten_acc(sexp x,sexp y,int start){
  if(NILP(x)){
    if(start){
      return nreverse(y);
    } else {
      return y;
    }
  } else if (CONSP(XCAR(x))) {
    return flatten_acc(XCDR(x),flatten_acc(XCAR(x),y),0);
  } else {
    flatten_acc(XCDR(x),Fcons(XCAR(x),y));
  }
}
sexp recursive_flatten(sexp x){
  //typecheck for cons
  sexp y=cons_sexp(xmalloc(sizeof(cons)));
  return flatten_acc(x,y,1);
}

//currently destructive, eaisly made non destructive
//by copying x before the first iteration
sexp iterative_flatten(sexp x){
  //naieve and unoptimized but it should work
  //loop over input list flattening it by one level each iteration
  //test after each iteration if the list is flattened, if not repeat
  sexp y=cons_sexp(xmalloc(sizeof(cons)));
  while(1){
  LOOP:
    if(NILP(x)){
      sexp ptr=y;
      while(CONSP(ptr)){
        if(CONSP(XCAR(ptr))){
          x=y;
          goto LOOP;
        }
        ptr=XCDR(ptr);
      }
      return nreverse(y);
    } else if(CONSP(XCAR(x))){
      XCDR(last(XCAR(x)))=XCAR(y);
      y=XCAR(x);
      x=XCDR(x);
    } else {
      y=Fcons(XCAR(x),y);
      x=XCDR(x);
    }
  }
}
sexp c_cons_search(sexp list,sexp elt,sexp (*test_fn)(sexp)){
  while(CONSP(list)){
    if(is_true(test_fn(POP(list),elt))){
      return elt;
    }
  }
  return NIL;
}

//return true if elt is contained in list(decided by test),
//otherwise returns false
sexp cons_exists(sexp list,sexp elt,sexp test){
  if(!CONSP(list)){
    raise_simple_error(Etype,format_type_error("cons-exists","cons",list.tag));
  }
  sexp *test_fn(sexp);
  if(NILP(test)){
    test_fn=lisp_eq;
  } else {
    raise_simple_error(Etype,"selectable test functions unimplemented");
  }
  while(CONSP(list)){
    if(is_true(test_fn(POP(list),elt))){
        return LISP_TRUE;
      }
  }
  return LISP_FALSE;
}

//return elt if elt is contained in list(decided by test),
//otherwise return nil
sexp cons_contains(sexp list,sexp elt,sexp test){
  if(!CONSP(list)){
    raise_simple_error(Etype,format_type_error("cons-contains","cons",list.tag));
  }
  sexp *test_fn(sexp);
  if(NILP(test)){
    test_fn=lisp_eq;
  } else {
    raise_simple_error(Etype,"selectable test functions unimplemented");
  }
  while(CONSP(list)){
    if(is_true(test_fn(POP(list),elt))){
      return elt;
    }
  }
  return NIL;
}
//if elt is found in list return the list starting with elt
//if elt is not found return nil
//for example (member '(1 2 3 4 5) 3) -> '(3 4 5)
//and (member '(1 2 3 4 5) 5) -> '(5)
sexp cons_member(sexp list,sexp elt,sexp test){
  if(!CONSP(list)){
    raise_simple_error(Etype,format_type_error("member","cons",list.tag));
  }
  sexp *test_fn(sexp);
  if(NILP(test)){
    test_fn=lisp_eq;
  } else {
    raise_simple_error(Etype,"selectable test functions unimplemented");
  }
  while(CONSP(list)){
    if(is_true(test_fn(XCAR(list),elt))){
      return list;
    }
    list=XCDR(list);
  }
  return NIL;
}
