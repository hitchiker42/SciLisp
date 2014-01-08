/*****************************************************************
 * Copyright (C) 2013-2014 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include "prim.h"
//I tried to simplify this a bit, but it broke things, so it stays like it is
sexp Cons(sexp car_cell,sexp cdr_cell){
  sexp retval;
  cons *new_cell=xmalloc(sizeof(cons));
  *new_cell=(cons){.car=car_cell,.cdr=cdr_cell};
  return list_sexp(new_cell);
}
sexp raw_cons(sexp car_cell,sexp cdr_cell){
  sexp retval;
  cons *new_cell=xmalloc(sizeof(cons));
  *new_cell=(cons){.car=car_cell,.cdr=cdr_cell};
  return cons_sexp(new_cell);
}
sexp mklist(sexp head,...){
  va_list ap;
  sexp retval,cur_loc;
  retval.tag=_list;
  cons *next=retval.val.cons=xmalloc(sizeof(cons));
  cons *trail=next;
  int i=0;
  next->car=head;
  va_start(ap,head);
  while(!NILP((cur_loc=va_arg(ap,sexp)))){
    next->car=cur_loc;
    next->cdr=cons_sexp(xmalloc(sizeof(cons)));
    trail=next;
    next=next->cdr.val.cons;
    i++;
  }
  trail->cdr=NIL;
  retval.is_ptr=1;
  retval.len=i;
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
  cons *cons_ptr=xmalloc(sizeof(cons));
  cons *trail=cons_ptr;
  int i,len=ls.len;
  cons_ptr->cdr=NIL;
  while(CONSP(ls)){
    len && i++;
    cons_ptr->car=XCAR(ls);
    trail=cons_ptr;
    cons_ptr=xmalloc(sizeof(cons));
    cons_ptr->cdr=cons_sexp(trail);
    ls=XCDR(ls);
  }
  sexp retval=cons_sexp(trail);
  retval.tag=_list;
  retval.len=(len ? len : i);
  return retval;
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
    return(Cons(NIL,ls));
  }
  if(i<0 || i > cons_length(ls).val.int64){
    return error_sexp("error in split, index out of bounds");
  }
  cons *left,*trail;
  sexp left_retval;
  left=left_retval.val.cons=xmalloc(sizeof(cons));
  left_retval.tag=_list,left_retval.is_ptr=1,left_retval.len=i;
  while(i>0 && CONSP(ls)){
    left->car=XCAR(ls);
    left->cdr=cons_sexp(xmalloc(sizeof(cons)));
    trail=left;
    left=left->cdr.val.cons;
    ls=XCDR(ls);
    i--;
  }
  trail->cdr=NIL;
  return(Cons(left_retval,ls));
}
sexp cons_split(sexp ls,sexp num){
  if(!CONSP(ls)){
    return format_type_error("split","list",ls.tag);
  } if (NILP(num)){
    num=long_sexp(cons_length(ls).val.int64>>1);
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
  int len=XCAR(conses).len;
  while(CONSP(XCDR(conses))){
    XCDR(last(XCAR(conses)))=XCAR(XCDR(conses));
    conses=XCDR(conses);
    len+=conses.len;
  }
  return cons_sexp(retval);
}


sexp cons_reduce(sexp ls,sexp reduce_fn,sexp start){
  if(!CONSP(ls) || !FUN2P(reduce_fn)){
    return format_type_error2("reduce","list",ls.tag,"function",reduce_fn.tag);
  }
  if(NILP(start)){
    start=long_sexp(0);
  }
  sexp result=start;
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
  sexp result;
  int have_closure=0;
  void **closure;
  cons* cur_cell=result.val.cons=xmalloc(sizeof(cons));
  result.tag=_cons;
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
static sexp len_acc(sexp ls,long n) __attribute__((pure));
static sexp len_acc(sexp ls,long n){
  if(!CONSP(ls)){
    PRINT_FMT("length = %d",n);
    return long_sexp(n);
  } else {
    return len_acc(XCDR(ls),++n);
  }
}
sexp cons_length(sexp ls) {
  if(ls.len > 0){
    return long_sexp(ls.len);
  } else {
    return len_acc(ls,0);
  }
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
    return format_type_error2("drop","list",ls.tag,"integer",num.tag);
  } else {
    int64_t i=num.val.int64;
    while(i>0 && CONSP(ls)){
      ls=XCDR(ls);
      i--;
    }
    if(i>0){
      return error_sexp("error in drop, index out of bounds");
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
  return (sexp){.tag=_list,.val={.cons=newlist},.len=i};
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
    int64_t jmax=lrint(getDoubleVal(start));
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
  if(start.tag==stop.tag && stop.tag==step.tag && step.tag == _int64){
  int64_t j=start.val.int64;
  int64_t jstep=(int64_t)dstep;
  int64_t imax=abs((stop.val.int64-j)/jstep);
    return int_iota_helper(j,jstep,imax);
  }
  int imax=ceil(fabs(getDoubleVal(lisp_sub_num(stop,start))/dstep));
  cons* newlist=xmalloc(sizeof(cons)*imax+1);
  double j=getDoubleVal(start);
  for(i=0;i<=imax;i++){
    newlist[i].car=double_sexp(j);
    newlist[i].cdr=(sexp){.tag=_list,.val={.cons=&newlist[i+1]}};
    j+=dstep;
  }
  newlist[i-1].cdr=NIL;
  //  PRINT_MSG(print((sexp){.tag=_list,.val={.cons=newlist},.len=i}));
  return list_len_sexp(newlist,i);
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
      if(isTrue(f(cur_cell,pivot))){
        lhs=Cons(cur_cell,lhs);
      } else {
        rhs=Cons(cur_cell,rhs);
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
      lhs=Cons(lhs,pivot_cell);
    } else {
      lhs=pivot_cell;
    }
    if(CONSP(rhs)){
      XCDR(pivot_cell)=rhs;
    } else if (!NILP(rhs)) {
      XCDR(pivot_cell)=Cons(rhs,NIL);
    }
    lhs.tag=_list;
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
    return format_type_error2("merge sort","list",ls.tag,"funciton",sort_fn.tag);
  }
  return _merge_sort(ls,sort_fn);
}
sexp cons_qsort(sexp ls,sexp sort_fn){
  if(!CONSP(ls) || !FUNCTIONP(sort_fn)){
    return error_sexp("qsort sort_fn type error");
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
    return error_sexp("merge-sort type error, expected a cons cell");
    return ls;
  } else if(len <= 1){
    return ls;
  } else {
    ls.len=len;
    int mid=len/2;
    sexp split_list=c_cons_split(ls,long_sexp(mid));
    if(ERRORP(split_list)){return split_list;}
    sexp left=XCAR(split_list);
    left.len=mid;
    //I think this should work, but I'm not sure
    sexp right=XCDR(split_list);
    right.len=len-mid;
    return merge_sort_merge(merge_sort_acc(left,f,mid),merge_sort_acc(right,f,len-mid),f);
  }
}
sexp merge_sort_merge(sexp left,sexp right,sexp(*f)(sexp,sexp)){
  if(NILP(left)){
    right.tag=_list;
    return right;
  } else if (NILP(right)){
    left.tag=_list;
    return left;
  } else {
    if(isTrue(f(XCAR(left),XCAR(right)))){
      return Cons(XCAR(left),merge_sort_merge(XCDR(left),right,f));
    } else {
      return Cons(XCAR(right),merge_sort_merge(left,XCDR(right),f));
    }
  }
}

sexp assoc(sexp obj,sexp ls,sexp eq_fn){
  sexp(*eq_fxn)(sexp,sexp)=eq_fn.val.fun->comp.f2;
  while(CONSP(ls)){
    if(isTrue(eq_fxn(XCAR(ls),obj))){
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
//(defun list (&rest args))
sexp lisp_list(sexp args){
  return args;
}
//recursively copy a list
//any non list/cons data structures are not
//copied (well, they're shallow copied)
static sexp unsafe_copy_cons(sexp ls){
  sexp retval;
  retval=ls;//shallow copy, to copy metadata
  cons *copy=retval.val.cons=xmalloc(sizeof(cons));
  cons *trail=copy;
  while(CONSP(ls)){
    if(CONSP(XCAR(ls))){
      copy->car=unsafe_copy_cons(XCAR(ls));
    } else if(IS_POINTER(XCAR(ls))){
        copy->car=XCAR(ls);
        void *mem=xmalloc(sizeof(*XCAR(ls).val.opaque));
        copy->car=opaque_sexp(memcpy(mem,XCAR(ls).val.opaque,
                                     sizeof(*XCAR(ls).val.opaque)));
    } else {
      copy->car=XCAR(ls);
    }
    copy->cdr=cons_sexp(xmalloc(sizeof(cons)));
    trail=copy;
    copy=copy->cdr.val.cons;
    ls=XCDR(ls);
  }
  trail->cdr=ls;//not nil so the same thing will work for improper lists too
  return retval;
}
//copy ls but don't copy the actual values in ls
sexp c_shallow_copy_cons(sexp ls){
  sexp retval;
  retval=ls;
  cons *copy=retval.val.cons=xmalloc(sizeof(cons));
  cons *trail=copy;
  while(CONSP(ls)){
    copy->car=XCAR(ls);
    copy->cdr.val.cons=xmalloc(sizeof(cons));
    trail=copy;
    copy=copy->cdr.val.cons;
    ls=XCDR(ls);
  }
  trail->cdr=ls;
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
    if(isTrue(lisp_equal(ls1,ls2))){
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
    if(isTrue(noerror)){
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
#if 0
sexp cons_insertion_sort
#endif
