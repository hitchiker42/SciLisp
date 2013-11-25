/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include "prim.h"
sexp Cons(sexp car_cell,sexp cdr_cell){
  sexp retval;
  retval.tag=_list;//This needs looking at,the cons/list issue needs to be fixed
  retval.val.cons=GC_malloc(sizeof(cons));
  retval.val.cons->car=car_cell;
  retval.val.cons->cdr=cdr_cell;
  return retval;
}
sexp mklist(sexp head,...){
  PRINT_MSG("Making a list");
  va_list ap;
  sexp retval,cur_loc;
  retval.tag=_list;
  cons *next=retval.val.cons=xmalloc(sizeof(cons));
  cons *prev=next;
  next->car=head;
  va_start(ap,head);
  while((cur_loc=va_arg(ap,sexp)).tag != _nil){
    next->car=cur_loc;
    next->cdr.val.cons=xmalloc(sizeof(cons));
    prev=next;
    next=next->cdr.val.cons;
  }
  prev->cdr=NIL;
  return retval;
}
sexp mkImproper(sexp head,...){
  PRINT_MSG("Making an improper list");
  cons* list=xmalloc(sizeof(cons)),*next=xmalloc(sizeof(cons));
  list->car=head;
  va_list ap;
  sexp cur_loc,next_sexp;
  cons*next_cell;
  va_start(ap,head);
  while((cur_loc=va_arg(ap,sexp)).tag != _nil){
    next->car=cur_loc;
    next->cdr.val.cons=xmalloc(sizeof(cons));
    list->cdr=(sexp){.tag=_cons,.val={.cons = next}};
    next=next->cdr.val.cons;
  }
  cur_loc=va_arg(ap,sexp);
  next->cdr=cur_loc;
  return (sexp){.tag=_cons,.val={.cons=list}};
}

sexp nreverse(sexp ls){
  cons* cur_cell=ls.val.cons,*next_cell=cur_cell->cdr.val.cons;
  sexp last_val=NIL;
  while(!NILP(cur_cell->cdr)){
    cur_cell->cdr=last_val;//update ptr of current cell
    last_val=next_cell->cdr;//get ptr to current cell
    cur_cell=next_cell;//update current cell
    next_cell=cur_cell->cdr.val.cons;//update next cell, unchecked union access
  }
  cur_cell->cdr=last_val;
  return cons_sexp(cur_cell);
}
sexp nappend(sexp conses){
  cons* retval=XCAR(conses).val.cons;
  cons* cur_cell=retval;
  while(CONSP(XCDR(conses))){
    if(!CONSP(XCAR(conses))){
      return error_sexp("append! requires arguments to be cons cells or lists");
    }
    XCDR(last(XCAR(conses)))=XCAR(XCDR(conses));
    conses=(sexp)XCDR(conses);
  }
  return (sexp){.tag = _list,.val={.cons=retval}};
}


sexp reduce(sexp ls,sexp reduce_fn){
  if(!CONSP(ls) || !FUNP(reduce_fn)){
    return error_sexp("reduce type error");
  }
  HERE();
  sexp result=XCAR(ls);
  sexp(*f)(sexp,sexp);
  switch(reduce_fn.tag){
    case _fun:
      f=reduce_fn.val.fun->comp.f2;
      break;
    case _lam:
      break;
  }
  while(CONSP(cdr(ls))){
    ls=XCDR(ls);
    result=f(XCAR(ls),result);
  }
  return result;
}
sexp mapcar(sexp ls,sexp map_fn){
  if(!CONSP(ls) || !FUNCTIONP(map_fn)){
    return error_sexp("mapcar map_fn type error");
  }
  sexp result;
  cons* cur_cell=result.val.cons=xmalloc(sizeof(cons));
  result.tag=_cons;
  sexp(*f)(sexp);
  if(FUNP(map_fn)){
    f=map_fn.val.fun->comp.f1;
  }
  while(!NILP(XCDR(ls))){
    if(FUNP(map_fn)){
      cur_cell->car=f(car(ls));
    } else {
      cur_cell->cdr=eval(Cons(map_fn,Cons(car(ls),NIL)),
                         map_fn.val.fun->lam->env);
    }
    cur_cell->cdr.val.cons=xmalloc(sizeof(cons));
    cur_cell=cur_cell->cdr.val.cons;
    ls=XCDR(ls);
  }
  cur_cell->car=f(XCAR(ls));
  cur_cell->cdr=NIL;
  return result;
}
static sexp len_acc(sexp ls,long n) __attribute__((pure));
static sexp len_acc(sexp ls,long n){
  if(!CONSP(ls)){
    PRINT_FMT("length = %d",n);
    return (sexp){.tag=_long,.val={.int64=n}};
  } else {
    return len_acc(XCDR(ls),++n);
  }
}
sexp cons_length(sexp ls) {
  if(ls.len > 0){
    return (sexp){.tag=_long,.val={.int64=ls.len}};
  } else {
    return len_acc(ls,0);
  }
}
//lisp declaration would be
//(defun iota (start &optional stop step))
sexp list_iota(sexp start,sexp stop,sexp step){
  int i=0;
  double dstep;
  if(!NUMBERP(start)){
    return error_sexp("iota type error, arguments must be numbers");
  }
  if(NILP(stop)){
    int imax=lrint(getDoubleVal(start));
    cons* newlist=xmalloc(sizeof(cons)*abs(imax)+1);
    while(abs(i)<abs(imax)){
      newlist[abs(i)].car=(sexp){.tag=_long,.val={.int64=i}};
      newlist[abs(i)].cdr=(sexp){.tag=_list,.val={.cons=&newlist[abs(i)+1]}};
      if(imax<0){i--;}
      else {i++;}
    }
    newlist[abs(i)-1].cdr=NIL;
    HERE();
    return (sexp){.tag=_list,.val={.cons=newlist},.len=abs(i)};
  } else if(NILP(step)){
    if(isTrue(lisp_lt(stop,start))){
      dstep=-1;
    } else {
      dstep/1;
    }
  } else {
    dstep=getDoubleVal(step);
    if(dstep == 0) return NIL;
  }
  int imax=ceil(fabs(getDoubleVal(lisp_sub(stop,start))/dstep));
  cons* newlist=xmalloc(sizeof(cons)*imax+1);
  double j=getDoubleVal(start);
  for(i=0;i<imax;i++){
    newlist[i].car=(sexp){.tag=_double,.val={.real64=j}};
    newlist[i].cdr=(sexp){.tag=_list,.val={.cons=&newlist[i+1]}};
    j+=dstep;
  }
  newlist[i-1].cdr=NIL;
  //  PRINT_MSG(print((sexp){.tag=_list,.val={.cons=newlist},.len=i}));
  return (sexp){.tag=_list,.val={.cons=newlist},.len=i};
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
    pivot_cell.val.cons=xmalloc(sizeof(cons));
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
    return lhs;
  }
}
static sexp merge_sort_acc(sexp ls,sexp(*f)(sexp,sexp));
static sexp merge_sort_merge(sexp left,sexp right,sexp(*f)(sexp,sexp));
sexp merge_sort(sexp ls,sexp sort_fn){
  if(!CONSP(ls) || !FUNP(sort_fn)){
    return error_sexp("merge sort sort_fn type error");
  }
  sexp(*f)(sexp,sexp);
  env lambda_env;
  f=sort_fn.val.fun->comp.f2;
  return merge_sort_acc(ls,f);
}
sexp qsort_cons(sexp ls,sexp sort_fn){
  if(!CONSP(ls) || !FUNP(sort_fn)){
    return error_sexp("qsort sort_fn type error");
  }
  sexp(*f)(sexp,sexp);
  env lambda_env;
  f=sort_fn.val.fun->comp.f2;
  return qsort_acc(ls,f);
}
sexp merge_sort_acc(sexp ls,sexp(*f)(sexp,sexp)){
  if(!CONSP(ls)){
    return error_sexp("merge-sort type error, expected a cons cell");
    return ls;
  }
  else if(NILP(XCDR(ls))){
    return ls;
  } else {
    int mid=cons_length(ls).val.int64/2;
    sexp left=ls;
    //I think this should work, but I'm not sure
    sexp mid_cell=nth(ls,mid);
    sexp right=XCDR(mid_cell);
    XCDR(mid_cell)=NIL;
    return merge_sort_merge(merge_sort_acc(left,f),merge_sort_acc(right,f),f);
  }
}
sexp merge_sort_merge(sexp left,sexp right,sexp(*f)(sexp,sexp)){
  if(NILP(left)){
    return right;
  } else if (NILP(right)){
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
  if(!CONSP(ls)){
    return error_sexp("argument 2 of assoc must be an alist");
  }
  if(NILP(eq_fn)){
    eq_fn=function_sexp(&lisp_eq_call);
  }
  sexp(*eq_fxn)(sexp,sexp)=eq_fn.val.fun->comp.f2;
  while(CONSP(ls)){
    if(isTrue(eq_fxn(XCAR(ls),obj))){
      return XCAR(ls);
    }
    ls=XCDR(ls);
  }
  return NIL;
}
sexp assq(sexp ls, sexp obj){
  return assoc(ls,obj,NIL);
}
sexp lisp_nth(sexp ls,sexp n){
  if(!CONSP(ls) || !(INTP(n))){
    return error_sexp("type error in nth");
  } else {
    return nth(ls,n.val.int64);
  }
}
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
sexp lisp_list(sexp args){
  return args;
}
static sexp unsafe_copy_cons(sexp ls){
  sexp retval;
  retval=ls;//shallow copy, to copy metadata
  cons *copy=retval.val.cons=xmalloc(sizeof(cons));
  cons *trail=copy;
  while(CONSP(ls)){
    if(CONSP(XCAR(ls))){
      copy->car=unsafe_copy_cons(XCAR(ls));
    } else  if(IS_POINTER(XCAR(ls))){
        copy->car=XCAR(ls);
        void *mem=xmalloc(sizeof(*XCAR(ls).val.opaque));
        copy->car.val.opaque=memcpy(mem,XCAR(ls).val.opaque,sizeof(*XCAR(ls).val.opaque));
    } else {
      copy->car=XCAR(ls);
    }
    copy->cdr.val.cons=xmalloc(sizeof(cons));
    trail=copy;
    copy=copy->cdr.val.cons;
    ls=XCDR(ls);
  }
  trail->cdr=ls;
  return retval;
}

sexp copy_cons(sexp ls){
  if(!CONSP(ls)){
    return format_type_error("copy-cons","cons cell",ls.tag);
  }
  unsafe_copy_cons(ls);
}
