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
    //error;
  }
  HERE();
  sexp result=XCAR(ls);
  sexp(*f)(sexp,sexp);
  switch(reduce_fn.tag){
    case _fun:
      f=reduce_fn.val.fun->fun.comp.f2;
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
    f=map_fn.val.fun->fun.comp.f1;
  }
  while(!NILP(XCDR(ls))){
    if(FUNP(map_fn)){
      cur_cell->car=f(car(ls));
    } else {
      cur_cell->cdr=eval(Cons(map_fn,Cons(car(ls),NIL)),
                         map_fn.val.fun->fun.lam->env);
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
    return (sexp){.tag=_long,.val={.int64=n}};
  } else {
    return len_acc(XCDR(ls),n++);
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
  int i;
  double dstep;
  if(!NUMBERP(start)){
    return error_sexp("iota type error, arguments must be numbers");
  }
  if(NILP(stop)){
    int imax=lrint(getDoubleVal(start));
    cons* newlist=xmalloc(sizeof(cons)*imax+1);
    for(i=0;i<imax;i++){
      newlist[i].car=(sexp){.tag=_long,.val={.int64=i}};
      newlist[i].cdr=(sexp){.tag=_list,.val={.cons=&newlist[i+1]}};
    }
    newlist[i-1].cdr=NIL;
    HERE();
    return (sexp){.tag=_list,.val={.cons=newlist},.len=i};
  } else if(NILP(step)){
    dstep=1;
  } else {
    dstep=getDoubleVal(step);
    if(dstep == 0) return NIL;
  }
  HERE();
  int imax=ceil(fabs(getDoubleVal(lisp_sub(stop,start))/dstep));
  cons* newlist=xmalloc(sizeof(cons)*imax+1);
  double j=getDoubleVal(start);
  for(i=0;i<imax;i++){
    newlist[i].car=(sexp){.tag=_double,.val={.real64=j}};
    newlist[i].cdr=(sexp){.tag=_list,.val={.cons=&newlist[i+1]}};
    j+=dstep;
  }
  HERE();
  newlist[i-1].cdr=NIL;
  HERE();
  PRINT_MSG(print((sexp){.tag=_list,.val={.cons=newlist},.len=i}));
  HERE();
  return (sexp){.tag=_list,.val={.cons=newlist},.len=i};
}
static sexp qsort_acc(sexp ls,sexp(*f)(sexp,sexp)){
  //find a way to use length somehow
  if(!CONSP(cdr(ls))){
    return ls;//maybe sort if ls is a cons cell
  } else {
    sexp rhs,lhs,pivot,cur_cell;
    rhs.val.cons=xmalloc(sizeof(cons));
    lhs.val.cons=xmalloc(sizeof(cons));
    XCDR(lhs)=XCDR(rhs)=NIL;
    pivot=pop_cons(ls);//cost of finding a better pivot outweights the benifits
    while(CONSP(ls)){
      cur_cell=pop_cons(ls);
      if(f(cur_cell,pivot).tag!=-3){
        push_cons(cur_cell,lhs);
      } else {
        push_cons(cur_cell,rhs);
      }
    }
    lhs=qsort_acc(lhs,f);
    rhs=qsort_acc(rhs,f);
    //find a better way to do this
    XCDR(last(lhs))=pivot;
    XCDR(pivot)=rhs;
    return lhs;
  }
}
sexp qsort_cons(sexp ls,sexp sort_fn){
  if(!CONSP(ls) || !FUNP(sort_fn)){
    return error_sexp("qsort sort_fn type error");
  }
  sexp(*f)(sexp,sexp);
  env lambda_env;
  f=sort_fn.val.fun->fun.comp.f2;
  return qsort_acc(ls,f);
}
/*sexp assoc(sexp ls,sexp obj,sexp eq_fn){
  if(!CONSP(ls)){
    return error_sexp("argument 1 of assoc must be an alist");
  }
  if(NILP(eq_fn)){
    eq_fn=lisp_eq;
  }
  while(CONSP(ls)){
    if(isTrue(eq_fn(XCAR(ls),obj))){
      return XCAR(ls);
    } 
    ls=XCDR(ls);
  }
  return NIL;
  }*/
/*sexp assq(sexp ls, sexp obj){
  return assoc(ls,obj,lisp_eq);
  }*/
