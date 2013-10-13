/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include "prim.h"
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


sexp reduce(sexp ls,sexp reduce_fn){
  if(!CONSP(ls) || !FUNP(reduce_fn)){
    //error;
  }
  sexp result=XCAR(ls);
  sexp(*f)(sexp,sexp);
  switch(reduce_fn.tag){
    case _fun:
      f=reduce_fn.val.fun->fxn_call.f2;
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
  env lambda_env;
  if(FUNP(map_fn)){
    f=map_fn.val.fun->fxn_call.f1;  
  } else {
    lambda_env=(env){.enclosing = (env*)&map_fn.val.lam->env,
                .head={.local=map_fn.val.lam->env.head},.tag=0};
  }
  while(!NILP(XCDR(ls))){
    if(FUNP(map_fn)){
      cur_cell->car=f(car(ls));
    } else {
      cur_cell->cdr=eval(Cons(map_fn,Cons(car(ls),NIL)),lambda_env);
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
    cons* newlist=xmalloc(sizeof(cons)*imax);
    for(i=0;i<imax;i++){
      newlist[i].car=(sexp){.tag=_long,.val={.int64=i}};
      newlist[i].cdr=(sexp){.tag=_cons,.val={.cons=&newlist[i+1]}};
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
  int imax=ceil(fabs(getDoubleVal(lisp_sub(stop,start))/dstep));
  cons* newlist=xmalloc(sizeof(cons)*imax);
  double j=getDoubleVal(start);
  for(i=0;i<imax;i++){
    newlist[i].car=(sexp){.tag=_double,.val={.real64=j}};
    newlist[i].cdr=(sexp){.tag=_cons,.val={.cons=&newlist[i+1]}};    
    j+=dstep;
  }
  newlist[i-1].cdr=NIL;
  return (sexp){.tag=_list,.val={.cons=newlist},.len=i};
}
