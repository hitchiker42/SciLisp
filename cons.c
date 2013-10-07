/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
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
    list->cdr=(sexp){.tag=_cons,(data)(cons*)next};
    next=next->cdr.val.cons;
  }
  cur_loc=va_arg(ap,sexp);
  next->cdr=cur_loc;
  return (sexp){.tag=_cons,(data)(cons*)list};
}
#define CAR(cell) cell->car
#define CDR(cell) cell->cdr
sexp nreverse(sexp ls){
  cons* cur_cell=ls.val.cons,*next_cell=CDR(cur_cell).val.cons;
  sexp last_val=NIL;
  while(!NILP(CDR(cur_cell))){
    CDR(cur_cell)=last_val;//update ptr of current cell
    last_val=CDR(next_cell);//get ptr to current cell
    cur_cell=next_cell;//update current cell
    next_cell=CDR(cur_cell).val.cons;//update next cell, unchecked union access
  } 
  CDR(cur_cell)=last_val;
  return cons_sexp(cur_cell);
}
#undef CAR
#undef CDR    
    
    
sexp reduce(sexp ls,sexp reduce_fn){
  if(!CONSP(ls) || !FUNP(reduce_fn)){
    //error;
  }
  sexp result=car(ls);
  sexp(*f)(sexp,sexp)=reduce_fn.val.fun->fxn_call.f2;
  while(cdr(ls).tag != _nil){
    ls=cdr(ls);
    result=f(car(ls),result);
  }
}
sexp mapcar(sexp ls,sexp map_fn){
  if(!CONSP(ls) || !FUNP(map_fn)){
    //error;
  }
  sexp result;
  cons* cur_cell=result.val.cons=xmalloc(sizeof(cons));
  result.tag=_cons;
  sexp(*f)(sexp)=map_fn.val.fun->fxn_call.f1;
  while(!NILP(cdr(ls))){
    cur_cell->car=f(car(ls));
    cur_cell->cdr.val.cons=xmalloc(sizeof(cons));
    cur_cell=cur_cell->cdr.val.cons;
    ls=cdr(ls);
  }
  cur_cell->car=f(car(ls));
  cur_cell->cdr=NIL;
  return result;
}
