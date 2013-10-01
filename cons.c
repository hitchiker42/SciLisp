#include "common.h"
#include "car.h"
sexp mklist(sexp head,...){
  PRINT_MSG("Making a list");
  cons* list=xmalloc(sizeof(cons)),*next=xmalloc(sizeof(cons));
  list->car=head;  
  va_list ap;
  sexp cur_loc,next_sexp;
  cons*next_cell;
  va_start(ap,head);
  while((cur_loc=va_arg(ap,sexp)).tag != _nil){
    next->car=cur_loc;
    next->cdr.val.cons=xmalloc(sizeof(cons));
    list->cdr=(sexp){_cons,(data)(cons*)next};
    next=next->cdr.val.cons;
  }
  next->cdr=NIL;
  return (sexp){_cons,(data)(cons*)list};
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
    list->cdr=(sexp){_cons,(data)(cons*)next};
    next=next->cdr.val.cons;
  }
  cur_loc=va_arg(ap,sexp);
  next->cdr=cur_loc;
  return (sexp){_cons,(data)(cons*)list};
}
#define CAR(cell) cell->car
#define CDR(cell) cell->cdr
sexp nreverse(sexp ls){
  cons* cur_cell=ls.val.cons,next_cell=CDR(cur_cell).val.cons;
  sexp last_val=NIL;
  while(CDR(cur_cell).tag != _nil){
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
    
    
sexp reduce(sexp ls,(sexp)(*reduce_fn)(sexp,sexp)){
  sexp result=car(ls);
  while(cdr(ls).tag != _nil){
    ls=cdr(ls);
    result=reduce_fn(car(ls),result);
  }
}
sexp lisp_sum(sexp next_elem,sexp acc){
  return double_sexp(getDoubleVal(next_elem)+getDoubleVal(acc));
}
