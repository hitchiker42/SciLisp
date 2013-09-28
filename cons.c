#include "common.h"
sexp mklist(sexp head,tail...){
  cons* list=xmalloc(sizeof(cons)),next=xmalloc(sizeof(cons));
  list->car=head;  
  va_list ap;
  sexp cur_loc,next_sexp;
  cons*next_cell;
  va_start(ap,head);
  while((cur_loc=va_arg(ap,sexp))!= NIL){
    next->car=cur_loc;
    next->cdr=xmalloc(sizeof(cons));
    list->cdr=(sexp){_cons,(data)(cons*)next};
    next=next->cdr;
  }
  next->cdr=NIL;
  return (sexp){_cons,(data)(cons*)list};
}
sexp mkImproper(sexp head,...){
  cons* list=xmalloc(sizeof(cons)),next=xmalloc(sizeof(cons));
  list->car=head;  
  va_list ap;
  sexp cur_loc,next_sexp;
  cons*next_cell;
  va_start(ap,head);
  while((cur_loc=va_arg(ap,sexp))!= NIL){
    next->car=cur_loc;
    next->cdr=xmalloc(sizeof(cons));
    list->cdr=(sexp){_cons,(data)(cons*)next};
    next=next->cdr;
  }
  cur_loc=va_arg(ap,sexp);
  next->cdr=cur_loc;
  return (sexp){_cons,(data)(cons*)list};
