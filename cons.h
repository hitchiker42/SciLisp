#ifndef __CONS_H__
#define __CONS_H__
sexp mklist(sexp head,...);
sexp mkImproper(sexp head,...);
static inline sexp car(sexp cell){
  if(cell.tag == _nil){return NIL;}
  if(cell.tag != _cons){my_err("Argument not a cons cell");}
  else return cell.val.cons->car;
}
static inline sexp cdr(sexp cell){
  if(cell.tag != _cons){my_err("Argument not a cons cell");}
  else return cell.val.cons->cdr;
}
static inline sexp nth(sexp cell,int n){
  while(n>0){
    cell=car(cell);
    n++;
  }
  return cell;
}
static sexp Cons(sexp car_cell,sexp cdr_cell){
  sexp retval;
  retval.tag=_cons;
  retval.val.cons=xmalloc(sizeof(cons));
  retval.val.cons->car=car_cell;
  retval.val.cons->car=cdr_cell;
  return retval;
}
static inline sexp caar(sexp cell){return car(car(cell));}
static inline sexp cadr(sexp cell){return car(cdr(cell));}
static inline sexp cdar(sexp cell){return cdr(car(cell));}
static inline sexp cddr(sexp cell){return cdr(cdr(cell));}
static inline sexp caaar(sexp cell){return car(car(car(cell)));}
static inline sexp caadr(sexp cell){return car(cdr(car(cell)));}
static inline sexp caddr(sexp cell){return car(cdr(cdr(cell)));}
static inline sexp cdddr(sexp cell){return cdr(cdr(cdr(cell)));}
static inline sexp cddar(sexp cell){return cdr(cdr(car(cell)));}
static inline sexp cdaar(sexp cell){return cdr(car(car(cell)));}
static inline sexp cadar(sexp cell){return car(cdr(car(cell)));}
static inline sexp cdadr(sexp cell){return cdr(car(cdr(cell)));}
#endif
