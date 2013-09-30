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
#endif
