/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __CONS_H__
#define __CONS_H__
#include "common.h"
//create a nil terminated list from a variable number of sexps
//because of how vararg functions work in c the arguments must end
//with a nil value
sexp mklist(sexp head,...);
//create an improper list, because of how c varargs functions work
//the 2nd to last argument must be nil, this argument is ignored
//and the last argument is set to the final cdr of the list
sexp mkImproper(sexp head,...);
//destructively reverse a list
sexp nreverse(sexp ls);
//apply reduce_fn(a functon of the form f(sexp,sexp)-> sexp to
//the members of ls iteratively to produce a single sexp result
//ls must be at least 2 elements long
sexp reduce(sexp ls,sexp reduce_fn);
//create a new list formed by applying map_fn(f(sexp)->sexp) to
//each element of ls in turn
sexp mapcar(sexp ls,sexp map_fn);
//typechecked car function
static inline sexp car(sexp cell){
  if(NILP(cell)){return NIL;}
  if(!CONSP(cell)){my_err("Argument not a cons cell\n");}
  else return cell.val.cons->car;
}
//typechecked cdr function
static inline sexp cdr(sexp cell){
  if(!(CONSP(cell))){my_err("Argument not a cons cell\n");}
  else return cell.val.cons->cdr;
}
//get nth member of a list using typechecked car
static inline sexp nth(sexp cell,int n){
  while(n>0){
    cell=car(cell);
    n++;
  }
  return cell;
}
//create a cons cell from 2 sexps, result may or may not be a list
static sexp Cons(sexp car_cell,sexp cdr_cell){
  sexp retval;
  retval.tag=_cons;
  retval.val.cons=GC_malloc(sizeof(cons));
  retval.val.cons->car=car_cell;
  retval.val.cons->cdr=cdr_cell;
  return retval;
}
//set car af a cons cell,non type checked (probably should be)
static inline sexp set_car(sexp cell,sexp new_val){
  return (cell.val.cons->car=new_val);
}
//type checked car/cdr extensions
static inline sexp caar(sexp cell){return car(car(cell));}
static inline sexp cadr(sexp cell){return car(cdr(cell));}
static inline sexp cdar(sexp cell){return cdr(car(cell));}
static inline sexp cddr(sexp cell){return cdr(cdr(cell));}
static inline sexp caaar(sexp cell){return car(car(car(cell)));}
static inline sexp caadr(sexp cell){return car(car(cdr(cell)));}
static inline sexp caddr(sexp cell){return car(cdr(cdr(cell)));}
static inline sexp cdddr(sexp cell){return cdr(cdr(cdr(cell)));}
static inline sexp cddar(sexp cell){return cdr(cdr(car(cell)));}
static inline sexp cdaar(sexp cell){return cdr(car(car(cell)));}
static inline sexp cadar(sexp cell){return car(cdr(car(cell)));}
static inline sexp cdadr(sexp cell){return cdr(car(cdr(cell)));}
//non typechecked car/cdr/etc macros, for use when type checking has been
//done in some other way. Don't use these if you don't know than the argument
//has to be a cons cell
#define XCAR(cell) cell.val.cons->car
#define XCDR(cell) cell.val.cons->cdr
#define XCAAR(cell) XCAR(XCAR(cell))
#define XCADR(cell) XCAR(XCDR(cell))
#define XCDAR(cell) XCDR(XCAR(cell))
#define XCDDR(cell) XCDR(XCDR(cell))
#define XCAAAR(cell) XCAR(XCAR(XCAR(cell)))
#define XCAADR(cell) XCAR(XCAR(XCDR(cell)))
#define XCADDR(cell) XCAR(XCDR(XCDR(cell)))
#define XCDDDR(cell) XCDR(XCDR(XCDR(cell)))
#define XCDDAR(cell) XCDR(XCDR(XCAR(cell)))
#define XCDAAR(cell) XCDR(XCAR(XCAR(cell)))
#define XCADAR(cell) XCAR(XCDR(XCAR(cell)))
#define XCDADR(cell) XCDR(XCAR(XCDR(cell)))
#endif



