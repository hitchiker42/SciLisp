/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __CONS_H__
#define __CONS_H__
#include "common.h"
//create a nil terminated list from a variable number of sexps
//because of how vararg functions work in c the argumentbs must end
//with a nil value
sexp mklist(sexp head,...);
//create an improper list, because of how c varargs functions work
//the 2nd to last argument must be nil, this argument is ignored
//and the last argument is set to the final cdr of the list
sexp mkImproper(sexp head,...);
//destructively reverse a list
sexp nreverse(sexp ls);
sexp cons_nreverse(sexp ls);
//non-destructively reverse a list
sexp cons_reverse(sexp ls);
//destructively append lists, argument is itself a list of lists to append
//need to change some stuff with function args(or at least check some stuff)
//before this can be added to lisp
sexp nappend(sexp conses);
//apply reduce_fn(a functon of the form f(sexp,sexp)-> sexp to
//the members of ls iteratively to produce a single sexp result
//ls must be at least 2 elements long
sexp cons_reduce(sexp ls,sexp reduce_fn,sexp start);
//create a new list formed by applying map_fn(f(sexp)->sexp) to
//each car of ls in turn
sexp mapcar(sexp ls,sexp map_fn);
//check len field of sexp for length
//if len==0 compute length using tail recursion
sexp cons_length(sexp ls)__attribute__((pure));
//list based equivlant to apl iota function
sexp list_iota(sexp start,sexp stop,sexp step);
//create a cons cell from 2 sexps, result may or may not be a list
sexp Cons(sexp car_cell,sexp cdr_cell);
//cons three args, return (car_cell . (cadr_cell . cddr_cell))
sexp Cons2(sexp car_cell,sexp cadr_cell,sexp cddr_cell);
static inline sexp c_list1(sexp cell){
  return Cons(cell,NIL);
}
static inline sexp c_list2(sexp cell1,sexp cell2){
  return Cons2(cell1,cell2,NIL);
}
sexp raw_cons(sexp car_cell,sexp cdr_cell);
//sort ls (mostly in place) using sort_fn to compair elements
sexp cons_qsort(sexp ls,sexp sort_fn);
//functions on alists
//general assoication function, look for an object in ls that is equal to obj
//according to eq_fn, default eq
sexp assoc(sexp ls,sexp obj,sexp eq_fn);
//assoc with eq_fn explictly set to eq
sexp assq(sexp ls, sexp obj);
//recursively copy ls
sexp copy_cons(sexp ls);
//return a cons cell containing a copy of the first num elements
//of ls and the list begining at the num'th value of ls
//mostly used as a means of implementing drop and take
sexp cons_split(sexp ls,sexp num);
//return the first num elements of ls
sexp cons_take(sexp ls,sexp num);
//return the list starting at the num'th element of ls
sexp cons_drop(sexp ls,sexp num);
//typechecked car function
static sexp car(sexp cell) __attribute__((pure,hot));
static sexp cdr(sexp cell) __attribute__((pure,hot));
static inline sexp car(sexp cell){
  if(NILP(cell)){return NIL;}
  if(!CONSP(cell)){
    return error_sexp("car error");
  }
  else return cell.val.cons->car;
}
//typechecked cdr function
static inline sexp cdr(sexp cell){
  if(!(CONSP(cell))){//my_err("Argument not a cons cell\n");}
    return error_sexp("cdr error");
  }
  else return cell.val.cons->cdr;
}
#define XCAR(cell) cell.val.cons->car
#define XCDR(cell) cell.val.cons->cdr
//get nth member of a list using typechecked car
static inline sexp nth(sexp cell,int64_t n){
  while(n>0 && CONSP(cell)){
    cell=XCDR(cell);
    n--;
  }
  return (n==0 ? cell : error_sexp("nth error, index greater than length of list"));
}
static inline sexp last(sexp cell){
  while(!NILP(cdr(cell))){
    cell=XCDR(cell);
  }
  return cell;
}
static inline sexp pop_cons(sexp ls){
  if(!(CONSP(ls))){
    return error_sexp("pop! type error, expected cons cell or list");
  } else {
    sexp retval=XCAR(ls);
    //why do I need to do this
    /*    ls.val.cons->car=cdr(ls).val.cons->car;
          ls.val.cons->cdr=cdr(ls).val.cons->cdr;*/
    if(NILP(XCDR(ls))){
      ls.val.cons->car=NIL;
    } else {
      *(ls.val.cons)=*(XCDR(ls).val.cons);
    }
    return retval;
  }
}
static inline sexp unsafe_pop(sexp cell){
  sexp retval=XCAR(cell);
  if(NILP(XCDR(cell))){
    cell.val.cons->car=NIL;
  } else {
    *(cell.val.cons)=*(XCDR(cell).val.cons);
  }
  return retval;
}
static sexp as_list(sexp obj){
  if(CONSP(obj)){
    return obj;
  } else {
    sexp retval;
    retval.val.cons=xmalloc(sizeof(cons));
    XCAR(retval)=obj;
    XCDR(retval)=NIL;
    retval.tag=_list;
    return retval;
  }
}
static sexp push_cons(sexp obj,sexp ls){
  if(!(CONSP(ls))){
    return format_type_error("push!","cons cell",ls.tag);    
  } else {
    sexp new_cons;
    new_cons.val.cons=xmalloc(sizeof(cons));    
    XCDR(new_cons).val.cons=xmalloc(sizeof(cons));
    new_cons.tag=_list;
    new_cons.len=(ls.len+1);
    XCAR(new_cons)=obj;
    XCDR(new_cons).val.cons->car=car(ls);
    XCDR(new_cons).val.cons->cdr=cdr(ls);
    *(ls.val.cons)=*(new_cons.val.cons);
    return ls;
  }
}
//set car af a cons cell,non type checked (probably should be)
static inline sexp set_car(sexp cell,sexp new_val){
  if(!(CONSP(cell))){
    return error_sexp("set_car type error, expected cons cell or list");
  }
  return (cell.val.cons->car=new_val);
}
static inline sexp set_cdr(sexp cell,sexp new_val){
  if(!(CONSP(cell))){
    return error_sexp("set_cdr type error, expected cons cell or list");
  }
  return (cell.val.cons->cdr=new_val);
}
sexp cons_equal(sexp ls1,sexp ls2);
//type checked car/cdr extensions (should any of these be inlined?)
static inline sexp caar(sexp cell){return car(car(cell));}
static inline sexp cadr(sexp cell){return car(cdr(cell));}
static inline sexp cdar(sexp cell){return cdr(car(cell));}
static inline sexp cddr(sexp cell){return cdr(cdr(cell));}
static sexp caaar(sexp cell){return car(car(car(cell)));}
static sexp caadr(sexp cell){return car(car(cdr(cell)));}
static sexp caddr(sexp cell){return car(cdr(cdr(cell)));}
static sexp cdddr(sexp cell){return cdr(cdr(cdr(cell)));}
static sexp cddar(sexp cell){return cdr(cdr(car(cell)));}
static sexp cdaar(sexp cell){return cdr(car(car(cell)));}
static sexp cadar(sexp cell){return car(cdr(car(cell)));}
static sexp cdadr(sexp cell){return cdr(car(cdr(cell)));}
static sexp caaaar(sexp cell){return car(car(car(car(cell))));}
static sexp caaadr(sexp cell){return car(car(car(cdr(cell))));}
static sexp caadar(sexp cell){return car(car(cdr(car(cell))));}
static sexp caaddr(sexp cell){return car(car(cdr(cdr(cell))));}
static sexp cadaar(sexp cell){return car(cdr(car(car(cell))));}
static sexp cadadr(sexp cell){return car(cdr(car(cdr(cell))));}
static sexp caddar(sexp cell){return car(cdr(cdr(car(cell))));}
static sexp cadddr(sexp cell){return car(cdr(cdr(cdr(cell))));}
static sexp cdaaar(sexp cell){return cdr(car(car(car(cell))));}
static sexp cdaadr(sexp cell){return cdr(car(car(cdr(cell))));}
static sexp cdadar(sexp cell){return cdr(car(cdr(car(cell))));}
static sexp cdaddr(sexp cell){return cdr(car(cdr(cdr(cell))));}
static sexp cddaar(sexp cell){return cdr(cdr(car(car(cell))));}
static sexp cddadr(sexp cell){return cdr(cdr(car(cdr(cell))));}
static sexp cdddar(sexp cell){return cdr(cdr(cdr(car(cell))));}
static sexp cddddr(sexp cell){return cdr(cdr(cdr(cdr(cell))));}

//non typechecked car/cdr/etc macros, for use when type checking has been
//done in some other way. Don't use these if you don't know that the argument
//has to be a cons cell
#define XCAAR(CELL) XCAR(XCAR(CELL))
#define XCADR(CELL) XCAR(XCDR(CELL))
#define XCDAR(CELL) XCDR(XCAR(CELL))
#define XCDDR(CELL) XCDR(XCDR(CELL))
#define XCAAAR(CELL) XCAR(XCAR(XCAR(CELL)))
#define XCAADR(CELL) XCAR(XCAR(XCDR(CELL)))
#define XCADDR(CELL) XCAR(XCDR(XCDR(CELL)))
#define XCDDDR(CELL) XCDR(XCDR(XCDR(CELL)))
#define XCDDAR(CELL) XCDR(XCDR(XCAR(CELL)))
#define XCDAAR(CELL) XCDR(XCAR(XCAR(CELL)))
#define XCADAR(CELL) XCAR(XCDR(XCAR(CELL)))
#define XCDADR(CELL) XCDR(XCAR(XCDR(CELL)))
#define XCAAAAR(CELL) XCAR(XCAR(XCAR(XCAR(CELL))))
#define XCAAADR(CELL) XCAR(XCAR(XCAR(XCDR(CELL))))
#define XCAADAR(CELL) XCAR(XCAR(XCDR(XCAR(CELL))))
#define XCAADDR(CELL) XCAR(XCAR(XCDR(XCDR(CELL))))
#define XCADAAR(CELL) XCAR(XCDR(XCAR(XCAR(CELL))))
#define XCADADR(CELL) XCAR(XCDR(XCAR(XCDR(CELL))))
#define XCADDAR(CELL) XCAR(XCDR(XCDR(XCAR(CELL))))
#define XCADDDR(CELL) XCAR(XCDR(XCDR(XCDR(CELL))))
#define XCDAAAR(CELL) XCDR(XCAR(XCAR(XCAR(CELL))))
#define XCDAADR(CELL) XCDR(XCAR(XCAR(XCDR(CELL))))
#define XCDADAR(CELL) XCDR(XCAR(XCDR(XCAR(CELL))))
#define XCDADDR(CELL) XCDR(XCAR(XCDR(XCDR(CELL))))
#define XCDDAAR(CELL) XCDR(XCDR(XCAR(XCAR(CELL))))
#define XCDDADR(CELL) XCDR(XCDR(XCAR(XCDR(CELL))))
#define XCDDDAR(CELL) XCDR(XCDR(XCDR(XCAR(CELL))))
#define XCDDDDR(CELL) XCDR(XCDR(XCDR(XCDR(CELL))))
/*(dolist (i '(?a ?d))
  (dolist (j '(?a ?d))
  (dolist (k '(?a ?d))
  (dolist (l '(?a ?d))
  (insert (format "static sexp c%c%c%c%cr(sexp cell){return c%cr(c%cr(c%cr(c%cr(cell))));}\n"))
  (insert (upcase (format "#define XC%c%c%c%cr(cell) XC%cR(XC%cR(XC%cR(XC%cR(cell))))\n")))
  i j k l i j k l)))))
*/
//fifo queues
//these are for use in c as they are now
#define C_QUEUE_EMPTY(queue)                    \
  (NILP(XCAR(queue)))
void enqueue(sexp queue,sexp val);
sexp dequeue(sexp queue);
//while these are for use in lisp
sexp make_queue(sexp initial_contents);
sexp lisp_enqueue(sexp val,sexp queue);
sexp lisp_dequeue(sexp queue,sexp noerror);
sexp queue_empty(sexp queue);
sexp queue_peek(sexp queue);
sexp cons_merge_sort(sexp ls,sexp sort_fn);
#endif
