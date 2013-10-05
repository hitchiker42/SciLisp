/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __PRIM_H_
#define __PRIM_H_
#include "common.h"
#include "cons.h"
#define binop_to_fun(op,fun_name,type)\
  static inline sexp fun_name(sexp x,sexp y){   \
    if(x.tag==y.tag==_long){return                         \
        (sexp){_long,x.val.int64 op y.val.int64};}         \
    else {                                                 \
    register double xx=getDoubleVal(x);\
    register double yy=getDoubleVal(y);\
    return (sexp){_double,{.real64=(xx op yy)}};}       \
  }
//ignore tags, allow logical operations on doubles
//be careful about this
#define lop_to_fun(op,fun_name,type)\
  static inline sexp fun_name(sexp x,sexp y){   \
    return (sexp){_long,x.val.int64 op y.val.int64};    \
  }
#define DEFUN_INTERN(lisp_name,c_name)                                  \
  symbol c_name ## _sym=(symbol){lisp_name,(sexp){_fun,{.fun = &c_name##call}},0}; \
  symref c_name ## _ptr=&c_name##_sym;                                  \
   addSym(c_name##_ptr)
#define DEFCONST(lisp_name,c_name,name,lisp_type)     \
  symbol c_name ## _sym={lisp_name,(sexp){lisp_type,{.name = c_name}},0}; \
  symref c_name ## _ptr=&c_name##_sym;                                  \
addSym(c_name##_ptr);
#define DEFUN_ARGS_0	(void)
#define DEFUN_ARGS_1	(sexp)
#define DEFUN_ARGS_2	(sexp, sexp)
#define DEFUN_ARGS_3	(sexp, sexp, sexp)
#define DEFUN_ARGS_4	(sexp, sexp, sexp, sexp)
#define DEFUN_ARGS_5	(sexp, sexp, sexp, sexp, \
                         sexp)
#define DEFUN_ARGS_6	(sexp, sexp, sexp, sexp, \
                         sexp, sexp)
#define DEFUN_ARGS_7	(sexp, sexp, sexp, sexp, \
                         sexp, sexp, sexp)
#define DEFUN_ARGS_8	(sexp, sexp, sexp, sexp, \
                         sexp, sexp, sexp, sexp)
//should write symbol table to a file
void memoizePrims(){}
binop_to_fun(+,lisp_add,double);
binop_to_fun(-,lisp_sub,double);
binop_to_fun(*,lisp_mul,double);
binop_to_fun(/,lisp_div,double);
lop_to_fun(^,lisp_xor,long);
lop_to_fun(>>,lisp_rshift,long);
lop_to_fun(<<,lisp_lshift,long);
lop_to_fun(&,lisp_logand,long);
lop_to_fun(|,lisp_logor,long);
static long ash(long x,long y){
  if(y<=0){return x >> y;}
  else{return x<<(-y);}
}
static sexp lisp_adds(sexp x){
  double sum=0;
  if(x.tag == _double||x.tag==_long){return x;}
  if(x.tag == _cons){
    do{
      switch(car(x).tag){
        case _long:
          sum+=(double)x.val.int64;
          break;
        case _double:
          sum+=x.val.real64;
          break;
      }
    } while ((x=cdr(x)).tag != _nil);
  } return (sexp){_double,(data)(double)sum};
}
static const double lisp_mach_eps=1.41484755040568800000e+16;
static const double lisp_pi=3.14159265358979323846;
static const double lisp_euler=2.7182818284590452354;
#define DEFUN(lname,cname,minargs,maxargs)  \
  static fxn_proto cname##call=             \
    { #cname, lname, minargs, maxargs, {.f##maxargs=cname}};
DEFUN("+",lisp_add,2,2);
DEFUN("-",lisp_sub,2,2);
DEFUN("*",lisp_mul,2,2);
DEFUN("/",lisp_div,2,2);
DEFUN("logxor",lisp_xor,2,2);
DEFUN("logand",lisp_logand,2,2);
DEFUN("logor",lisp_logor,2,2);
DEFUN("print",lisp_print,1,1);

#define initPrims()                              \
globalSymbolTable.head=NULL;\
DEFUN_INTERN("+",lisp_add);                      \
DEFUN_INTERN("-",lisp_sub);                      \
DEFUN_INTERN("*",lisp_mul);                      \
DEFUN_INTERN("/",lisp_div);                      \
DEFUN_INTERN("logxor",lisp_xor);                  \
DEFUN_INTERN("logand",lisp_logand);               \
DEFUN_INTERN("logor",lisp_logor);                 \
DEFUN_INTERN("print",lisp_print);                  \
DEFCONST("Meps",lisp_mach_eps,real64,_double);      \
DEFCONST("pi",lisp_pi,real64,_double);               \
DEFCONST("e",lisp_euler,real64,_double);
#endif
