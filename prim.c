/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "prim.h"
op_to_fun(+,lisp_fadd,double);
op_to_fun(-,lisp_fsub,double);
op_to_fun(*,lisp_fmul,double);
op_to_fun(/,lisp_fdiv,double);
op_to_fun(+,lisp_iadd,long);
op_to_fun(-,lisp_isub,long);
op_to_fun(*,lisp_imul,long);
op_to_fun(/,lisp_idiv,long);
op_to_fun(^,lisp_xor,long);
op_to_fun(>>,lisp_rshift,long);
op_to_fun(<<,lisp_lshift,long);
op_to_fun(&,lisp_logand,long);
op_to_fun(|,lisp_logor,long);
static long ash(long x,long y){
  if(y<=0){return x >> y;}
  else{return x<<(-y);}
}
static sexp lisp_add(sexp x){
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
#define mkFxnProto(lisp_name,c_name,numargs)\
  static funcall c_name##call=(funcall)(struct f##numargs)\
  {(sexp[numargs]){},c_name};\
static fxn_proto c_name##struct=\
  (fxn_proto){#c_name,lisp_name,numargs,&c_name##call}
//args are (name in c,value,name in lisp)       
#define initPrims()\
  globalSymbolTable.head=NULL;                                      \
  DEFUN("add",lisp_add,1,-1);                                       \
  DEFUN("+",lisp_fadd,2,2);                                         \
  DEFUN("-",lisp_fsub,2,2);                                         \
  DEFUN("*",lisp_fmul,2,2);                                         \
  DEFUN("/",lisp_fdiv,2,2);                                         \
  DEFUN("logxor",lisp_xor,2,2);                                    \
  DEFUN("logand",lisp_logand,2,2);                              \
  DEFUN("logor",lisp_logor,2,2);                                    \
  DEFUN("print",print,1,1);                                         \
  DEFCONST("Meps",lisp_mach_eps,double,_double);                    \
  DEFCONST("pi",lisp_pi,double,_double);                            \
  DEFCONST("e",lisp_euler,double,_double);
