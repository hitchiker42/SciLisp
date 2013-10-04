/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __PRIM_H_
#define __PRIM_H_
#include "common.h"
#include "cons.h"
#define doubleBinOp(function) (fxn_proto){2,
#define op_to_fun(op,fun_name,type)\
  static inline type fun_name(type x,type y){   \
    return x op y;                              \
  }
#define DEFUN(symName,fun_name,lisp_name)    \
  symbol symName={lisp_name,(sexp){_fun,(data)(void*)fun_name}};        \
  symref symName##ptr=&symName;                                        \
  addSym(symName##ptr)
#define DEFUN_typed(symName,fun_name,lisp_name,num_args,types...)       \
  fxn_proto symName##proto=(fxn_proto){num_args,##types,(void*)fun_name}; \
  symbol symName={lisp_name,(sexp){_fun,(data)(fxn_ptr)&symName##proto}}; \
  symref symName##ptr=&symName;                                        \
  addSym(symName##ptr)
#define DEFCONSTDBL(symName,value,lisp_name)   \
  symbol symName={lisp_name,(sexp){_double,(data)(double)value}};     \
  symref symName##ptr=&symName;                                        \
  addSym(symName##ptr);
#define DEFCONSTLONG(symName,value,lisp_name)   \
  symbol symName={lisp_name,(sexp){_double,(data)(long)value}};         \
  symref symName##ptr=&symName;                                        \
  addSym(symName##ptr);
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
static inline long ash(long x,long y){
  if(y<=0){return x >> y;}
  else{return x<<(-y);}
}
static inline sexp lisp_add(sexp x){
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
//args are (name in c,value,name in lisp)       
#define initPrims()\
  symbolTable=NULL;\
  DEFUN(PRIM_add_sym,lisp_add,"add");\
  DEFUN_typed(PRIM_fadd_typed,lisp_fadd,"fadd",2,_double,_double);      \
  DEFUN(PRIM_fadd_sym,lisp_fadd,"+");\
  DEFUN(PRIM_fsub_sym,lisp_fsub,"-");\
  DEFUN(PRIM_fmul_sym,lisp_fmul,"*");\
  DEFUN(PRIM_fdiv_sym,lisp_fdiv,"/");\
  DEFUN(PRIM_logxor_sym,lisp_xor,"^");\
  DEFUN(PRIM_logand_sym,lisp_logand,"logand");\
  DEFUN(PRIM_logor_sym,lisp_logor,"logor");\
  DEFUN(PRIM_Print,print,"print");\
  DEFCONSTDBL(PRIM_Mach_Eps,1.41484755040568800000e+16,"Meps");\
  DEFCONSTDBL(PRIM_eulers_number,2.7182818284590452354,"e");\
  DEFCONSTDBL(PRIM_pi,3.14159265358979323846,"pi");
//should write symbol table to a file
void memoizePrims(){}

#endif  
