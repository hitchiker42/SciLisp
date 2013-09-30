#ifndef __PRIM_H_
#define __PRIM_H_
#include "common.h"
#include <math.h>
#include "cons.h"
#define op_to_fun(op,fun_name,type)\
  static inline type fun_name(type x,type y){   \
    return x op y;                              \
  }
#define makeSyms(symName,fun_name,lisp_name)    \
  symref symName={lisp_name,(sexp){_fun,(data)(void*)fun_name}};        \
  symref* symName##ptr=&symName;                                        \
  addSym(symName##ptr)
#define makeConstDbl(symName,value,lisp_name)   \
  symref symName={lisp_name,(sexp){_double,(data)(double)value}};     \
  symref* symName##ptr=&symName;                                        \
  addSym(symName##ptr);
#define makeConstLong(symName,value,lisp_name)   \
  symref symName={lisp_name,(sexp){_double,(data)(long)value}};         \
  symref* symName##ptr=&symName;                                        \
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
//args are (name in c,value,name in lisp)       
#define initPrims()\
  symbolTable=NULL;\
  makeSyms(PRIM_fadd_sym,lisp_fadd,"+");\
  makeSyms(PRIM_fsub_sym,lisp_fsub,"-");\
  makeSyms(PRIM_fmul_sym,lisp_fmul,"*");\
  makeSyms(PRIM_fdiv_sym,lisp_fdiv,"/");\
  makeSyms(PRIM_logxor_sym,lisp_xor,"^");\
  makeSyms(PRIM_logand_sym,lisp_logand,"logand");\
  makeSyms(PRIM_logor_sym,lisp_logor,"logor");\
  makeSyms(PRIM_Princ,princ,"princ");\
  makeConstDbl(PRIM_Mach_Eps,1.41484755040568800000e+16,"Meps");\
  makeConstDbl(PRIM_eulers_number,2.7182818284590452354,"e");\
  makeConstDbl(PRIM_pi,3.14159265358979323846,"pi");
void memoizePrims(){}
#endif  
