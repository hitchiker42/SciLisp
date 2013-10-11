/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __PRIM_H_
#define __PRIM_H_
#include "common.h"
#include "cons.h"
#include "print.h"
#include <time.h>
#define binop_to_fun(op,fun_name)                               \
  static inline sexp fun_name(sexp x,sexp y){                   \
    if((x.tag==y.tag)==_long){                                  \
      return (sexp){.tag=_long,.val=                            \
          {.int64 = (x.val.int64 op y.val.int64)}};             \
    } else if(NUMBERP(x)&&NUMBERP(y)){                          \
      register double xx=getDoubleVal(x);                       \
      register double yy=getDoubleVal(y);                       \
      return (sexp){.tag=_double,.val={.real64=(xx op yy)}};    \
    } else {return NIL;}                                        \
  }
#define mkLisp_cmp(op,cname)                                            \
  static inline sexp cname(sexp x,sexp y){                              \
    if((x.tag == y.tag)==_long){                                        \
      return (x.val.int64 op y.val.int64 ? LISP_TRUE : LISP_FALSE);     \
    } else if(NUMBERP(x)&&NUMBERP(y)){                                  \
      register double xx=getDoubleVal(x);                               \
      register double yy=getDoubleVal(y);                               \
      return (xx op yy ? LISP_TRUE : LISP_FALSE);                       \
    } else {return NIL;}                                                \
  }
//ignore tags, allow logical operations on doubles
//be careful about this
#define lop_to_fun(op,fun_name)                                         \
  static inline sexp fun_name(sexp x,sexp y){                           \
    if((x.tag == y.tag)==_long){                                        \
      return (sexp){.tag=_long,.val={.int64=(x.val.int64 op y.val.int64)}}; \
    } else {return NIL;}                                                \
  }
/*  global_symbol c_name ## _sym=(global_symbol){lisp_name,(sexp){.tag=_fun,.val={.fun = &c_name##call}},0}; \
    global_symref c_name ## _ptr=&c_name##_sym;                                  */
#define DEFUN_INTERN(lname,cname)                                       \
  global_symbol cname ## _sym=(global_symbol){.name = lname,.val = {.tag=_fun,.val={.fun = &cname##call}}}; \
  global_symref cname ## _ptr=&cname##_sym;                             \
  addGlobalSymMacro(cname##_ptr)
#define DEFCONST(lisp_name,c_name)                                      \
  global_symbol c_name ## _sym={.name = lisp_name,.val = c_name};       \
  global_symref c_name ## _ptr=&c_name##_sym;                           \
  addGlobalSymMacro(c_name##_ptr);
#define MK_PREDICATE(lname,cname,tag)           \
  sexp cname(sexp obj){                         \
  if(obj.tag == tag){                           \
  return LISP_TRUE;
#define DEFUN(lname,cname,minargs,maxargs)                      \
  static fxn_proto cname##call=                                 \
    { #cname, lname, minargs, maxargs, {.f##maxargs=cname}};
#define DEFUN_ARGS_0	(void)
#define DEFUN_ARGS_1	(sexp)
#define DEFUN_ARGS_2	(sexp, sexp)
#define DEFUN_ARGS_3	(sexp, sexp, sexp)
#define DEFUN_ARGS_4	(sexp, sexp, sexp, sexp)
#define DEFUN_ARGS_5	(sexp, sexp, sexp, sexp,        \
                         sexp)
#define DEFUN_ARGS_6	(sexp, sexp, sexp, sexp,        \
                         sexp, sexp)
#define DEFUN_ARGS_7	(sexp, sexp, sexp, sexp,        \
                         sexp, sexp, sexp)
#define DEFUN_ARGS_8	(sexp, sexp, sexp, sexp,        \
                         sexp, sexp, sexp, sexp)
#define DEFUN_ARGS_MANY (...)
#define mkMathFun1(cname,lispname)                                      \
  static sexp lispname (sexp obj){                                      \
    if(obj.tag==_double){                                               \
      return (sexp){.tag=_double,.val={.real64 = cname(getDoubleVal(obj))}}; \
    } else {return NIL;}                                                \
  }
#define mkMathFun2(cname,lispname)                                      \
  static sexp lispname(sexp x,sexp y){                                  \
    if((x.tag == y.tag)==_double){                                      \
      register double xx=getDoubleVal(x);                               \
      register double yy=getDoubleVal(y);                               \
      return (sexp){.tag=_double,.val={.real64 = cname(xx,yy)}};        \
    } else {return NIL;}                                                \
  }
//create c functions for primitives
//arithmatic primitives
binop_to_fun(+,lisp_add);
binop_to_fun(-,lisp_sub);
binop_to_fun(*,lisp_mul);
binop_to_fun(/,lisp_div);
//bitwise primitives(need to add !)
lop_to_fun(^,lisp_xor);
lop_to_fun(>>,lisp_rshift);
lop_to_fun(<<,lisp_lshift);
lop_to_fun(&,lisp_logand);
lop_to_fun(|,lisp_logor);
//compairson primitives
mkLisp_cmp(>,lisp_gt);
mkLisp_cmp(<,lisp_lt);
mkLisp_cmp(>=,lisp_gte);
mkLisp_cmp(<=,lisp_lte);
mkLisp_cmp(!=,lisp_ne);
mkLisp_cmp(==,lisp_equals);
//math primitives
mkMathFun2(pow,lisp_pow);
mkMathFun1(sqrt,lisp_sqrt);
mkMathFun1(cos,lisp_cos);
mkMathFun1(sin,lisp_sin);
mkMathFun1(tan,lisp_tan);
mkMathFun1(exp,lisp_exp);
mkMathFun1(log,lisp_log);
static inline sexp lisp_abs(sexp x){
  if(x.tag==_long){return
      (sexp){.tag=_long,.val={.int64 = (labs(x.val.int64))}};
  } else if(x.tag == _double){
    return (sexp){.tag=_double,.val={.real64=fabs(x.val.real64)}};
  } else {return NIL;}
}
static inline sexp lisp_mod(sexp x,sexp y){
  if((x.tag==y.tag)==_long){
    return (sexp){.tag=_long,.val={.int64 = (x.val.int64 % y.val.int64)}};
  } else {
    register double xx=getDoubleVal(x);
    register double yy=getDoubleVal(y);
    return (sexp){.tag=_double,.val={.real64=fmod(xx,yy)}};
  }
}
static inline sexp ash(sexp x,sexp y){
  if(y.tag != _long || x.tag != _long){
    fprintf(stderr,"arguments to ash must be integers");
    return NIL;
  } else if(y.val.int64>=0){
    return lisp_rshift(x,y);
  } else{
    return lisp_lshift(x,(sexp){.tag=_long,.val={.int64 = (labs(y.val.int64))}});
  }
}
static inline sexp lisp_iota(sexp start,sexp stop,sexp step){
  int i;
  double dstep;
  if(NILP(stop)){
    int imax=ceil(getDoubleVal(start));
    cons* newlist=xmalloc(sizeof(cons)*imax);
    for(i=0;i<imax;i++){
      newlist[i].car.val.int64=i;
      newlist[i].cdr.val.cons=&newlist[i+1];
    }
    newlist[i].cdr=NIL;
    HERE();
    return (sexp){.tag=_cons,.val={.cons=newlist},.len=i};
  } else if(NILP(step)){
    dstep=1;
  } else {
    dstep=getDoubleVal(step);
    int imax=ceil(abs(dstep*getDoubleVal(lisp_sub(stop,start))));
    cons* newlist=xmalloc(sizeof(cons)*imax);
    double j;
    for(i=0,j=start.val.real64;i<imax;i++,j+=dstep){
      newlist[i].car.val.real64=j;
      newlist[i].cdr.val.cons=&newlist[i+1];
    }
    newlist[i].cdr=NIL;
    return (sexp){.tag=_cons,.val={.cons=newlist},.len=i};
  }
}
static inline sexp simple_iota(sexp stop){
  int i,imax;
  imax=stop.val.int64;
  cons* newlist=xmalloc(sizeof(cons)*imax);
  HERE();
  for(i=0;i<imax;i++){
    newlist[i].car=(sexp){.tag=_long,.val={.int64=i}};
    newlist[i].cdr=(sexp){.tag=_cons,.val={.cons=&newlist[i+1]}};
  }
  newlist[i].cdr=NIL;
  return (sexp){.tag=_cons,.val={.cons=newlist},.len=i};
}
static inline sexp lisp_randint(){
  return (sexp){.tag=_long,.val={.int64=mrand48()}};
}
static inline sexp lisp_randfloat(sexp scale){
  double retval;
  if(scale.tag != _nil){
    retval=drand48()*getDoubleVal(scale);
  } else {
    retval = drand48();
  }
  return (sexp){.tag=_double,.val={.real64=retval}};
}
/*static sexp funcall(sexp fn,sexp args){
  local_symbol* cur_arg = fn.val.lam->env.head;
  while(CONSP(args) && cur_arg){
  cur_arg.val = eval(car(args));
  }
  eval_in_env(fn.val.lam->body,fn.val.lam->env);
  }*/
static const sexp lisp_mach_eps = {.tag=_double,.val={.real64 = 1.41484755040568800000e-16}};
static const sexp lisp_pi = {.tag=_double,.val={.real64 = 3.14159265358979323846}};
static const sexp lisp_euler = {.tag=_double,.val={.real64 = 2.7182818284590452354}};
static const sexp lisp_max_long = {.tag = _long,.val={.int64 = LONG_MAX}};
DEFUN("+",lisp_add,2,2);
DEFUN("-",lisp_sub,2,2);
DEFUN("*",lisp_mul,2,2);
DEFUN("/",lisp_div,2,2);
DEFUN("logxor",lisp_xor,2,2);
DEFUN("logand",lisp_logand,2,2);
DEFUN("logor",lisp_logor,2,2);
DEFUN("car",car,1,1);
DEFUN("cdr",cdr,1,1);
DEFUN("caar",caar,1,1);
DEFUN("cadr",cadr,1,1);
DEFUN("cddr",cddr,1,1);
DEFUN("cdar",cdar,1,1);
DEFUN("caaar",caaar,1,1);
DEFUN("caadr",caadr,1,1);
DEFUN("caddr",caddr,1,1);
DEFUN("cdddr",cdddr,1,1);
DEFUN("cddar",cddar,1,1);
DEFUN("cdaar",cdaar,1,1);
DEFUN("cadar",cadar,1,1);
DEFUN("cdadr",cdadr,1,1);
DEFUN("cons",Cons,2,2);
DEFUN("typeName",lisp_typeName,1,1);
DEFUN("print",lisp_print,1,1);
DEFUN("reduce",reduce,2,2);
DEFUN("<",lisp_lt,2,2);
DEFUN(">",lisp_gt,2,2);
DEFUN(">=",lisp_gte,2,2);
DEFUN("<=",lisp_lte,2,2);
DEFUN("!=",lisp_ne,2,2);
DEFUN("=",lisp_equals,2,2);
DEFUN("expt",lisp_pow,2,2);
DEFUN("sqrt",lisp_sqrt,1,1);
DEFUN("cos",lisp_cos,1,1);
DEFUN("sin",lisp_sin,1,1);
DEFUN("tan",lisp_tan,1,1);
DEFUN("exp",lisp_exp,1,1);
DEFUN("log",lisp_log,1,1);
DEFUN("abs",lisp_abs,1,1);
DEFUN("ash",ash,2,2);
DEFUN("mod",lisp_mod,2,2);
DEFUN("drand",lisp_randfloat,0,1);
DEFUN("lrand",lisp_randint,0,0);
DEFUN("iota",simple_iota,1,1);
/*
  (defun SciLisp-mkIntern ()
  (interactive)
  (let ((start (point)))
  (save-excursion
  (replace-regexp-lisp ",[0-9],[0-9]);" ");\\\\")
  (goto-char start)
  (replace-regexp-lisp "DEFUN(" "DEFUN_INTERN("))))*/
#define initPrims()                                                     \
  globalSymbolTable=(global_env){.enclosing=NULL,.head=NULL};           \
  topLevelEnv=(env){.tag = 1,.enclosing=NULL,.head={.global = globalSymbolTable.head}}; \
  DEFUN_INTERN("+",lisp_add);                                           \
  DEFUN_INTERN("-",lisp_sub);                                           \
  DEFUN_INTERN("*",lisp_mul);                                           \
  DEFUN_INTERN("/",lisp_div);                                           \
  DEFUN_INTERN("logxor",lisp_xor);                                      \
  DEFUN_INTERN("logand",lisp_logand);                                   \
  DEFUN_INTERN("logor",lisp_logor);                                     \
  DEFUN_INTERN("car",car);                                              \
  DEFUN_INTERN("cdr",cdr);                                              \
  DEFUN_INTERN("<",lisp_lt);                                            \
  DEFUN_INTERN(">",lisp_gt);                                            \
  DEFUN_INTERN(">=",lisp_gte);                                          \
  DEFUN_INTERN("<=",lisp_lte);                                          \
  DEFUN_INTERN("!=",lisp_ne);                                           \
  DEFUN_INTERN("=",lisp_equals);                                        \
  DEFUN_INTERN("expt",lisp_pow);                                        \
  DEFUN_INTERN("sqrt",lisp_sqrt);                                       \
  DEFUN_INTERN("cos",lisp_cos);                                         \
  DEFUN_INTERN("sin",lisp_sin);                                         \
  DEFUN_INTERN("tan",lisp_tan);                                         \
  DEFUN_INTERN("exp",lisp_exp);                                         \
  DEFUN_INTERN("log",lisp_log);                                         \
  DEFUN_INTERN("caar",caar);                                            \
  DEFUN_INTERN("cadr",cadr);                                            \
  DEFUN_INTERN("cddr",cddr);                                            \
  DEFUN_INTERN("cdar",cdar);                                            \
  DEFUN_INTERN("caaar",caaar);                                          \
  DEFUN_INTERN("caadr",caadr);                                          \
  DEFUN_INTERN("caddr",caddr);                                          \
  DEFUN_INTERN("cdddr",cdddr);                                          \
  DEFUN_INTERN("cddar",cddar);                                          \
  DEFUN_INTERN("cdaar",cdaar);                                          \
  DEFUN_INTERN("cadar",cadar);                                          \
  DEFUN_INTERN("cdadr",cdadr);                                          \
  DEFUN_INTERN("cons",Cons);                                            \
  DEFUN_INTERN("typeName",lisp_typeName);                               \
  DEFUN_INTERN("print",lisp_print);                                     \
  DEFUN_INTERN("reduce",reduce);                                        \
  DEFUN_INTERN("abs",lisp_abs);                                         \
  DEFUN_INTERN("ash",ash);                                              \
  DEFUN_INTERN("mod",lisp_mod);                                         \
  DEFUN_INTERN("drand",lisp_randfloat);                                 \
  DEFUN_INTERN("lrand",lisp_randint);                                   \
  DEFUN_INTERN("iota",simple_iota);                                     \
  DEFCONST("Meps",lisp_mach_eps);                                       \
  DEFCONST("pi",lisp_pi);                                               \
  DEFCONST("e",lisp_euler);                                             \
  DEFCONST("nil",NIL);                                                  \
  DEFCONST("t",LISP_TRUE);                                              \
  DEFCONST("#f",LISP_FALSE);                                            \
  DEFCONST("MAX_LONG",lisp_max_long);                                   \
  DEFCONST("$$",LispEmptyList);                                         \
  srand48(time(NULL));
#endif
