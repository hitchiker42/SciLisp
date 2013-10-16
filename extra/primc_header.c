/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/*"standard library" of SciLisp as it were.
 * used to all be in prim.h, split into prim.h as declarations and
 * the initPrims macro (which actually reads all these funcitons into
 * the global symbol table)
 * this file mostly defines basic binary operations, for more list functions
 * see cons.c/h and for array functions see array.c/h*/
#include "common.h"
#include "cons.h"
#include "array.h"
#include "print.h"
#define binop_to_fun(op,fun_name)                                       \
  sexp fun_name(sexp x,sexp y){                                         \
    if((x.tag==y.tag)==_long){                                          \
      return                                                            \
        (sexp){.tag=_long,.val={.int64 = (x.val.int64 op y.val.int64)}}; \
    } else if(NUMBERP(x)&&NUMBERP(y)){                                  \
      register double xx=getDoubleVal(x);                               \
      register double yy=getDoubleVal(y);                               \
      return (sexp){.tag=_double,.val={.real64=(xx op yy)}};            \
    } else {                                                            \
      return error_sexp("type error in "#op);                           \
    }                                                                   \
  }
#define mkLisp_cmp(op,cname)                                    \
  sexp cname(sexp x,sexp y){                                    \
    if((x.tag == y.tag)==_long){                                \
      return (x.val.int64 op y.val.int64 ? LISP_TRUE : NIL);    \
    } else if(NUMBERP(x)&&NUMBERP(y)){                          \
      register double xx=getDoubleVal(x);                       \
      register double yy=getDoubleVal(y);                       \
      return (xx op yy ? LISP_TRUE : NIL);                      \
    } else {                                                    \
      return error_sexp("type error in "#op);                   \
    }                                                           \
  }    
//ignore tags, allow logical operations on doubles(or anything else)
//be careful about this
#define lop_to_fun(op,fun_name)                                         \
  sexp fun_name(sexp x,sexp y){                                         \
    return (sexp){.tag=_long,.val={.int64=(x.val.int64 op y.val.int64)}}; \
  }
#define DEFUN(lname,cname,minargs,maxargs)                      \
  fxn_proto cname##call=                                        \
    { #cname, lname, minargs, maxargs, {.f##maxargs=cname}};
#define DEFUN_MANY(lname,cname,minargs)                 \
  fxn_proto cname##call=                                \
    { #cname, lname, minargs, -1, {.fmany=cname}};
#define MK_PREDICATE(lname,test)                \
  sexp lisp_##lname (sexp obj){                 \
    if(obj.tag == test){                        \
      return LISP_TRUE;                         \
    } else {                                    \
      return LISP_FALSE;                        \
    }                                           \
  }                                             \
  DEFUN(#lname,lisp_##lname,1,1)
#define MK_PREDICATE2(lname,test,test2)         \
  sexp lisp_##lname (sexp obj){                 \
    if(obj.tag == test || obj.tag == test2){    \
      return LISP_TRUE;                         \
    } else {                                    \
      return LISP_FALSE;                        \
    }                                           \
  }                                             \
  DEFUN(#lname,lisp_##lname,1,1)

#define mkMathFun1(cname,lispname)                                      \
  sexp lispname (sexp obj){                                             \
    if(!NUMBERP(obj)){return error_sexp("type error in "#lispname);}    \
    return (sexp){.tag=_double,.val={.real64 = cname(getDoubleVal(obj))}}; \
  }
#define mkMathFun2(cname,lispname)                              \
  sexp lispname(sexp x,sexp y){                                 \
    if(!NUMBERP(x)||!NUMBERP(y))                                \
      {return error_sexp("type error in "#lispname);}           \
    register double xx=getDoubleVal(x);                         \
    register double yy=getDoubleVal(y);                         \
    return (sexp){.tag=_double,.val={.real64 = cname(xx,yy)}};  \
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
sexp lisp_abs(sexp x){
  if(x.tag==_long){return
      (sexp){.tag=_long,.val={.int64 = (labs(x.val.int64))}};
  } else if(x.tag == _double){
    return (sexp){.tag=_double,.val={.real64=fabs(x.val.real64)}};
  } else {
    return error_sexp("Argument to Abs must be a number");
  }
}
sexp lisp_mod(sexp x,sexp y){
  if((x.tag==y.tag)==_long){
    return (sexp){.tag=_long,.val={.int64 = (x.val.int64 % y.val.int64)}};
  } else if(NUMBERP(x) && NUMBERP(y)){
    register double xx=getDoubleVal(x);
    register double yy=getDoubleVal(y);
    return (sexp){.tag=_double,.val={.real64=fmod(xx,yy)}};
  } else {
    return error_sexp("Arguments to mod must be numbers");
  }
}
sexp ash(sexp x,sexp y){
  if(y.tag != _long || x.tag != _long){
    return error_sexp("arguments to ash must be integers");
  } else if(y.val.int64>=0){
    return lisp_rshift(x,y);
  } else{
    return lisp_lshift(x,(sexp){.tag=_long,.val={.int64 = (labs(y.val.int64))}});
  }
}
sexp lisp_randint(){
  return (sexp){.tag=_long,.val={.int64=mrand48()}};
}
sexp lisp_randfloat(sexp scale){
  double retval;
  if(scale.tag != _nil){
    retval=drand48()*getDoubleVal(scale);
  } else {
    retval = drand48();
  }
  return (sexp){.tag=_double,.val={.real64=retval}};
}
sexp lisp_eval(sexp obj){return eval(obj,topLevelEnv);}
sexp lisp_length(sexp obj){
  if(obj.len){
    return (sexp){.tag=_long,.val={.int64 = obj.len}};
  } else if (CONSP(obj)){
    return cons_length(obj);
  } else {
    return error_sexp("object does not have a meaningful length field");
  }
}
sexp lisp_round(sexp float_num,sexp mode){
  double double_val=getDoubleVal(float_num);
  if(double_val == NAN){
    return error_sexp("round argument is not a number");
  } else if(NILP(mode)){
    return long_sexp(lround(double_val));
  } else if(!(INTP(mode))){
        return error_sexp("rounding mode type error");
  } else {
    switch (mode.val.int64){
    //ceil,floor & trunc return doubles, there is a function
    //lrint which rounds to integers based on the current rounding mode
    //but because rounding modes are tricky we use a bit of a hack by
    //using lround to get an integer from the specified rounding function
      case -1:
        return long_sexp(lround(floor(double_val)));
      case 0:
        return long_sexp(lround(double_val));
      case 1:
        return long_sexp(lround(ceil(double_val)));
      case 2:
        return long_sexp(lround(trunc(double_val)));
      default:
        return error_sexp("round error,undefined rounding mode");
    }
  }
}
//should make this lisp_iota(a,b,c,d)
//where d is a switch to decide between a list or an array
sexp lisp_iota(sexp start,sexp stop,sexp step,sexp arrayorlist){
  if(NILP(arrayorlist)){
    return list_iota(start, stop, step);
  } else {
    return array_iota(start,stop,step);
  }
}
const sexp lisp_mach_eps = {.tag=_double,.val={.real64 = 1.41484755040568800000e-16}};
const sexp lisp_pi = {.tag=_double,.val={.real64 = 3.14159265358979323846}};
const sexp lisp_euler = {.tag=_double,.val={.real64 = 2.7182818284590452354}};
const sexp lisp_max_long = {.tag = _long,.val={.int64 = LONG_MAX}};
MK_PREDICATE2(consp,_cons,_list);
MK_PREDICATE2(numberp,_long,_double);
MK_PREDICATE(arrayp,_array);
MK_PREDICATE(nilp,_nil);