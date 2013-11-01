#include "common.h"
#include "bignum.h"
//c isn't very functional, we need to define these functions staticly
//and outside of any function
sexp lisp_neg(sexp num){
  if(!BIGNUMP(num)){
    return error_sexp("can't negate something that's not a number");
  }
  switch(num.tag){
    case _long:
      return long_sexp(-num.val.int64);
    case _double:
      return double_sexp(-num.val.real64);
    case _bigint:
      return lisp_bigint_neg(num);
    case _bigfloat:
      return lisp_bigfloat_neg(num);
    default:
      return nil;
  }
}
sexp lisp_abs(sexp num){
  if(!BIGNUMP(num)){
    return error_sexp("can't negate something that's not a number");
  }
  switch(num.tag){
    case _long:
      return long_sexp(abs(num.val.int64));
    case _double:
      return double_sexp(fabs(num.val.real64));
    case _bigint:
      return lisp_bigint_abs(num);
    case _bigfloat:
      return lisp_bigfloat_abs(num);
    default:
      return nil;
  }
}
sexp lisp_recip(sexp num){
  if(!BIGNUMP(num)){
    return error_sexp("can't negate something that's not a number");
  }
  switch(num.tag){
    case _long:
      return long_sexp(1/num.val.int64);
    case _double:
      return double_sexp(1/num.val.real64);
    case _bigint:
      return nil;
    case _bigfloat:
      return nil;
    default:
      return nil;
  }
}
//I could write using c types which would be faster
//but it's a lot eaiser to use sexps, because generic funtions woo
//basically I'm lazy
#define op_to_fun(name,op)                                     \
  static sexp lisp_double_##name(sexp a,sexp b)\
  {return double_sexp(a.val.real64 op getDoubleValUnsafe(b));}
op_to_fun(add,+);
op_to_fun(sub,-);
op_to_fun(mul,*);
op_to_fun(div,/);
static sexp lisp_double_max(sexp a,sexp b)
{return a.val.real64>b.val.real64?a:b;}
static sexp lisp_double_min(sexp a,sexp b)
{return a.val.real64<b.val.real64?a:b;}
#undef op_to_fun
#define op_to_fun(name,op)                                     \
  static sexp lisp_long_##name(sexp a,sexp b)\
  {return long_sexp(a.val.int64 op b.val.int64);}
op_to_fun(add,+);
op_to_fun(sub,-);
op_to_fun(mul,*);
op_to_fun(div,/);
static sexp lisp_long_max(sexp a,sexp b)
{return a.val.int64>b.val.int64?a:b;}
static sexp lisp_long_min(sexp a,sexp b)
{return a.val.int64<b.val.intl64?a:b;}
sexp arith_driver(sexp required,sexp values,enum operator op);
sexp arith_driver(sexp required,sexp values,enum operator op){
  //lets make this a bit unsafe, the type of the result is the type of required
  //we'll prevent serious errors by type checking each argument against
  //the required type, and raising an error if it can't be promoted
  if(!BIGNUMP(required)){
    return error_sexp("arithmatic functions require numeric arguments");
  }  
  _tag type = required.tag;
  sexp(*fp)(sexp,sexp);
  if(NILP(values)){
    if(op == _sub){
      return lisp_neg(required);
    } else if (op == _div){
      return lisp_recip(required);
    } else {
      return required;
    }
  } else {
#define get_fun(binop)                                 \
    switch(type){                                      \
    case _long:                                        \
      fp=lisp_long_##binop;break;                      \
    case _double:                                      \
      fp=lisp_double_##binop;break;                    \
    case _bigint:                                      \
      fp=lisp_bigint_unsafe_##binop;break;             \
    case _bigfloat:                                    \
      fp=lisp_bigfloat_unsafe_##binop;break;
    switch(op){

}
