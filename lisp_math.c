#include "common.h"
#include "bignum.h"
#include "lisp_math.h"
#include "cons.h"
sexp lisp_bigint_unsafe_min(sexp obj1, sexp obj2){
  return error_sexp("bigint unsafe min unimplemented");
}
sexp lisp_bigint_unsafe_max(sexp obj1, sexp obj2){
  return error_sexp("bigint unsafe max unimplemented");
}
sexp lisp_bigint_unsafe_pow(sexp obj1, sexp obj2){
  return error_sexp("bigint unsafe pow unimplemented");
}
sexp lisp_bigfloat_unsafe_min(sexp obj1, sexp obj2){
  return error_sexp("bigfloat unsafe min unimplemented");
}
sexp lisp_bigfloat_unsafe_max(sexp obj1, sexp obj2){
  return error_sexp("bigfloat unsafe max unimplemented");
}
sexp lisp_bigint_unsafe_div(sexp obj1, sexp obj2){
  return error_sexp("bigint unsafe div won't be implemented");
}
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
      return NIL;
  }
}
sexp lisp_abs(sexp num){
  if(!BIGNUMP(num)){
    return error_sexp("can't take the absolute value of something that's not a number");
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
      return NIL;
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
      return NIL;
    case _bigfloat:
      return NIL;
    default:
      return NIL;
  }
}
//I could write using c types which would be faster
//but it's a lot eaiser to use sexps, because generic funtions woo
//basically I'm lazy
#define op_to_fun(name,op)                                      \
  static sexp lisp_double_##name(sexp a,sexp b)                 \
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
#define op_to_fun(name,op)                              \
  static sexp lisp_long_##name(sexp a,sexp b)           \
  {return long_sexp(a.val.int64 op b.val.int64);}
op_to_fun(add,+);
op_to_fun(sub,-);
op_to_fun(mul,*);
op_to_fun(div,/);
static sexp lisp_long_max(sexp a,sexp b)
{return a.val.int64>b.val.int64?a:b;}
static sexp lisp_long_min(sexp a,sexp b)
{return a.val.int64<b.val.int64?a:b;}
sexp constOfTypeX(_tag x,long val){
  switch(x){
    case _long:
      return long_sexp(val);
    case _double:
      return double_sexp((double)val);
    case _bigint:{
      mpz_t *retval=xmalloc(sizeof(mpz_t));
      mpz_init_set_si(*retval,val);
      return bigint_sexp(retval);
    }
    case _bigfloat:{
      mpfr_t *retval=xmalloc(sizeof(mpfr_t));
      mpfr_init_set_si(*retval,val,MPFR_RNDN);
      return bigfloat_sexp(retval);
    }
    default:
      return error_sexp("non numeric type recieved");
  }
}
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
    sexp acc;
#define get_fun(binop)                          \
    switch(type){                               \
      case _long:                               \
        fp=lisp_long_##binop;break;             \
      case _double:                             \
        fp=lisp_double_##binop;break;           \
      case _bigint:                             \
        fp=lisp_bigint_unsafe_##binop;break;    \
      case _bigfloat:                           \
        fp=lisp_bigfloat_unsafe_##binop;break;  \
    }
    switch(op){
      case _add:
        get_fun(add);acc=required;break;
      case _sub:
        get_fun(sub);acc=required;break;
      case _mul:
        get_fun(mul);acc=required;break;
      case _div:
        get_fun(div);acc=required;break;
        //      case _pow:
        //        get_fun(pow);acc=required;break;
      case _max:
        get_fun(max);acc=required;break;
      case _min:
        get_fun(min);acc=required;break;
      default:
        return error_sexp("unexpected binary operator");
    }
    while(CONSP(values)){
      if(XCAR(values).tag > type){
        CORD retval;
        CORD_sprintf(&retval,"arithmatic type error, can't convert a %r to a %r",
                     tag_name(XCAR(values).tag),tag_name(type));
        return error_sexp(retval);
      }
      acc=fp(acc,XCAR(values));
      values=XCDR(values);
    }
    return acc;
  }
}
#define mk_arith_funs(opname)                                   \
  sexp lisp_##opname##_driver(sexp required,sexp values){       \
    return arith_driver(required,values,_##opname);             \
}
mk_arith_funs(add);
mk_arith_funs(sub);
mk_arith_funs(mul);
mk_arith_funs(div);
mk_arith_funs(min);
mk_arith_funs(max);
mk_arith_funs(pow);
#define mk_lisp_cmp(name,op)                                            \
  sexp lisp_cmp_##name (sexp obj1,sexp obj2){                           \
    if(!BIGNUMP(obj1) || !BIGNUMP(obj2)){                               \
      return format_type_error2("lisp_"#name,"bignum",                  \
                                obj1.tag,"bignum",obj2.tag);            \
    }                                                                   \
    sexp retval;                                                        \
    int invert=0;                                                       \
    if(obj2.tag>obj1.tag){                                              \
      sexp temp=obj1;                                                   \
      obj1=obj2;                                                        \
      obj2=temp;                                                        \
      invert=1;                                                         \
    }                                                                   \
    switch(obj1.tag){                                                   \
      case _bigfloat:                                                   \
        retval=lisp_bigfloat_##name(obj1,obj2);                         \
        break;                                                          \
      case _bigint:                                                     \
        retval=lisp_bigint_##name(obj1,obj2);                           \
        break;                                                          \
      case _double:                                                     \
        retval=(obj1.val.real64 op (getDoubleValUnsafe(obj2))           \
                ? LISP_TRUE : LISP_FALSE);                              \
        break;                                                          \
      case _long:                                                       \
        retval=(obj1.val.int64 op obj2.val.int64                        \
                ? LISP_TRUE : LISP_FALSE);                              \
        break;                                                          \
    }                                                                   \
    return (invert ? lisp_not(retval) : retval);                        \
  }
mk_lisp_cmp(gt,>);
mk_lisp_cmp(eq,==);
mk_lisp_cmp(lt,<);
mk_lisp_cmp(ge,>=);
mk_lisp_cmp(le,<=);
mk_lisp_cmp(ne,!=);
sexp lisp_mod(sexp x,sexp y){
  if((x.tag==y.tag)==_long){
    return long_sexp(x.val.int64 % y.val.int64);
  } else if(NUMBERP(x) && NUMBERP(y)){
    register double xx=getDoubleValUnsafe(x);
    register double yy=getDoubleValUnsafe(y);
    return double_sexp(fmod(xx,yy));
  } else {
    return error_sexp("Arguments to mod must be numbers");
  }
}
