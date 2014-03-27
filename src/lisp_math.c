/* Generic math functions, which act on arbitary numeric types

   Copyright (C) 2013-2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/

/* NOTE: Write common functions and constants for bigfloats,
   I need an exponential function, log function and basic power function
   I also need arbitary precision versions of e and pi */
#include "common.h"
#include "frames.h"
#include "unicode.h"
#include "bignum.h"
#include "lisp_math.h"
#include "cons.h"
//#include "prim.h"
#include "SFMT/SFMT.h"
#define binop_to_fun(op,fun_name)                                       \
  sexp fun_name(sexp x,sexp y){                                         \
    if((x.tag==y.tag)==sexp_long){                                          \
      return                                                            \
        long_sexp(x.val.int64 op y.val.int64);                          \
    } else if(NUMBERP(x)&&NUMBERP(y)){                                  \
      register double xx=get_double_val(x);                               \
      register double yy=get_double_val(y);                               \
      return double_sexp(xx op yy);                                     \
    } else {                                                            \
      raise_simple_error(Etype,format_type_error2(#fun_name,"number",x.tag,"number",y.tag)); \
    }                                                                   \
  }
#define make_math_fun1(cname)                                           \
  sexp lisp_num_##cname (sexp obj){                                     \
    if(!NUMBERP(obj)){                                                  \
      raise_simple_error(Etype,format_type_error("lisp_num_" #cname,    \
                                                 "number",obj.tag));}   \
    return double_sexp(cname(get_double_val_unsafe(obj)));              \
  }
#define make_math_fun1_safe(cname)                                      \
  sexp lisp_num_##cname##_safe (sexp obj){                                      \
  if(!NUMBERP(obj)){                                                    \
    return NIL;                                                         \
  }                                                                     \
    return double_sexp(cname(get_double_val_unsafe(obj)));              \
  }
#define make_math_fun2(cname,lispname)                          \
  sexp lispname(sexp x,sexp y){                                 \
    if(!NUMBERP(x)||!NUMBERP(y)){\
      raise_simple_error(Etype,format_type_error2(#lispname,"number",   \
                                                  x.tag,"number",y.tag));\
    }                                                                   \
    register double xx=get_double_val_unsafe(x);                   \
    register double yy=get_double_val_unsafe(y);                   \
    return double_sexp(cname(xx,yy));                           \
  }
#define mkLisp_cmp(op,cname)                                    \
  sexp cname(sexp x,sexp y){                                    \
    if((x.tag == y.tag)==sexp_long){                                \
      return (x.val.int64 op y.val.int64 ? LISP_TRUE : LISP_FALSE);    \
    } else if(NUMBERP(x)&&NUMBERP(y)){                          \
      register double xx=get_double_val(x);                       \
      register double yy=get_double_val(y);                       \
      return (xx op yy ? LISP_TRUE : LISP_FALSE);                      \
    } else {                                                    \
      raise_simple_error(Etype,format_type_error2(#cname,"number",      \
                                                  x.tag,"number",y.tag)); \
    }                                                                   \
  }
//arithmatic primitives
binop_to_fun(+,lisp_add_num);
binop_to_fun(-,lisp_sub_num);
binop_to_fun(*,lisp_mul_num);
//division is special
sexp lisp_div_num(sexp x,sexp y){
  if((x.tag==y.tag)==sexp_long){
    if(y.val.int64 == 0){
      raise_simple_error(Emath,"Error, integer division by 0");
    }
    return
      long_sexp(x.val.int64 / y.val.int64);
  } else if(NUMBERP(x)&&NUMBERP(y)){
    register double yy=get_double_val(y);
    register double xx=get_double_val(x);
    return double_sexp(xx / yy);
  } else {
    raise_simple_error(Etype,format_type_error2("lisp_div_num","number",x.tag,"number",y.tag));
  }
}
//binop_to_fun(/,lisp_div_num);
//math primitives (aka wrappers around libm functions)
//log and exp 
make_math_fun1(exp);
make_math_fun1(exp10);
make_math_fun1(exp2);
make_math_fun1(log);
make_math_fun1(log10);
make_math_fun1(log2);
make_math_fun1(expm1);
make_math_fun1(log1p);
//trig functions 
make_math_fun1(cos);
make_math_fun1(sin);
make_math_fun1(tan);
make_math_fun1(acos);
make_math_fun1(asin);
make_math_fun1(atan);
make_math_fun1(cosh);
make_math_fun1(sinh);
make_math_fun1(tanh);
make_math_fun1(acosh);
make_math_fun1(asinh);
make_math_fun1(atanh);
//other functions 
make_math_fun2(pow,lisp_pow);
make_math_fun1(sqrt);
make_math_fun1(cbrt);
make_math_fun2(hypot,lisp_hypot);
//special functions
make_math_fun1(erf);
make_math_fun1(erfc);
make_math_fun1(lgamma);
make_math_fun1(tgamma);
make_math_fun1(j0);
make_math_fun1(j1);
//make_math_fun2(jn,lisp_jn);
make_math_fun1(y0);
make_math_fun1(y1);
//make_math_fun2(yn,lisp_yn);
//compairson primitives
mkLisp_cmp(>,lisp_numgt);
mkLisp_cmp(<,lisp_numlt);
mkLisp_cmp(>=,lisp_numge);
mkLisp_cmp(<=,lisp_numle);
mkLisp_cmp(!=,lisp_numne);
mkLisp_cmp(==,lisp_numeq);

sexp lisp_sincos(sexp x){
  if(!NUMBERP(x)){
    raise_simple_error(Etype,format_type_error("sincos","number",x.tag));
  }
  register double temp=get_double_val_unsafe(x);
  double *sinx,*cosx;
  sincos(temp,sinx,cosx);
  return Fcons(double_sexp(*sinx),double_sexp(*cosx));
}
//some helper functions to make translating things to lisp eaiser
//essentially translate everything to functions of one argument
struct sfmt_and_buf {
  sfmt_t sfmt;
  sfmt_buf buf;
};
typedef struct sfmt_and_buf sfmt_and_buf;
static uint32_t sfmt_and_buf_nrand32(sfmt_and_buf *sfmt){
  return sfmt_nrand32_buf(&sfmt->sfmt,&sfmt->buf);
}
static int32_t sfmt_and_buf_jrand32(sfmt_and_buf *sfmt){
  return (int32_t)sfmt_and_buf_nrand32(sfmt);
}
static uint64_t sfmt_and_buf_nrand64(sfmt_and_buf *sfmt){
  return sfmt_nrand64_buf(&sfmt->sfmt,&sfmt->buf);
}
static int64_t sfmt_and_buf_jrand64(sfmt_and_buf *sfmt){
  return (int64_t)sfmt_nrand64_buf(&sfmt->sfmt,&sfmt->buf);
}
static double sfmt_and_buf_erand64(sfmt_and_buf *sfmt){
  return sfmt_to_res53(sfmt_and_buf_nrand64(sfmt));
}
static void sfmt_and_buf_init(sfmt_and_buf *sfmt,uint32_t seed){
  if(seed){
    sfmt_init_fast_r(&sfmt->sfmt);
  } else {
    sfmt_init_explicit_r(&sfmt->sfmt,seed);
  }
  sfmt_init_buf_r(&sfmt->sfmt,&sfmt->buf);
}
static sfmt_and_buf sfmt_and_buf_static[1];
//(defun init-rand (&optional seed)
//when I add better support for unsigned/specific sized ints
//I should look at this again
sexp lisp_init_rand(sexp seed){
  sfmt_and_buf_static->sfmt=sfmt_static;
  sfmt_and_buf_static->buf=sfmt_static_buf;
  if(NILP(seed)){
    sfmt_init_fast_static();
    return NIL;
  } else if (!INTP(seed)){
    raise_simple_error(Etype,format_type_error_opt("init-rand","integer",seed.tag));
  } else {
    uint32_t seed_val=(uint32_t)seed.val.int64;
    sfmt_init_explicit_static(seed_val);
    return NIL;
  }
}
//(defun init-rand-r (&optional seed)
sexp lisp_init_rand_r(sexp seed){
  if(!NILP(seed) && !INTP(seed)){
    raise_simple_error(Etype,format_type_error_opt("init-rand","integer",seed.tag));
  }
  sfmt_and_buf *sfmt=xmalloc_atomic(sizeof(sfmt_and_buf));
  uint32_t seed_val=(NILP(seed)?0:(uint32_t)seed.val.int64);
  sfmt_and_buf_init(sfmt,seed_val);
  return opaque_sexp(sfmt);
}
//(defun rand-int (&optional state unsigned)
sexp lisp_randint(sexp sfmt,sexp un_signed){
  if(!OPAQUEP(sfmt) && !NILP(sfmt)){
    raise_simple_error(Etype,format_type_error_opt("float","random-state",sfmt.tag));
  }
  sfmt_and_buf *sfmt_val;
  if(NILP(sfmt)){
    sfmt_val=sfmt_and_buf_static;
  } else{
    sfmt_val=(sfmt_and_buf*)sfmt.val.opaque;
  }
  if(is_true(un_signed)){
    return long_sexp(sfmt_and_buf_nrand64(sfmt_val));
  } else {
    return long_sexp(sfmt_and_buf_jrand64(sfmt_val));
  }
}
//(defun rand-float (&optional state scale))
sexp lisp_randfloat(sexp sfmt,sexp scale){
  if(!OPAQUEP(sfmt) && !NILP(sfmt)){
    raise_simple_error(Etype,format_type_error_opt("rand-float","random-state",sfmt.tag));
  }
  sfmt_and_buf *sfmt_val;
  if(NILP(sfmt)){
    sfmt_val=sfmt_and_buf_static;
  } else{
    sfmt_val=(sfmt_and_buf*)sfmt.val.opaque;
  }
  if(NILP(scale)){
    return double_sexp(sfmt_and_buf_erand64(sfmt_val));
  } else if (!NUMBERP(scale)){
    raise_simple_error(Etype,format_type_error_opt("rand-float","number",scale.tag));
  } else {
    return lisp_mul_num(scale,
                        double_sexp(sfmt_and_buf_erand64(sfmt_val)));
  }
}
/*
//I wonder if it'd be faster to allocate one array
//and fill it with 2x the needed number of random numbers
//then modify it in place to get the result, being as it
//would only need 1 allocation
static sexp c_rand_array(int len,sfmt_t *sfmt,sexp_tag type){
  sexp *retval=xmalloc_atomic(sizeof(sexp)*len);
  uint32_t *sfmt_array=alloca(sizeof(uint32_t)*len);
  //needs typechecking
  sfmt_fill_array32(sfmt,sfmt_array,len*2);
  int i;
  //should compile no a noop
  uint64_t *sfmt_array64=(uint64_t*)(sfmt_array);
  switch(type){
    case sexp_int64:
      for(i=0;i<len;i++){
        retval[i]=long_sexp(sfmt_array64[i]);
      };
      return array_sexp(retval,len);
    case sexp_real64:
      for(i=0;i<len;i++){
        retval[i]=double_sexp(sfmt_to_res53(sfmt_array64[i]));
      }
      return array_len_sexp(retval,len);
  }
}
//(defun rand-array (len &optional sfmt type))
sexp rand_array_r(sexp len,sexp sfmt,sexp type){
  if(!INTP(len)){
    raise_simple_error(Etype,format_type_error("rand-array-r","integer",len.tag));
  }
  if(!OPAQUEP(sfmt) && !NILP(sfmt)){
    raise_simple_error(Etype,format_type_error_opt_named
                       ("rand-array","state","random-state",sfmt.tag));
  }
  sfmt_t *sfmt_val;
  if(NILP(sfmt)){
    sfmt_val=&(sfmt_and_buf_static->sfmt);
  } else {
    sfmt_val=&(((sfmt_and_buf*)(sfmt.val.opaque))->sfmt);
  }
  uint64_t array_len=len.val.int64;
  sexp_tag type_tag;
  if(!SYMBOLP(type) && !NILP(type)){
    raise_simple_error(Etype,format_type_error_opt("rand-array-r",
                                                   "keyword",type.tag));
  }
  if(NILP(type)){
    type_tag=sexp_long;
  } else {//type must be a keyword
    sexp type_sexp=getKeywordType(type);
    if(!TYPEP(type_sexp)){
      return type_sexp;
    }
    type_tag=type_sexp.val.type;
  }
  return c_rand_array(array_len,sfmt_val,type_tag);
}
/*sexp lisp_randint(sexp un_signed){
  if(NILP(un_signed)){
    return long_sexp(mrand48());
  } else {
    return long_sexp(lrand48());
  }
}
sexp lisp_randfloat(sexp scale){
  double retval;
  if(scale.tag != _nil){
    retval=drand48()*get_double_val(scale);
  } else {
    retval = drand48();
  }
  return double_sexp(retval);
  }*/
sexp lisp_bigint_unsafe_min(sexp obj1, sexp obj2){
  raise_simple_error(Einternal,"bigint unsafe min unimplemented");
}
sexp lisp_bigint_unsafe_max(sexp obj1, sexp obj2){
  raise_simple_error(Einternal,"bigint unsafe max unimplemented");
}
sexp lisp_bigint_unsafe_pow(sexp obj1, sexp obj2){
  raise_simple_error(Einternal,"bigint unsafe pow unimplemented");
}
sexp lisp_bigfloat_unsafe_min(sexp obj1, sexp obj2){
  raise_simple_error(Einternal,"bigfloat unsafe min unimplemented");
}
sexp lisp_bigfloat_unsafe_max(sexp obj1, sexp obj2){
  raise_simple_error(Einternal,"bigfloat unsafe max unimplemented");
}
sexp lisp_bigint_unsafe_div(sexp obj1, sexp obj2){
  raise_simple_error(Einternal,"bigint unsafe div won't be implemented");
}
sexp lisp_round(sexp float_num,sexp mode){
  double double_val=get_double_val(float_num);
  if(double_val == NAN){
    raise_simple_error(Etype,format_type_error("round","number",float_num.tag));
  } else if(NILP(mode)){
    return long_sexp(lround(double_val));
  } else if(!(SYMBOLP(mode))){
    raise_simple_error(Etype,format_type_error("round","keyword",mode.tag));
  } else {
    if(EQ(Kfloor_sexp,mode)){
      return long_sexp(lround(floor(double_val)));
    } else if (EQ(Kround_sexp,mode)){
      return long_sexp(lround(double_val));
    } else if (EQ(Kceil_sexp,mode)){
      return long_sexp(lround(ceil(double_val)));
    } else if (EQ(Ktrunc_sexp,mode)){
      return long_sexp(lround(trunc(double_val)));
    } else {
      raise_simple_error(Ekey,"Error, invalid keyword passed to round");
    }
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
        raise_simple_error(Emath,"Error ,undefined rounding mode");
    }
  }
}
//NEED to make these work with bignums 
//(defun min (num1 num2))
sexp lisp_min(sexp a,sexp b){
  if (!NUMBERP(a) || !(NUMBERP(b))){
    raise_simple_error(Etype,format_type_error2("min","number",a.tag,
                                                "number",b.tag));
  } if (a.tag == b.tag && a.tag==sexp_long){
    return (a.val.int64 > b.val.int64?a:b);
  } else {
    return (get_double_val(a) > get_double_val(b)?a:b);
  }
}
//(defun max (num1 num2))
sexp lisp_max(sexp a,sexp b){
  if (!NUMBERP(a) || !(NUMBERP(b))){
    raise_simple_error(Etype,format_type_error2("max","number",a.tag,
                                                "number",b.tag));
  } if (a.tag == b.tag && a.tag==sexp_long){
    return (a.val.int64 < b.val.int64?a:b);
  } else {
    return (get_double_val(a) < get_double_val(b)?a:b);
  }
}
//(defun zero? (number))
sexp lisp_zerop(sexp obj){
  if(!BIGNUMP(obj)){
    raise_simple_error(Etype,format_type_error("zero?","bignum",obj.tag));
  }
  return lisp_numeq(obj,long_sexp(0));
}
#define negate(num) (-num)
#define lisp_bignum_fun1a(lname,long_fun,double_fun,bigint_fun,bigfloat_fun) \
  sexp lname(sexp num){                                              \
    if(!BIGNUMP(num)){                                                  \
      raise_simple_error(Etype,format_type_error(#lname,"bignum",num.tag)); \
    }                                                                   \
    switch(num.tag){                                                    \
      case sexp_long:                                                   \
        return long_sexp(long_fun(num.val.int64));                      \
      case sexp_double:                                                 \
        return double_sexp(double_fun(num.val.real64));                 \
      case sexp_bigint:                                                 \
        return bigint_fun(num);                                         \
      case sexp_bigfloat:                                               \
        return bigfloat_fun(num);                                       \
      default:                                                          \
        return NIL;                                                     \
    }                                                                   \
  }
#define lisp_bignum_fun1b(lname,number_fun,bigint_fun,bigfloat_fun)     \
  sexp lname(sexp num){                                              \
    if(!BIGNUMP(num)){                                                  \
      raise_simple_error(Etype,format_type_error(#lname,"bignum",num.tag)); \
    }                                                                   \
    switch(num.tag){                                                    \
      case sexp_long:                                                   \
      case sexp_double:                                                 \
        return number_fun(num);                                         \
      case sexp_bigint:                                                 \
        return bigint_fun(num);                                         \
      case sexp_bigfloat:                                               \
        return bigfloat_fun(num);                                       \
      default:                                                          \
        return NIL;                                                     \
    }                                                                   \
  }                         
#define lisp_bignum_fun1c(cname)                                        \
  lisp_bignum_fun1b(lisp_##cname,lisp_num_##cname,                      \
                    lisp_bigint_##cname,lisp_bigfloat_##cname)
//c isn't very functional, we need to define these functions staticly
//and outside of any function
lisp_bignum_fun1a(lisp_neg,negate,negate,lisp_bigint_neg,lisp_bigfloat_neg)
lisp_bignum_fun1a(lisp_abs,abs,fabs,lisp_bigint_abs,lisp_bigfloat_abs)
//lisp_bignum_fun1c(sqrt)
lisp_bignum_fun1c(log)
lisp_bignum_fun1c(exp)
lisp_bignum_fun1c(log10)
lisp_bignum_fun1c(exp10)
lisp_bignum_fun1c(log2)
lisp_bignum_fun1c(exp2)
lisp_bignum_fun1c(cos);
lisp_bignum_fun1c(sin);
lisp_bignum_fun1c(tan);
lisp_bignum_fun1c(acos);
lisp_bignum_fun1c(asin);
lisp_bignum_fun1c(atan);
lisp_bignum_fun1c(log1p);
lisp_bignum_fun1c(expm1);
lisp_bignum_fun1c(lgamma);
lisp_bignum_fun1c(erf);
lisp_bignum_fun1c(erfc);
lisp_bignum_fun1c(j0);
lisp_bignum_fun1c(j1);
//lisp_bignum_fun1cp(jn);
lisp_bignum_fun1c(y0);
lisp_bignum_fun1c(y1);
//lisp_bignum_fun1c(yn);
sexp lisp_recip(sexp num){
  if(!BIGNUMP(num)){
    raise_simple_error(Etype,format_type_error("recip","bignum",num.tag));
  }
  switch(num.tag){
    case sexp_long:
      return long_sexp(1/num.val.int64);
    case sexp_double:
      return double_sexp(1/num.val.real64);
    case sexp_bigint:
      return NIL;
    case sexp_bigfloat:
      return NIL;
    default:
      return NIL;
  }
}
//I could write using c types which would be faster
//but it's a lot eaiser to use sexps
#define op_to_fun(name,op)                                      \
  static sexp lisp_double_##name(sexp a,sexp b)                 \
  {return double_sexp(a.val.real64 op get_double_val_unsafe(b));}
#define cmp_driver_fun(name,op)                                 \
  static sexp lisp_double_##name(sexp a,sexp b)                 \
  {return (a.val.real64 op get_double_val_unsafe(b)?               \
           double_sexp(get_double_val_unsafe(b)):LISP_FALSE);}
op_to_fun(add,+);
op_to_fun(sub,-);
op_to_fun(mul,*);
op_to_fun(div,/);
op_to_fun(eq_driv,==);
op_to_fun(ne_driv,!=);
op_to_fun(lt_driv,<);
op_to_fun(gt_driv,>);
op_to_fun(le_driv,<=);
op_to_fun(ge_driv,>=);
static sexp lisp_double_max(sexp a,sexp b)
{return a.val.real64>b.val.real64?a:b;}
static sexp lisp_double_min(sexp a,sexp b)
{return a.val.real64<b.val.real64?a:b;}
#undef op_to_fun
#undef cmp_driver_fun
#define op_to_fun(name,op)                              \
  static sexp lisp_long_##name(sexp a,sexp b)           \
  {return long_sexp(a.val.int64 op b.val.int64);}
#define cmp_driver_fun(name,op)                         \
  static sexp lisp_long_##name(sexp a,sexp b)           \
  {return long_sexp(a.val.int64 op b.val.int64?b:LISP_FALSE);}
op_to_fun(add,+);
op_to_fun(sub,-);
op_to_fun(mul,*);
op_to_fun(logior,|);
op_to_fun(logxor,^);
op_to_fun(logand,&);
op_to_fun(logandn,&~);
static sexp lisp_long_div(sexp a,sexp b){
  if(b.val.int64==0){
    raise_simple_error(Emath,"Error, integer division by 0");
  } else {
    return long_sexp(a.val.int64 / b.val.int64);
  }
}
op_to_fun(eq_driv,==);
op_to_fun(ne_driv,!=);
op_to_fun(lt_driv,<);
op_to_fun(gt_driv,>);
op_to_fun(le_driv,<=);
op_to_fun(ge_driv,>=);
static sexp lisp_long_max(sexp a,sexp b)
{return a.val.int64>b.val.int64?a:b;}
static sexp lisp_long_min(sexp a,sexp b)
{return a.val.int64<b.val.int64?a:b;}
#undef op_to_fun
#undef cmp_driver_fun
sexp constOfTypeX(sexp_tag x,long val){
  switch(x){
    case sexp_long:
      return long_sexp(val);
    case sexp_double:
      return double_sexp((double)val);
    case sexp_bigint:{
      mpz_t *retval=xmalloc(sizeof(mpz_t));
      mpz_init_set_si(*retval,val);
      return bigint_sexp(retval);
    }
    case sexp_bigfloat:{
      mpfr_t *retval=xmalloc(sizeof(mpfr_t));
      mpfr_init_set_si(*retval,val,MPFR_RNDN);
      return bigfloat_sexp(retval);
    }
    default:
      //I don't think this is in lisp yet,
      //but make tihs a better message before it is
      raise_simple_error(Etype,"non numeric type recieved");
  }
}
static const char *op_fun_name(enum operator op){return"";};
sexp arith_driver(uint64_t numargs,sexp *values,enum operator op){
  //lets make this a bit unsafe, the type of the result is the type of required
  //we'll prevent serious errors by type checking each argument against
  //the required type, and raising an error if it can't be promoted
  if(!numargs){
    switch(op){
      case binop_mul:
        return lisp_int64_1;
      case binop_add:
      case binop_logior:
      case binop_logxor:
        return lisp_int64_0;
      case binop_logand:
      case binop_logeqv:
        return lisp_int64_m1;
      default:
        raise_simple_error_cord(Eargs,CORD_cat("Too few arguments passed to",
                                               op_fun_name(op)));
    }
  }   
  if(!BIGNUMP(values[0])){
    //make this a better error message
    raise_simple_error(Etype,"Expected numeric argument to arithmatic function");
  }
  if(numargs==1){
    if(op == binop_sub){
      return lisp_neg(values[0]);
    } else if (op == binop_div){
      return lisp_recip(values[0]);
    } else {
      return values[0];
    }
  }
  sexp_tag type = values[0].tag;
  int cmp=0;
  sexp(*fp)(sexp,sexp);
  sexp acc;
#define get_fun(binop)                           \
  switch(type){                                  \
    case sexp_int64:                             \
      fp=lisp_long_##binop;break;                \
    case sexp_real64:                            \
      fp=lisp_double_##binop;break;              \
    case sexp_bigint:                            \
      fp=lisp_bigint_unsafe_##binop;break;       \
    case sexp_bigfloat:                          \
      fp=lisp_bigfloat_unsafe_##binop;break;     \
  }                                              \
  acc=values[0];                                 \
  break
  switch(op){
    case binop_add:
      get_fun(add);
    case binop_sub:
      get_fun(sub);
    case binop_mul:
      get_fun(mul);
    case binop_div:
      get_fun(div);
      //      case _pow:
      //        get_fun(pow);acc=required;break;
    case binop_max:
      get_fun(max);
    case binop_min:
      get_fun(min);
    case binop_lt:
      cmp=1;
      get_fun(lt_driv);
    case binop_le:
      cmp=1;
      get_fun(le_driv);
    case binop_eq:
      cmp=1;
      get_fun(eq_driv);
    case binop_ne:
      cmp=1;
      get_fun(ne_driv);
    case binop_gt:
      cmp=1;
      get_fun(gt_driv);
    case binop_ge:
      cmp=1;
      get_fun(ge_driv);
    default:
      raise_simple_error_cord(Ekey,CORD_cat("unknown key argument in",
                                            op_fun_name(op)));
  }
  int i;
  switch(cmp){
    case 0:
      for(i=1;i<numargs;i++){
        if(values[i].tag > type || values[i].tag<=1){
          raise_simple_error_fmt(Etype,
                                 "Type error in %r, can't convert a %r to a %r",
                                 op_fun_name(op),tag_name(values[i].tag),
                                 tag_name(type));
        }
        acc=fp(acc,values[i]);
      }
      return acc;
    case 1:
      for(i=1;i<numargs;i++){
        if(values[i].tag>type || values[i].tag<=1){
          raise_simple_error_fmt(Etype,
                                 "Type error in %r, can't convert a %r to a %r",
                                 op_fun_name(op),tag_name(values[i].tag),
                                 tag_name(type));
        }
        /* maybe change to
           for(i=0;i<numargs-1;i++){
           if(!fp(values[i],values[i+1]))
           return LISP_FALSE;
           }}return LISP_TRUE;*/
        acc=fp(acc,values[i]);
        if(!BIGNUMP(acc)){
          return acc;
        }
      }
      return LISP_TRUE;
  }
}
#define mk_arith_funs(opname)                                       \
  sexp lisp_##opname##_driver(uint64_t numargs,sexp *values){       \
    return arith_driver(numargs,values,binop_##opname);            \
  }
mk_arith_funs(add);
mk_arith_funs(sub);
mk_arith_funs(mul);
mk_arith_funs(div);
mk_arith_funs(min);
mk_arith_funs(max);
mk_arith_funs(pow);
mk_arith_funs(eq);
mk_arith_funs(ne);
mk_arith_funs(lt);
mk_arith_funs(le);
mk_arith_funs(gt);
mk_arith_funs(ge);
#define mk_lisp_cmps(name,op)                   \
  mk_lisp_cmp(name,op);                         \
  mk_lisp_cmp_safe(name,op);                    \
  mk_lisp_cmp_unsafe(name,op)
#define mk_lisp_cmp_safe(name,op)                                       \
  sexp lisp_cmp_##name (sexp obj1,sexp obj2){                           \
  if(!BIGNUMP(obj1) || !BIGNUMP(obj2)){                                 \
    raise_simple_error(Etype,format_type_error2("lisp_"#name,"bignum",  \
                                                obj1.tag,"bignum",obj2.tag)); \
  }                                                                     \
  return internal_lisp_cmp_##name(obj1,obj2);                           \
  }
#define mk_lisp_cmp_unsafe(name,op)                                     \
  sexp unsafe_lisp_cmp_##name(sexp obj1,sexp obj2){                     \
    return internal_lisp_cmp_##name(obj1,obj2);                         \
  }
#define mk_lisp_cmp(name,op)                                            \
  static inline sexp internal_lisp_cmp_##name (sexp obj1,sexp obj2){    \
    sexp retval;                                                        \
    int invert=0;                                                       \
    if(obj2.tag>obj1.tag){                                              \
      sexp temp=obj1;                                                   \
      obj1=obj2;                                                        \
      obj2=temp;                                                        \
      invert=1;                                                         \
    }                                                                   \
    switch(obj1.tag){                                                   \
      case sexp_bigfloat:                                                   \
        retval=lisp_bigfloat_##name(obj1,obj2);                         \
        break;                                                          \
      case sexp_bigint:                                                     \
        retval=lisp_bigint_##name(obj1,obj2);                           \
        break;                                                          \
      case sexp_double:                                                     \
        retval=(obj1.val.real64 op (get_double_val_unsafe(obj2))           \
                ? LISP_TRUE : LISP_FALSE);                              \
        break;                                                          \
      case sexp_long:                                                       \
        retval=(obj1.val.int64 op obj2.val.int64                        \
                ? LISP_TRUE : LISP_FALSE);                              \
        break;                                                          \
    }                                                                   \
    return (invert ? lisp_not(retval) : retval);                        \
  }
mk_lisp_cmps(gt,>);
mk_lisp_cmps(eq,==);
mk_lisp_cmps(lt,<);
mk_lisp_cmps(ge,>=);
mk_lisp_cmps(le,<=);
mk_lisp_cmps(ne,!=);
sexp lisp_mod(sexp x,sexp y){
  if((x.tag==y.tag)==sexp_long){
    return long_sexp(x.val.int64 % y.val.int64);
  } else if(NUMBERP(x) && NUMBERP(y)){
    register double xx=get_double_val_unsafe(x);
    register double yy=get_double_val_unsafe(y);
    return double_sexp(fmod(xx,yy));
  } else {
    raise_simple_error(Etype,format_type_error2("mod","number",
                                               x.tag,"number",y.tag));
  }
}
sexp lisp_evenp(sexp obj){
  if(!BIGNUMP(obj)){
    raise_simple_error(Etype,format_type_error("even?","bignum",obj.tag));
  } else {
    switch(obj.tag){
      case sexp_int64:
        return (obj.val.int64 &1 ? LISP_FALSE : LISP_TRUE);
      case sexp_real64:
        return (obj.val.real64 == (lisp_round(obj,NIL)).val.int64 ?
                ((lisp_round(obj,NIL)).val.int64 &1 ? LISP_FALSE :LISP_TRUE):
                LISP_FALSE);
      case sexp_bigint:
        return (mpz_tstbit(*obj.val.bigint,0) ? LISP_FALSE :LISP_TRUE);
      case sexp_bigfloat:
        return LISP_FALSE;//too lazy to do this now
      default:
        raise_simple_error(Einternal,"unimplmented numeric type passed to even?");
        
    }
  }
}
sexp lisp_oddp(sexp obj){
  if(!BIGNUMP(obj)){
    raise_simple_error(Etype,format_type_error("odd?","bignum",obj.tag));
  } else {
    switch(obj.tag){
      case sexp_int64:
        return (obj.val.int64 &1 ? LISP_TRUE : LISP_FALSE);
      case sexp_real64:
        return (obj.val.real64 == (lisp_round(obj,NIL)).val.int64 ?
                ((lisp_round(obj,NIL)).val.int64 &1 ? LISP_TRUE :LISP_FALSE):
                LISP_FALSE);
      case sexp_bigint:
        return (mpz_tstbit(*obj.val.bigint,0) ? LISP_TRUE :LISP_FALSE);
      case sexp_bigfloat:
        return LISP_FALSE;//too lazy to do this now
      default:
        raise_simple_error(Einternal,"unimplmented numeric type passed to odd?");
    }
  }
}
#define mk_lisp_cmp_select(name,op)                     \
  sexp lisp_cmp_select_##name(sexp obj1,sexp obj2){     \
  if(!BIGNUMP(obj1)){                                   \
    raise_simple_error(Etype,format_type_error(#op,"bignum",obj1.tag)); \
  }                                                     \
  if(BIGNUMP(obj2)){                                    \
    return unsafe_lisp_cmp_##name(obj1,obj2);           \
  } else if (CONSP(obj2)){                              \
    sexp args[2]={obj1,obj2};                           \
    return lisp_##name##_driver(2,args);                \
  } else if(NILP(obj2)){                                \
    return LISP_TRUE;                                   \
  } else {                                              \
    raise_simple_error(Etype,format_type_error_opt2_named       \
                       (#op,"rest","bignum","list",obj2.tag));  \
  }                                                     \
  }
mk_lisp_cmp_select(ge,>=);
mk_lisp_cmp_select(le,<=);
mk_lisp_cmp_select(gt,>);
mk_lisp_cmp_select(lt,<);
mk_lisp_cmp_select(eq,=);
mk_lisp_cmp_select(ne,!=);
//sexp bitwise_driver(sexp required,sexp rest){
sexp lisp_inc(sexp num){
  if(!NUMBERP(num)){
    raise_simple_error(Etype,format_type_error("inc","number",num.tag));
  } else {
    switch(num.tag){
      case sexp_long:
        return (sexp){.tag=num.tag,.meta=num.meta,
            .val={.int64=(++num.val.int64)}};
      case sexp_double:
        return (sexp){.tag=num.tag,.meta=num.meta,
            .val={.real64=(++num.val.real64)}};
    }
  }
}
sexp lisp_dec(sexp num){
  if(!NUMBERP(num)){
    raise_simple_error(Etype,format_type_error("dec","number",num.tag));
  } else {
    switch(num.tag){
      case sexp_long:
        num.val.int64-=1;
        return num;
      case sexp_double:
        num.val.real64-=1;
        return num;
    }
  }
}
