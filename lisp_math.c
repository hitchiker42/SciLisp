/* NOTE: Write common functions and constants for bigfloats,
   I need an exponential function, log function and basic power function
   I also need arbitary precision versions of e and pi */
#include "common.h"
#include "bignum.h"
#include "lisp_math.h"
#include "cons.h"
#include "prim.h"
#define binop_to_fun(op,fun_name)                                       \
  sexp fun_name(sexp x,sexp y){                                         \
    if((x.tag==y.tag)==_long){                                          \
      return                                                            \
        (sexp){.tag=_long,.val={.int64 = (x.val.int64 op y.val.int64)}}; \
    } else if(NUMBERP(x)&&NUMBERP(y)){                                  \
      register double xx=getDoubleVal(x);                               \
      register double yy=getDoubleVal(y);                               \
      return double_sexp(xx op yy);                                     \
    } else {                                                            \
      return format_type_error2(#fun_name,"number",x.tag,"number",y.tag);\
    }                                                                   \
  }
//ignore tags, allow logical operations on doubles(or anything else)
//be careful about this
#define lop_to_fun(op,fun_name)                                         \
  sexp fun_name(sexp x,sexp y){                                         \
    return long_sexp(x.val.int64 op y.val.int64);                       \
  }
#define mkMathFun1(cname,lispname)                                      \
  sexp lispname (sexp obj){                                             \
    if(!NUMBERP(obj)){return format_type_error(#lispname,"number",obj.tag);} \
  return double_sexp(cname(getDoubleValUnsafe(obj)));                   \
  }
#define mkMathFun2(cname,lispname)                              \
  sexp lispname(sexp x,sexp y){                                 \
    if(!NUMBERP(x)||!NUMBERP(y))                                \
      {return format_type_error2(#lispname,"number",            \
                                 x.tag,"number",y.tag);}        \
    register double xx=getDoubleValUnsafe(x);                   \
    register double yy=getDoubleValUnsafe(y);                   \
    return double_sexp(cname(xx,yy));                           \
  }
#define mkLisp_cmp(op,cname)                                    \
  sexp cname(sexp x,sexp y){                                    \
    if((x.tag == y.tag)==_long){                                \
      return (x.val.int64 op y.val.int64 ? LISP_TRUE : LISP_FALSE);    \
    } else if(NUMBERP(x)&&NUMBERP(y)){                          \
      register double xx=getDoubleVal(x);                       \
      register double yy=getDoubleVal(y);                       \
      return (xx op yy ? LISP_TRUE : LISP_FALSE);                      \
    } else {                                                    \
      return format_type_error2(#cname,"number",x.tag,"number",y.tag); \
    }                                                           \
  }
//arithmatic primitives
binop_to_fun(+,lisp_add_num);
binop_to_fun(-,lisp_sub_num);
binop_to_fun(*,lisp_mul_num);
binop_to_fun(/,lisp_div_num);
//bitwise primitives(need to add !)
lop_to_fun(^,lisp_xor);
lop_to_fun(>>,lisp_rshift);
lop_to_fun(<<,lisp_lshift);
lop_to_fun(&,lisp_logand);
lop_to_fun(|,lisp_logor);
//math primitives
mkMathFun2(pow,lisp_pow);
mkMathFun1(sqrt,lisp_sqrt);
mkMathFun1(cos,lisp_cos);
mkMathFun1(sin,lisp_sin);
mkMathFun1(tan,lisp_tan);
mkMathFun1(exp,lisp_exp);
mkMathFun1(log,lisp_log);
//compairson primitives
mkLisp_cmp(>,lisp_numgt);
mkLisp_cmp(<,lisp_numlt);
mkLisp_cmp(>=,lisp_numge);
mkLisp_cmp(<=,lisp_numle);
mkLisp_cmp(!=,lisp_numne);
mkLisp_cmp(==,lisp_numeq);
sexp ash(sexp x,sexp y){
  if(y.tag != _long || x.tag != _long){
    return error_sexp("arguments to ash must be integers");
  } else if(y.val.int64>=0){
    return lisp_rshift(x,y);
  } else{
    return lisp_lshift(x,long_sexp(labs(y.val.int64)));
  }
}
sexp lisp_randint(sexp un_signed){
  if(NILP(un_signed)){
    return long_sexp(mrand48());
  } else {
    return long_sexp(lrand48());
  }
}
sexp lisp_randfloat(sexp scale){
  double retval;
  if(scale.tag != _nil){
    retval=drand48()*getDoubleVal(scale);
  } else {
    retval = drand48();
  }
  return double_sexp(retval);
}
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
sexp lisp_round(sexp float_num,sexp mode){
  double double_val=getDoubleVal(float_num);
  if(double_val == NAN){
    return error_sexp("round argument is not a number");
  } else if(NILP(mode)){
    return long_sexp(lround(double_val));
  } else if(!(KEYWORDP(mode))){
    return format_type_error("round","keyword",mode.tag);
  } else {
    if(KEYWORD_COMPARE(":floor",mode)){
      return long_sexp(lround(floor(double_val)));
    } else if (KEYWORD_COMPARE(":round",mode)){
      return long_sexp(lround(double_val));
    } else if (KEYWORD_COMPARE(":ceil",mode)){
      return long_sexp(lround(ceil(double_val)));
    } else if (KEYWORD_COMPARE(":trunc",mode)){
      return long_sexp(lround(trunc(double_val)));
    } else {
      return error_sexp("error, invalid keyword passed to round");
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
        return error_sexp("round error,undefined rounding mode");
    }
  }
}
sexp lisp_min(sexp a,sexp b){
  if (!NUMBERP(a) || !(NUMBERP(b))){
    return error_sexp("arguments to min must be numbers");
  } if (a.tag == b.tag && a.tag==_long){
    return (a.val.int64 > b.val.int64?a:b);
  } else {
    return (getDoubleVal(a) > getDoubleVal(b)?a:b);
  }
}
sexp lisp_max(sexp a,sexp b){
  if (!NUMBERP(a) || !(NUMBERP(b))){
    return error_sexp("arguments to max must be numbers");
  } if (a.tag == b.tag && a.tag==_long){
    return (a.val.int64 < b.val.int64?a:b);
  } else {
    return (getDoubleVal(a) < getDoubleVal(b)?a:b);
  }
}
sexp lisp_zerop(sexp obj){
  if(!BIGNUMP(obj)){
    return error_sexp("error expected an number");
  }
  return lisp_numeq(obj,long_sexp(0));
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
#define cmp_driver_fun(name,op)                                 \
  static sexp lisp_double_##name(sexp a,sexp b)                 \
  {return (a.val.real64 op getDoubleValUnsafe(b)?               \
           double_sexp(getDoubleValUnsafe(b)):LISP_FALSE);}
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
  {return (a.val.int64 op b.val.int64?b:LISP_FALSE);}
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
static sexp lisp_long_max(sexp a,sexp b)
{return a.val.int64>b.val.int64?a:b;}
static sexp lisp_long_min(sexp a,sexp b)
{return a.val.int64<b.val.int64?a:b;}
#undef op_to_fun
#undef cmp_driver_fun
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
  int cmp=0;
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
    }                                           \
    acc=required;                               \
    break
    switch(op){
      case _add:
        get_fun(add);
      case _sub:
        get_fun(sub);
      case _mul:
        get_fun(mul);
      case _div:
        get_fun(div);
        //      case _pow:
        //        get_fun(pow);acc=required;break;
      case _max:
        get_fun(max);
      case _min:
        get_fun(min);
      case _lt:
        cmp=1;
        get_fun(lt_driv);
      case _le:
        cmp=1;
        get_fun(le_driv);
      case _eq:
        cmp=1;
        get_fun(eq_driv);
      case _ne:
        cmp=1;
        get_fun(ne_driv);
      case _gt:
        cmp=1;
        get_fun(gt_driv);
      case _ge:
        cmp=1;
        get_fun(ge_driv);
      default:
        return error_sexp("unexpected binary operator");
    }
    switch(cmp){
      case 0:
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
      case 1:
        while(CONSP(values)){
          if(XCAR(values).tag > type || XCAR(values).tag<=0){
            CORD retval;
            CORD_sprintf(&retval,"arithmatic type error, can't convert a %r to a %r",
                         tag_name(XCAR(values).tag),tag_name(type));
            return error_sexp(retval);
          }
          acc=fp(acc,XCAR(values));
          values=XCDR(values);
          if(!BIGNUMP(acc)){
            return acc;
          }
        }
        return LISP_TRUE;
    }
  }
}
#define mk_arith_funs(opname)                                   \
  sexp lisp_##opname##_driver(sexp required,sexp values){       \
    return arith_driver(required,values,_##opname);             \
  }
#define mk_cmp_driv_funs(opname)                                   \
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
mk_cmp_driv_funs(eq);
mk_cmp_driv_funs(ne);
mk_cmp_driv_funs(lt);
mk_cmp_driv_funs(le);
mk_cmp_driv_funs(gt);
mk_cmp_driv_funs(ge);
#define mk_lisp_cmps(name,op)                   \
  mk_lisp_cmp(name,op);                         \
  mk_lisp_cmp_safe(name,op);                    \
  mk_lisp_cmp_unsafe(name,op)
#define mk_lisp_cmp_safe(name,op)                                       \
  sexp lisp_cmp_##name (sexp obj1,sexp obj2){                           \
  if(!BIGNUMP(obj1) || !BIGNUMP(obj2)){                                 \
    return format_type_error2("lisp_"#name,"bignum",                    \
                              obj1.tag,"bignum",obj2.tag);              \
  }                                                                     \
  return _lisp_cmp_##name(obj1,obj2);                                   \
  }
#define mk_lisp_cmp_unsafe(name,op)                                     \
  sexp unsafe_lisp_cmp_##name(sexp obj1,sexp obj2){                     \
    return _lisp_cmp_##name(obj1,obj2);                                 \
  }
#define mk_lisp_cmp(name,op)                                            \
  static inline sexp _lisp_cmp_##name (sexp obj1,sexp obj2){            \
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
mk_lisp_cmps(gt,>);
mk_lisp_cmps(eq,==);
mk_lisp_cmps(lt,<);
mk_lisp_cmps(ge,>=);
mk_lisp_cmps(le,<=);
mk_lisp_cmps(ne,!=);
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
sexp lisp_evenp(sexp obj){
  if(!BIGNUMP(obj)){
    return error_sexp("even? type error, expected a number");
  } else {
    switch(obj.tag){
      case _int64:
        return (obj.val.int64 &1 ? LISP_FALSE : LISP_TRUE);
      case _real64:
        return (obj.val.real64 == (lisp_round(obj,NIL)).val.int64 ?
                ((lisp_round(obj,NIL)).val.int64 &1 ? LISP_FALSE :LISP_TRUE):
                LISP_FALSE);
      case _bigint:
        return (mpz_tstbit(*obj.val.bigint,0) ? LISP_FALSE :LISP_TRUE);
      case _bigfloat:
        return LISP_FALSE;//too lazy to do this now
      default:
        return error_sexp("even? unimplemented numeric type");
    }
  }
}
sexp lisp_oddp(sexp obj){
  if(!BIGNUMP(obj)){
    return error_sexp("even? type error, expected a number");
  } else {
    switch(obj.tag){
      case _int64:
        return (obj.val.int64 &1 ? LISP_TRUE : LISP_FALSE);
      case _real64:
        return (obj.val.real64 == (lisp_round(obj,NIL)).val.int64 ?
                ((lisp_round(obj,NIL)).val.int64 &1 ? LISP_TRUE :LISP_FALSE):
                LISP_FALSE);
      case _bigint:
        return (mpz_tstbit(*obj.val.bigint,0) ? LISP_TRUE :LISP_FALSE);
      case _bigfloat:
        return LISP_FALSE;//too lazy to do this now
      default:
        return error_sexp("even? unimplemented numeric type");
    }
  }
}
#define mk_lisp_cmp_select(name,op)                     \
  sexp lisp_cmp_select_##name(sexp obj1,sexp obj2){     \
  if(!BIGNUMP(obj1)){                                   \
    return format_type_error(#op,"bignum",obj1.tag);    \
  }                                                     \
  if(BIGNUMP(obj2)){                                    \
    return unsafe_lisp_cmp_##name(obj1,obj2);           \
  } else if (CONSP(obj2)){                              \
    return lisp_##name##_driver(obj1,obj2);             \
  } else if(NILP(obj2)){                                \
    return LISP_TRUE;                                   \
  } else {                                              \
    return format_type_error_opt2_named                 \
      (#op,"rest","bignum","list",obj2.tag);            \
  }                                                     \
  }
mk_lisp_cmp_select(ge,>=);
mk_lisp_cmp_select(le,<=);
mk_lisp_cmp_select(gt,>);
mk_lisp_cmp_select(lt,<);
mk_lisp_cmp_select(eq,=);
mk_lisp_cmp_select(ne,!=);
