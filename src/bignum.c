#include "common.h"
#include "bignum.h"
//gmp functions work by side effect, so hide that fact in a macro
#define gmp_wrapper(function,args...)           \
  mpz_t *gmp_temp=xmalloc(sizeof(mpz_t));       \
  mpz_init(*gmp_temp);                          \
  function(*gmp_temp,args);                     \
  return bigint_sexp(gmp_temp)
#define mpfr_wrapper(function,args...)          \
  mpfr_t *mpfr_temp=xmalloc(sizeof(mpfr_t));    \
  mpfr_init(*mpfr_temp);                        \
  function(*mpfr_temp,args,MPFR_RNDN);          \
  return bigfloat_sexp(mpfr_temp)
#define gmp_mpfr_wrapper1(function,arg)         \
  mpfr_t *mpfr_temp=xmalloc(sizeof(mpfr_t));    \
  mpfr_init_set_z(*mpfr_temp,arg,MPFR_RNDN);     \
  function(*mpfr_temp,*mpfr_temp,MPFR_RNDN);    \
  return bigfloat_sexp(mpfr_temp)
#define gmp_mpfr_wrapper2(function,arg1,arg2)                   \
  mpfr_t *mpfr_temp1=xmalloc(sizeof(mpfr_t)),mpfr_temp2;        \
  mpfr_init_set_z(*mpfr_temp1,arg1,MPFR_RNDN);                  \
  mpfr_init_set_z(mpfr_temp2,arg2,MPFR_RNDN);                   \
  function(*mpfr_temp1,*mpfr_temp1,mpfr_temp2MPFR_RNDN);        \
  return bigfloat_sexp(mpfr_temp1)
sexp lisp_bigint(sexp init){
  switch(init.tag){
    case sexp_double:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_d(*new_bignum,init.val.real64);
      return bigint_sexp(new_bignum);
    }
    case sexp_long:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_si(*new_bignum,init.val.int64);
      return bigint_sexp(new_bignum);
    }
    case sexp_ulong:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_ui(*new_bignum,init.val.int64);
      return bigint_sexp(new_bignum);
    }
    case sexp_str:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_str(*new_bignum,CORD_to_const_char_star(init.val.cord),0);
      return bigint_sexp(new_bignum);
    }
    case sexp_bigint:{
      //bignum's are effectively immutable (at least from lisp)
      //so we can just return the same thing we got
      return init;
    }
    case sexp_bigfloat:{
      mpz_t *new_bignum = xmalloc(sizeof(mpz_t));
      mpz_init(*new_bignum);
      mpz_set_f(*new_bignum,*init.val.bigfloat);
      return bigint_sexp(new_bignum);
    }
    default:
      raise_simple_error(Etype,format_type_error("bigint","bignum",init.tag));
  }
}
sexp lisp_bigfloat(sexp init,sexp prec_sexp,sexp rnd_sexp){
  //prec & rnd are optional args, I'll add them later
  if(!INTP(prec_sexp) && !NILP(prec_sexp)){
    raise_simple_error
      (format_type_error_opt_named("bigfloat","prec","integer",prec_sexp.tag));
  }
  if(!KEYWORDP(rnd_sexp) && !NILP(rnd_sexp)){
    raise_simple_error
      (format_type_error_opt_named("bigfloat","rnd","keyword",rnd_sexp.tag));
  }
  mpfr_t *new_bignum=xmalloc(sizeof(mpfr_t));
  mpfr_prec_t prec;
  mpfr_rnd_t rnd;
  if(NILP(prec_sexp)){
    prec=256;
  } else {
    prec=prec_sexp.val.int64;
  }
  mpfr_init2(*new_bignum,prec);
  if(NILP(rnd_sexp)){
    rnd=MPFR_RNDN;
  } else {
    raise_simple_error(Einternal,("selectable rounding modes unimplemented"));
  }
  switch(init.tag){
    case sexp_double:{
      mpfr_set_d(*new_bignum,init.val.real64,rnd);
      return bigfloat_sexp(new_bignum);
    }
    case sexp_long:{
      mpfr_set_si(*new_bignum,init.val.int64,rnd);
      return bigfloat_sexp(new_bignum);
    }
    case sexp_ulong:{
      mpfr_set_ui(*new_bignum,init.val.uint64,rnd);
      return bigfloat_sexp(new_bignum);
    }
    case sexp_bigint:{
      mpfr_set_z(*new_bignum,*init.val.bigint,rnd);
      return bigfloat_sexp(new_bignum);
    }
    case sexp_bigfloat:{
      return init;//bigfloats are immutable
    }
    case sexp_str:{
      int status=mpfr_set_str(*new_bignum,
                              CORD_to_const_char_star(init.val.cord),
                              0,rnd);
      if(status){
        //not sure that this is the best error type for this 
        raise_simple_error(Eargs,("invalid bigfloat string passed to bigfloat"));
      } else {
        return bigfloat_sexp(new_bignum);
      }
    }
    default:
      raise_simple_error(Etype,format_type_error("bigfloat","bignum",init.tag));
  }   
}
#undef init_set
sexp asDouble(sexp obj);
sexp asLong(sexp obj);
static sexp promoteInt(sexp obj1,_tag type){
  switch(obj1.tag){
    case sexp_byte:
      obj1.val.int64=(int64_t)obj1.val.int8;
      break;
    case sexp_short:
      obj1.val.int64=(int64_t)obj1.val.int16;
      break;
    case sexp_int:
      obj1.val.int64=(int64_t)obj1.val.int32;
    case sexp_long:
      break;
    default:
      raise_simple_error(Etype,format_type_error("promote-int","Integer",obj1.tag));
  }
  obj1.tag=type;
  return obj1;
}
sexp promoteNum(sexp obj1,sexp obj2){
  if(!(BIGNUMP(obj1)) || !(BIGNUMP(obj2))){
    raise_simple_error(Etype,format_type_error2("promote","bignum",
                                                obj1.tag,"bignum",obj2.tag);
  }
  if(obj1.tag == obj2.tag){return obj2;}
  if(obj2.tag > obj1.tag){
    sexp temp;temp=obj1;obj1=obj2;obj2=temp;
  }
  switch(obj1.tag){
    case sexp_short:
      return int_n_sexp(obj2.val.int8,16);
    case sexp_int:
      return promoteInt(obj2,_int);
    case sexp_long:
      return promoteInt(obj2,_long);
    case sexp_float:
      return (sexp){.tag=_float,.val={.real32=(float)(promoteInt(obj2,_long).val.int64)}};
    case sexp_double:
      if(obj2.tag == _float){
        return double_sexp(obj2.val.real32);
      }
      return double_sexp(getDoubleValUnsafe(promoteInt(obj2,_long)));
    case sexp_bigint:
      return lisp_bigint(obj2);
    case sexp_bigfloat:
      return lisp_bigfloat(obj2,NIL,NIL);
  }
}
static jmp_buf cmp_err;
int gmp_compare_generic(sexp obj1,sexp obj2){
  if(!(BIGINTP(obj1)) || !(BIGNUMP(obj2))){
    longjmp(cmp_err,-1);
  }
  mpz_t *bigint1=obj1.val.bigint;
  switch(obj2.tag){
    case sexp_long:
      return mpz_cmp_si(*bigint1,obj2.val.int64);
    case sexp_ulong:
      return mpz_cmp_ui(*bigint1,obj2.val.uint64);
    case sexp_double:
      return mpz_cmp_d(*bigint1,obj2.val.real64);
    case sexp_bigint:
      return mpz_cmp(*bigint1,*obj2.val.bigint);
    case sexp_bigfloat:
      return mpz_cmp(*bigint1,*(lisp_bigint(obj2).val.bigint));
  }
}
int mpfr_compare_generic(sexp obj1,sexp obj2){
  if(!(BIGFLOATP(obj1)) || !(BIGNUMP(obj2))){
    longjmp(cmp_err,-1);
  }
  mpfr_t *bigfloat1=(obj1.val.bigfloat);
  switch(obj2.tag){
    case sexp_long:
      return mpfr_cmp_si(*bigfloat1,obj2.val.int64);
    case sexp_ulong:
      return mpfr_cmp_ui(*bigfloat1,obj2.val.uint64);
    case sexp_double:
      return mpfr_cmp_d(*bigfloat1,obj2.val.real64);
    case sexp_bigint:
      return mpfr_cmp_z(*bigfloat1,*obj2.val.bigint);
    case sexp_bigfloat:
      return mpfr_cmp(*bigfloat1,*obj1.val.bigfloat);
  }
}
#define lisp_bigfloat_cmp(name,op)                                  \
  sexp lisp_bigfloat_##name (sexp obj1,sexp obj2){                  \
    if(setjmp(cmp_err)){                                            \
      return format_type_error2("bigfloat-"#name,"bigfloat"         \
                                ,obj1.tag,"bignum",obj2.tag);       \
    } else if(mpfr_compare_generic(obj1,obj2) op 0){                \
      return LISP_TRUE;                                             \
    } else {                                                        \
      return LISP_FALSE;                                            \
    }                                                               \
  }
#define lisp_bigfloat_cmp_driver_fun(name,op)                       \
  sexp lisp_bigfloat_unsafe_##name (sexp obj1,sexp obj2){           \
    if(mpfr_compare_generic(obj1,obj2) op 0){                       \
      return lisp_bigfloat(obj2,NIL,NIL);                           \
    } else {                                                        \
      return LISP_FALSE;                                            \
    }                                                               \
  }
lisp_bigfloat_cmp(gt,>);
lisp_bigfloat_cmp(eq,==);
lisp_bigfloat_cmp(lt,<);
lisp_bigfloat_cmp(ge,>=);
lisp_bigfloat_cmp(le,<=);
lisp_bigfloat_cmp(ne,!=);
lisp_bigfloat_cmp_driver_fun(gt_driv,>);
lisp_bigfloat_cmp_driver_fun(eq_driv,==);
lisp_bigfloat_cmp_driver_fun(lt_driv,<);
lisp_bigfloat_cmp_driver_fun(ge_driv,>=);
lisp_bigfloat_cmp_driver_fun(le_driv,<=);
lisp_bigfloat_cmp_driver_fun(ne_driv,!=);
#define lisp_bigint_cmp(name,op)                                        \
  sexp lisp_bigint_##name (sexp obj1,sexp obj2){                        \
    if(setjmp(cmp_err)){                                                \
    return format_type_error2("bigint-"#name,"bigint"                   \
                              ,obj1.tag,"bignum",obj2.tag);             \
    } else if(gmp_compare_generic(obj1,obj2) op 0){                     \
      return LISP_TRUE;                                                 \
    } else {                                                            \
      return LISP_FALSE;                                                \
    }                                                                   \
  }
#define lisp_bigint_cmp_driver_fun(name,op)                             \
  sexp lisp_bigint_unsafe_##name (sexp obj1,sexp obj2){                 \
    if(gmp_compare_generic(obj1,obj2) op 0){                            \
      lisp_bigint(obj2);                                                \
    } else {                                                            \
      return LISP_FALSE;                                                \
    }                                                                   \
  }
lisp_bigint_cmp(gt,>);
lisp_bigint_cmp(eq,==);
lisp_bigint_cmp(lt,<);
lisp_bigint_cmp(ge,>=);
lisp_bigint_cmp(le,<=);
lisp_bigint_cmp(ne,!=);
lisp_bigint_cmp_driver_fun(gt_driv,>);
lisp_bigint_cmp_driver_fun(eq_driv,==);
lisp_bigint_cmp_driver_fun(lt_driv,<);
lisp_bigint_cmp_driver_fun(ge_driv,>=);
lisp_bigint_cmp_driver_fun(le_driv,<=);
lisp_bigint_cmp_driver_fun(ne_driv,!=);
int mpfr_pow_d(mpfr_t ROP,mpfr_t OP1,double OP2,mpfr_rnd_t RND){
  return mpfr_pow(ROP,OP1,
                  *(lisp_bigfloat(double_sexp(OP2),NIL,NIL).val.bigfloat),RND);
}
//generic functions on bigfloat,number
#define mpfr_binop(op,unsafe)                                           \
  sexp lisp_bigfloat_##unsafe##op(sexp obj1,sexp obj2){                 \
    if(!(BIGNUMP(obj1)) || !(BIGNUMP(obj2))){                           \
      raise_simple_error(Etype,format_type_error2(#op,"bignum",obj1.tag, \
                                                  "bignum",obj2.tag));  \
    }                                                                   \
    if(!(BIGFLOATP(obj1))&& BIGFLOATP(obj2)){                           \
      sexp temp_obj=obj1;                                               \
      obj1=obj2;                                                        \
      obj2=temp_obj;                                                    \
    } else {                                                            \
      obj1=lisp_bigfloat(obj1,NIL,NIL);                                 \
    }                                                                   \
    mpfr_t *bigfloat1=(obj1.val.bigfloat);                              \
    switch(obj2.tag){                                                   \
      case sexp_long:{                                                  \
        mpfr_wrapper(mpfr_##op##_si,*bigfloat1,obj2.val.int64);}        \
      case sexp_ulong:{                                                 \
        mpfr_wrapper(mpfr_##op##_ui,*bigfloat1,obj2.val.uint64);}       \
      case sexp_double:{                                                \
        mpfr_wrapper(mpfr_##op##_d,*bigfloat1,obj2.val.real64);}        \
      case sexp_bigint:{                                                \
        mpfr_wrapper(mpfr_##op##_z,*bigfloat1,*obj2.val.bigint);}       \
      case sexp_bigfloat:{                                              \
        mpfr_wrapper(mpfr_##op,*bigfloat1,*obj2.val.bigfloat);}         \
    }                                                                   \
  }
//gmp doesn't have functions on bigint,(long|double|bigfloat)unlike mpfr,its sad
#define gmp_binop(op,unsafe)                                            \
  sexp lisp_bigint_##unsafe##op(sexp obj1,sexp obj2){                   \
    if(!(BIGNUMP(obj1)) || !(BIGNUMP(obj2))){                           \
      raise_simple_error(Etypeformat_type_error2(#op,"bignum",obj1.tag, \
                                                 "bignum",obj2.tag));   \
    }                                                                   \
    if(!(BIGINTP(obj1)) && (BIGINTP(obj2))){                            \
      sexp temp_obj = obj1;                                             \
      obj1=obj2;                                                        \
      obj2=temp_obj;                                                    \
    } else {                                                            \
      obj1=lisp_bigint(obj1);                                           \
    }                                                                   \
    mpz_t *bigint1=(obj1.val.bigint);                                   \
    switch(obj2.tag){                                                   \
      case sexp_ulong:{                                                     \
        gmp_wrapper(mpz_##op##_ui,*bigint1,obj2.val.uint64);}           \
      case sexp_bigint:{                                                    \
        gmp_wrapper(mpz_##op,*bigint1,*obj2.val.bigint);}               \
      default:{                                                         \
        gmp_wrapper(mpz_##op,*bigint1,*(lisp_bigint(obj2).val.bigint));} \
    }                                                                   \
  }

gmp_binop(add,);
gmp_binop(sub,);
gmp_binop(mul,);
gmp_binop(mod,);
gmp_binop(cdiv_q,);
gmp_binop(fdiv_q,);
gmp_binop(tdiv_q,);
mpfr_binop(add,);
mpfr_binop(sub,);
mpfr_binop(mul,);
mpfr_binop(div,);
mpfr_binop(pow,);
#pragma push_macro("BIGINTP")
#pragma push_macro("BIGFLOATP")
#pragma push_macro("BIGNUMP")
#undef BIGINTP
#undef BIGFLOATP
#undef BIGNUMP
#define BIGINTP(a) 1
#define BIGFLOATP(a) 1
#define BIGNUMP(a) 1
gmp_binop(add,unsafe_);
gmp_binop(sub,unsafe_);
gmp_binop(mul,unsafe_);
gmp_binop(mod,unsafe_);
gmp_binop(cdiv_q,unsafe_);
gmp_binop(fdiv_q,unsafe_);
gmp_binop(tdiv_q,unsafe_);
mpfr_binop(add,unsafe_);
mpfr_binop(sub,unsafe_);
mpfr_binop(mul,unsafe_);
mpfr_binop(div,unsafe_);
mpfr_binop(pow,unsafe_);
#pragma pop_macro("BIGINTP")
#pragma pop_macro("BIGFLOATP")
#pragma pop_macro("BIGNUMP")
//obsolete
#define gmp_binop_mpz(op)                                               \
  sexp lisp_bigint_##op(sexp obj1,sexp obj2){                           \
    if(!(BIGINTP(obj1))||!(BIGINTP(obj2))){                             \
      return format_type_error2(#op,"bigint",obj1.tag,"bigint",obj2.tag); \
    }                                                                   \
    gmp_wrapper(mpz_##op,*obj1.val.bigint,*obj2.val.bigint);            \
  }
#define gmp_unop_mpz(op)                                                \
  sexp lisp_bigint_##op(sexp obj1){                                     \
    if(!(BIGINTP(obj1))){                                               \
      return format_type_error(#op,"bigint",obj1.tag);                  \
    }                                                                   \
    gmp_wrapper(mpz_##op,*obj1.val.bigint);                             \
  }
#define gmp_mpfr_unop(op)                                               \
  sexp lisp_bigint_##op(sexp obj1){                                     \
    if(!(BIGINTP(obj1))){                                               \
      return format_type_error(#op,"bigint",obj1.tag);                  \
    }                                                                   \
    mpfr_gmp_wrapper1(mpfr_##op,*obj1.val.bigint);                      \
  }
#define gmp_mpfr_binop(op)                                              \
  sexp lisp_bigint_##op(sexp obj1,sexp obj2){                           \
    if(!(BIGINTP(obj1))||!(BIGINTP(obj2))){                             \
      return format_type_error2(#op,"bigint",obj1.tag,"bigint",obj2.tag); \
    }                                                                   \
    gmp_wrapper(mpfr_##op,*obj1.val.bigint,*obj2.val.bigint);           \
  }
#define mpfr_binop_mpfr(op)                                             \
  sexp lisp_mpfr_##op(sexp obj1,sexp obj2){                             \
    if(!(BIGFLOATP(obj1))||!(BIGFLOATP(obj2))){                         \
      return format_type_error2(#op,"bigfloat",obj1.tag,"bigfloat",obj2.tag); \
    }                                                                   \
    mpfr_wrapper(mpfr_##op,*obj1.val.bigfloat,*obj2.val.bigfloat);      \
  }
#define mpfr_unop_mpfr(op)                                              \
  sexp lisp_bigfloat_##op(sexp obj1){                                   \
    if(!(BIGFLOATP(obj1))){                                             \
      return format_type_error(#op,"bigfloat",obj1.tag);               \
    }                                                                   \
    mpfr_wrapper(mpfr_##op,*obj1.val.bigfloat);                         \
  }
/*gmp_binop_mpz(add);
  gmp_binop_mpz(sub);
  gmp_binop_mpz(mul);
  gmp_binop_mpz(mod);
  gmp_binop_mpz(cdiv_q);
  gmp_binop_mpz(fdiv_q);
  gmp_binop_mpz(tdiv_q);*/
gmp_binop_mpz(cdiv_r);
gmp_binop_mpz(fdiv_r);
gmp_binop_mpz(tdiv_r);
//gmp_binop_mpz(powm);
gmp_binop_mpz(and);
gmp_binop_mpz(ior);
gmp_binop_mpz(xor);
gmp_unop_mpz(neg);
gmp_unop_mpz(abs);
gmp_unop_mpz(sqrt);

gmp_mpfr_binop(div);
gmp_mpfr_binop(pow);
gmp_mpfr_binop(hypot);

gmp_mpfr_unop(log);
gmp_mpfr_unop(exp);
gmp_mpfr_unop(log10);
gmp_mpfr_unop(exp10);
gmp_mpfr_unop(log2);
gmp_mpfr_unop(exp2);
gmp_mpfr_unop(cos);
gmp_mpfr_unop(sin);
gmp_mpfr_unop(tan);
gmp_mpfr_unop(acos);
gmp_mpfr_unop(asin);
gmp_mpfr_unop(atan);
gmp_mpfr_unop(abs);
gmp_mpfr_unop(neg);
gmp_mpfr_unop(log1p);
gmp_mpfr_unop(expm1);
gmp_mpfr_unop(gamma);
gmp_mpfr_unop(lgamma);
gmp_mpfr_unop(erf);
gmp_mpfr_unop(erfc);
gmp_mpfr_unop(j0);
gmp_mpfr_unop(j1);
gmp_mpfr_unop(jn);
gmp_mpfr_unop(y0);
gmp_mpfr_unop(y1);
gmp_mpfr_unop(yn);


mpfr_binop_mpfr(add);
mpfr_binop_mpfr(sub);
mpfr_binop_mpfr(mul);
mpfr_binop_mpfr(div);
mpfr_binop_mpfr(pow);
mpfr_binop_mpfr(hypot);

mpfr_unop_mpfr(sqrt);

mpfr_unop_mpfr(log);
mpfr_unop_mpfr(exp);
mpfr_unop_mpfr(log10);
mpfr_unop_mpfr(exp10);
mpfr_unop_mpfr(log2);
mpfr_unop_mpfr(exp2);
mpfr_unop_mpfr(cos);
mpfr_unop_mpfr(sin);
mpfr_unop_mpfr(tan);
mpfr_unop_mpfr(acos);
mpfr_unop_mpfr(asin);
mpfr_unop_mpfr(atan);
mpfr_unop_mpfr(abs);
mpfr_unop_mpfr(neg);
mpfr_unop_mpfr(log1p);
mpfr_unop_mpfr(expm1);
mpfr_unop_mpfr(gamma);
mpfr_unop_mpfr(lgamma);
mpfr_unop_mpfr(erf);
mpfr_unop_mpfr(erfc);
mpfr_unop_mpfr(j0);
mpfr_unop_mpfr(j1);
mpfr_unop_mpfr(jn);
mpfr_unop_mpfr(y0);
mpfr_unop_mpfr(y1);
mpfr_unop_mpfr(yn);
