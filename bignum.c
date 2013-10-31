#include "common.h"
#include "bignum.h"
//gmp functions work by side effect, so hide that fact in a macro
#define gmp_wrapper(function,args...)           \
  mpz_t *gmp_temp=xmalloc(sizeof(mpz_t));      \
  mpz_init(*gmp_temp);                          \
  function(*gmp_temp,args);                     \
  return bigint_sexp(gmp_temp)
#define mpfr_wrapper(function,args...)          \
  mpfr_t *mpfr_temp=xmalloc(sizeof(mpfr_t));    \
  mpfr_init(*mpfr_temp);                        \
  function(*mpfr_temp,args,MPFR_RNDN);          \
  return bigfloat_sexp(mpfr_temp)
sexp lisp_bigint(sexp init){
  switch(init.tag){
    case _double:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_d(*new_bignum,init.val.real64);
      return bigint_sexp(new_bignum);
    }
    case _long:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_si(*new_bignum,init.val.int64);
      return bigint_sexp(new_bignum);
    }
    case _str:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_str(*new_bignum,CORD_as_cstring(init.val.cord),0);
      return bigint_sexp(new_bignum);
    }
    case _bigint:{
      mpz_t *new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set(*new_bignum,*init.val.bigint);
      return bigint_sexp(new_bignum);
    }
    case _bigfloat:{
      mpz_t *new_bignum = xmalloc(sizeof(mpz_t));
      mpz_init(*new_bignum);
      mpz_set_f(*new_bignum,*init.val.bigfloat);
      return bigint_sexp(new_bignum);
    }
    default:
      return error_sexp("bigint type error");
  }
}
#define init_set(suffix,value,star)                                  \
  mpfr_init_set##suffix(*new_bignum,star init.val.value,MPFR_RNDN);  \
  return bigfloat_sexp(new_bignum)
sexp lisp_bigfloat(sexp init,sexp prec,sexp rnd){
  //prec & rnd are optional args, I'll add them later
  switch(init.tag){
    case _double:{
      mpfr_t *new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(_d,real64,);
    }
    case _long:{
      mpfr_t *new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(_si,int64,);
    }
    case _bigint:{
      mpfr_t *new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(_z,bigint,*);
    }
    case _bigfloat:{
      mpfr_t *new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(,bigfloat,*);
    }
    case _str:{
      mpfr_t *new_bignum=xmalloc(sizeof(mpfr_t));
      mpfr_init_set_str(*new_bignum,CORD_as_cstring(init.val.cord),0,MPFR_RNDN);
      return bigfloat_sexp(new_bignum);
    }
    default:
      return error_sexp("bigfloat type error");
  }
}
#undef init_set
sexp asDouble(sexp obj);
sexp asLong(sexp obj);
sexp promoteNum(sexp obj1,sexp obj2){
  if(!(BIGNUMP(obj1)) || !(BIGNUMP(obj2))){
    return error_sexp("type error expected a number");
  }
  if(obj2.tag > obj1.tag){
    sexp temp;temp=obj1;obj1=obj2;obj2=temp;
  }
  switch(obj1.tag){
    case _byte:
      return obj2;
    case _short:
    case _int:
    case _long:
    case _float:
    case _double:
      return double_sexp(getDoubleVal(obj2));
    case _bigint:
      return lisp_bigint(obj2);
    case _bigfloat:
      return lisp_bigfloat(obj2,NIL,NIL);
  }
}
static jmp_buf cmp_err;
int gmp_compare_generic(sexp obj1,sexp obj2){
    if(!(BIGINTP(obj1)) || !(BIGNUMB(obj2))){
      longjmp(cmp_err,-1);
    }
    mpz_t bigint1=obj1.val.bigint;
    switch(obj2.tag){
      case _long:
        return mpz_cmp_si(bigint1,obj2.val.int64);
      case _ulong:
        return mpz_cmp_ui(bigint1,obj2.val.uint64);
      case _double:
        return mpz_cmp_d(bigint1,obj2.val.real64);
      case _bigint:
        return mpz_cmpi(bigint1,obj2.val.bigint);
      case _bigfloat:
        return mpz_cmp(bigint1,bigint(obj2));
    }
}
int mpfr_compare_generic(sexp obj1,sexp obj2){
    if(!(BIGFLOATP(obj1)) || !(BIGNUMB(obj2))){
      longjmp(cmp_err,-1);
    }
    mpfr_t bigfloat1=obj1.val.bigfloat;
    switch(obj2.tag){
      case _long:
        return mpfr_cmp_si(bigfloat1,obj2.val.int64);
      case _ulong:
        return mpfr_cmp_ui(bigfloat1,obj2.val.uint64);
      case _double:
        return mpfr_cmp_d(bigfloat1,obj2.val.real64);
      case _bigint:
        return mpfr_cmp_z(bigfloat1,obj2.val.bigint);
      case _bigfloat:
        return mpfr_cmp(bigfloat1,obj1.val.bigfloat);
    }
}
#define lisp_bigfloat_cmp(name,op)                                \
  sexp lisp_bigfloat_##name (sexp obj1,sexp obj2){                \
    if(setjmp(cmp_err)){                                          \
      if(mpfr_compare_generic(obj1,obj2) op 0){                   \
        return LISP_TRUE;                                         \
      } else {                                                    \
        return LISP_FALSE;                                        \
      }                                                           \
    }                                                             \
    return error_sexp("bigfloat compare " #name " type error");   \
  }
lisp_bigfloat_cmp(gt,>);
lisp_bigfloat_cmp(eq,==);
lisp_bigfloat_cmp(lt,<);
lisp_bigfloat_cmp(ge,>=);
lisp_bigfloat_cmp(le,<=);
lisp_bigfloat_cmp(ne,!=);
#define lisp_bigint_cmp(name,op)                                \
  sexp lisp_bigint_##name (sexp obj1,sexp obj2){                \
    if(setjmp(cmp_err)){                                        \
      if(gmp_compare_generic(obj1,obj2) op 0){                  \
        return LISP_TRUE;                                       \
      } else {                                                  \
        return LISP_FALSE;                                      \
      }                                                         \
    }                                                           \
    return error_sexp("bigint compare " #name " type error");   \
  }
lisp_bigint_cmp(gt,>);
lisp_bigint_cmp(eq,==);
lisp_bigint_cmp(lt,<);
lisp_bigint_cmp(ge,>=);
lisp_bigint_cmp(le,<=);
lisp_bigint_cmp(ne,!=);
//generic functions on bigfloat,number
#define mpfr_binop(op)                                                  \
  sexp lisp_bigfloat_op(sexp obj1,sexp obj2){                           \
    if(!(BIGFLOATP(obj1)) || !(BIGNUMB(obj2))){                         \
      return error_sexp                                                 \
        ("bigfloat arithmatic funcitons require numeric arguments");    \
    }                                                                   \
    mpfr_t bigfloat1=obj1.val.bigfloat;                                 \
    switch(obj2.tag){                                                   \
      case _long:                                                       \
        return bigfloat_sexp                                            \
          (mpfr_wrapper(mprf_##op##_si(bigfoat1,obj2.val.int64)));      \
      case _ulong:                                                      \
        return bigfloat_sexp                                            \
          (mpfr_wrapper(mprf_##op##_ui(bigfoat1,obj2.val.uint64)));     \
      case _double:                                                     \
        return bigfloat_sexp                                            \
          (mpfr_wrapper(mpfr_##op##_d(bigfloat1,obj2.val.real64)));     \
      case _bigint:                                                     \
        return bigfloat_sexp                                            \
          (mpfr_wrapper(mpfr_##op##_z(bigfloat1,obj2.val.bigint)));     \
      case _bigfloat:                                                   \
        return bigfloat_sexp                                            \
          (mpfr_wrapper(mpfr_##op(bigfloat1,obj2.val.bigfloat)));       \
    }                                                                   \
  }
//gmp doesn't have functions on bigint,(long|double|bigfloat)unlike mpfr,its sad
#define gmp_binop(op)                                                   \
  sexp lisp_bigint_op(sexp obj1,sexp obj2){                             \
    if(!(BIGINTP(obj1)) || !(BIGNUMB(obj2))){                           \
      return error_sexp                                                 \
        ("bigint arithmatic funcitons require numeric arguments");      \
    }                                                                   \
    mpz_t bigint1=obj1.val.bigint;                                      \
    switch(obj2.tag){                                                   \
      case _ulong:                                                      \
        return bigint_sexp                                              \
          (gmp_wrapper(gmp_##op##_ui(bigint1,obj2.val.uint64)));        \
      case _bigint:                                                     \
        return bigint_sexp                                              \
          (gmp_wrapper(gmp_##op(bigint1,obj2.val.bigint)));             \
      default:                                                          \
        return bigint_sexp                                              \
        (gmp_wrapper(gmp_##op(bigint1,bigint(obj2).val.bigint)))        \
    }                                                                   \
  }

gmp_binop(add);
gmp_binop(sub);
gmp_binop(mul);
gmp_binop(mod);
gmp_binop(cdiv_q);
gmp_binop(fdiv_q);
gmp_binop(tdiv_q);
mpfr_binop(add);
mpfr_binop(sub);
mpfr_binop(mul);
mpfr_binop(div);
mpfr_binop(pow);

#define gmp_binop_mpz(op)                                               \
  sexp lisp_gmp_##op(sexp obj1,sexp obj2){                              \
    if(!(BIGINTP(obj1))||!(BIGINTP(obj2))){                             \
      return error_sexp("bigint-op functions require bigint operands"); \
    }                                                                   \
    gmp_wrapper(mpz_##op,*obj1.val.bigint,*obj2.val.bigint);            \
  }
#define mpfr_binop_mpfr(op)                                               \
  sexp lisp_mpfr_##op(sexp obj1,sexp obj2){                              \
    if(!(BIGFLOATP(obj1))||!(BIGFLOATP(obj2))){                             \
      return error_sexp("bigfloat-op functions require bigfloat operands"); \
    }                                                                   \
    mpfr_wrapper(mpfr_##op,*obj1.val.bigfloat,*obj2.val.bigfloat);      \
  }
#define mpfr_unop_mpfr(op)                                             \
  sexp lisp_mpfr_##op(sexp obj1){                                       \
    if(!(BIGFLOATP(obj1))){                                             \
      return error_sexp("bigfloat-op functions require bigfloat operands"); \
    }                                                                   \
    mpfr_wrapper(mpfr_##op,*obj1.val.bigfloat);                         \
  }
gmp_binop_mpz(add);
gmp_binop_mpz(sub);
gmp_binop_mpz(mul);
gmp_binop_mpz(mod);
gmp_binop_mpz(cdiv_q);
gmp_binop_mpz(fdiv_q);
gmp_binop_mpz(tdiv_q);
gmp_binop_mpz(cdiv_r);
gmp_binop_mpz(fdiv_r);
gmp_binop_mpz(tdiv_r);
//gmp_binop_mpz(powm);
gmp_binop_mpz(and);
gmp_binop_mpz(ior);
gmp_binop_mpz(xor);
mpfr_binop_mpfr(add);
mpfr_binop_mpfr(sub);
mpfr_binop_mpfr(mul);
mpfr_binop_mpfr(div);
mpfr_binop_mpfr(pow);
mpfr_unop_mpfr(log);
mpfr_unop_mpfr(exp);
mpfr_unop_mpfr(cos);
mpfr_unop_mpfr(sin);
mpfr_unop_mpfr(tan);
