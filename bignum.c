#include "common.h"
#include "bignum.h"
//gmp functions work by side effect, so hide that fact in a macro
#define gmp_wrapper(function,args...)           \
  mpz_ptr gmp_temp=xmalloc(sizeof(mpz_t));      \
  mpz_init(*gmp_temp);                          \
  function(*gmp_temp,args);                     \
  return gmp_temp
#define mpfr_wrapper(function,args...)          \
  mprf_ptr mpfr_temp=xmalloc(sizeof(mpfr_t));   \
  function(*mpfr_temp,args,MPFR_RNDN);          \
  return mpfr_temp
sexp lisp_bigint(sexp init){
  switch(init.tag){
    case _double:{
      mpz_ptr new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_d(new_bignum,init.val.real64);
      return bigint_sexp(new_bignum);
    }
    case _long:{
      mpz_ptr new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_si(new_bignum,init.val.int64);
      return bigint_sexp(new_bignum);
    }
    case _str:{
      mpz_ptr new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set_str(new_bignum,CORD_as_cstring(init.val.cord),0);
      return bigint_sexp(new_bignum);
    }
    case _bigint:{
      mpz_ptr new_bignum =xmalloc(sizeof(mpz_t));
      mpz_init_set(new_bignum,init.val.bigint);
      return bigint_sexp(new_bignum);
    }
    case _bigfloat:{
      mpz_ptr new_bignum = xmalloc(sizeof(mpz_t));
      mpz_init(new_bignum);
      mpz_set_f(new_bignum,init.val.bigfloat);
      return bigint_sexp(new_bignum);
    }
    default:
      return error_sexp("bigint type error");
  }
}
#define init_set(suffix,value,star)                                  \
  mpfr_init_set##suffix(new_bignum,star init.val.value,MPFR_RNDN);  \
  return bigfloat_sexp(new_bignum)
sexp lisp_bigfloat(sexp init,sexp prec,sexp rnd){
  //prec & rnd are optional args, I'll add them later
  switch(init.tag){
    case _double:{
      mpfr_ptr new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(_d,real64,);
    }
    case _long:{
      mpfr_ptr new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(_si,int64,);
    }
    case _bigint:{
      mpfr_ptr new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(_z,bigint,);
    }
    case _bigfloat:{
      mpfr_ptr new_bignum=xmalloc(sizeof(mpfr_t));
      init_set(,bigfloat,);
    }
    case _str:{
      mpfr_ptr new_bignum=xmalloc(sizeof(mpfr_t));
      mpfr_init_set_str(new_bignum,CORD_as_cstring(init.val.cord),0,MPFR_RNDN);
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
#define mpfr_binop(op,size)
#define gmp_binop(op,size)
