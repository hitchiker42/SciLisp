#ifndef __BIGNUM_H__
#define __BIGNUM_H__
sexp lisp_bigint(sexp init);
sexp lisp_bigfloat(sexp init,sexp prec,sexp rnd);
sexp asDouble(sexp obj);
sexp asLong(sexp obj);
sexp promoteNum(sexp obj1,sexp obj2);
sexp lisp_gmp_add(sexp obj1,sexp obj2);
sexp lisp_gmp_sub(sexp obj1,sexp obj2);
sexp lisp_gmp_mul(sexp obj1,sexp obj2);
sexp lisp_gmp_mod(sexp obj1,sexp obj2);
sexp lisp_gmp_cdiv_q(sexp obj1,sexp obj2);
sexp lisp_gmp_fdiv_q(sexp obj1,sexp obj2);
sexp lisp_gmp_tdiv_q(sexp obj1,sexp obj2);
sexp lisp_gmp_cdiv_r(sexp obj1,sexp obj2);
sexp lisp_gmp_fdiv_r(sexp obj1,sexp obj2);
sexp lisp_gmp_tdiv_r(sexp obj1,sexp obj2);
sexp lisp_gmp_powm(sexp obj1,sexp obj2);
sexp lisp_gmp_and(sexp obj1,sexp obj2);
sexp lisp_gmp_ior(sexp obj1,sexp obj2);
sexp lisp_gmp_xor(sexp obj1,sexp obj2);
sexp lisp_mpfr_log(sexp obj);
sexp lisp_mpfr_exp(sexp obj);
sexp lisp_mpfr_sin(sexp obj);
sexp lisp_mpfr_cos(sexp obj);
sexp lisp_mpfr_tan(sexp obj);
sexp lisp_mpfr_add(sexp obj1,sexp obj2);
sexp lisp_mpfr_sub(sexp obj1,sexp obj2);
sexp lisp_mpfr_mul(sexp obj1,sexp obj2);
sexp lisp_mpfr_div(sexp obj1,sexp obj2);
sexp lisp_mpfr_pow(sexp obj1,sexp obj2);
#endif
