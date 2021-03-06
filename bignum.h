#ifndef __BIGNUM_H__
#define __BIGNUM_H__
sexp lisp_bigint(sexp init);
sexp lisp_bigfloat(sexp init,sexp prec,sexp rnd);
sexp asDouble(sexp obj);
sexp asLong(sexp obj);
sexp promoteNum(sexp obj1,sexp obj2);
sexp lisp_bigint_abs(sexp obj);
sexp lisp_bigint_neg(sexp obj);
sexp lisp_bigint_sqrt(sexp obj);
sexp lisp_bigfloat_log(sexp obj);
sexp lisp_bigfloat_exp(sexp obj);
sexp lisp_bigfloat_sin(sexp obj);
sexp lisp_bigfloat_cos(sexp obj);
sexp lisp_bigfloat_tan(sexp obj);
sexp lisp_bigfloat_add(sexp obj1, sexp obj2);
sexp lisp_bigfloat_sub(sexp obj1, sexp obj2);
sexp lisp_bigfloat_mul(sexp obj1, sexp obj2);
sexp lisp_bigfloat_div(sexp obj1, sexp obj2);
sexp lisp_bigfloat_pow(sexp obj1, sexp obj2);
sexp lisp_bigint_add(sexp obj1, sexp obj2);
sexp lisp_bigint_sub(sexp obj1, sexp obj2);
sexp lisp_bigint_mul(sexp obj1, sexp obj2);
sexp lisp_bigint_mod(sexp obj1, sexp obj2);
sexp lisp_bigint_cdiv_q(sexp obj1, sexp obj2);
sexp lisp_bigint_fdiv_q(sexp obj1, sexp obj2);
sexp lisp_bigint_tdiv_q(sexp obj1, sexp obj2);
sexp lisp_bigfloat_unsafe_add(sexp obj1, sexp obj2);
sexp lisp_bigfloat_unsafe_sub(sexp obj1, sexp obj2);
sexp lisp_bigfloat_unsafe_mul(sexp obj1, sexp obj2);
sexp lisp_bigfloat_unsafe_div(sexp obj1, sexp obj2);
sexp lisp_bigfloat_unsafe_pow(sexp obj1, sexp obj2);
sexp lisp_bigint_unsafe_add(sexp obj1, sexp obj2);
sexp lisp_bigint_unsafe_sub(sexp obj1, sexp obj2);
sexp lisp_bigint_unsafe_mul(sexp obj1, sexp obj2);
sexp lisp_bigint_unsafe_mod(sexp obj1, sexp obj2);
sexp lisp_bigint_unsafe_cdiv_q(sexp obj1, sexp obj2);
sexp lisp_bigint_unsafe_fdiv_q(sexp obj1, sexp obj2);
sexp lisp_bigint_unsafe_tdiv_q(sexp obj1, sexp obj2);
//these expect bigint args only
sexp lisp_bigint_cdiv_r(sexp obj1,sexp obj2);
sexp lisp_bigint_fdiv_r(sexp obj1,sexp obj2);
sexp lisp_bigint_tdiv_r(sexp obj1,sexp obj2);
sexp lisp_bigint_powm(sexp obj1,sexp obj2);
sexp lisp_bigint_and(sexp obj1,sexp obj2);
sexp lisp_bigint_ior(sexp obj1,sexp obj2);
sexp lisp_bigint_xor(sexp obj1,sexp obj2);
//
sexp lisp_bigint_eq(sexp obj1,sexp obj2);
sexp lisp_bigint_gt(sexp obj1,sexp obj2);
sexp lisp_bigint_lt(sexp obj1,sexp obj2);
sexp lisp_bigint_ne(sexp obj1,sexp obj2);
sexp lisp_bigint_ge(sexp obj1,sexp obj2);
sexp lisp_bigint_le(sexp obj1,sexp obj2);
sexp lisp_bigint_unsafe_eq_driv(sexp obj1,sexp obj2);
sexp lisp_bigint_unsafe_gt_driv(sexp obj1,sexp obj2);
sexp lisp_bigint_unsafe_lt_driv(sexp obj1,sexp obj2);
sexp lisp_bigint_unsafe_ne_driv(sexp obj1,sexp obj2);
sexp lisp_bigint_unsafe_ge_driv(sexp obj1,sexp obj2);
sexp lisp_bigint_unsafe_le_driv(sexp obj1,sexp obj2);
sexp lisp_bigfloat_eq(sexp obj1,sexp obj2);
sexp lisp_bigfloat_gt(sexp obj1,sexp obj2);
sexp lisp_bigfloat_lt(sexp obj1,sexp obj2);
sexp lisp_bigfloat_ne(sexp obj1,sexp obj2);
sexp lisp_bigfloat_ge(sexp obj1,sexp obj2);
sexp lisp_bigfloat_le(sexp obj1,sexp obj2);
sexp lisp_bigfloat_unsafe_eq_driv(sexp obj1,sexp obj2);
sexp lisp_bigfloat_unsafe_gt_driv(sexp obj1,sexp obj2);
sexp lisp_bigfloat_unsafe_lt_driv(sexp obj1,sexp obj2);
sexp lisp_bigfloat_unsafe_ne_driv(sexp obj1,sexp obj2);
sexp lisp_bigfloat_unsafe_ge_driv(sexp obj1,sexp obj2);
sexp lisp_bigfloat_unsafe_le_driv(sexp obj1,sexp obj2);
sexp lisp_bigfloat_neg(sexp obj);
sexp lisp_bigint_neg(sexp obj);
sexp lisp_bigfloat_abs(sexp obj);
sexp lisp_bigint_abs(sexp obj);
/*
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
sexp lisp_mpfr_add(sexp obj1,sexp obj2);
sexp lisp_mpfr_sub(sexp obj1,sexp obj2);
sexp lisp_mpfr_mul(sexp obj1,sexp obj2);
sexp lisp_mpfr_div(sexp obj1,sexp obj2);
sexp lisp_mpfr_pow(sexp obj1,sexp obj2);*/
#endif
