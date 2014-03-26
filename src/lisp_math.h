/* Header file for generic math functions on sexp's 

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
#ifndef __LISP_MATH_H
#define __LISP_MATH_H
sexp lisp_add_driver(uint64_t numargs,sexp *values);
sexp lisp_sub_driver(uint64_t numargs,sexp *values);
sexp lisp_mul_driver(uint64_t numargs,sexp *values);
sexp lisp_div_driver(uint64_t numargs,sexp *values);
sexp lisp_pow_driver(uint64_t numargs,sexp *values);
sexp lisp_min_driver(uint64_t numargs,sexp *values);
sexp lisp_max_driver(uint64_t numargs,sexp *values);
sexp lisp_gt_driver(uint64_t numargs,sexp *values);
sexp lisp_ge_driver(uint64_t numargs,sexp *values);
sexp lisp_lt_driver(uint64_t numargs,sexp *values);
sexp lisp_le_driver(uint64_t numargs,sexp *values);
sexp lisp_eq_driver(uint64_t numargs,sexp *values);
sexp lisp_ne_driver(uint64_t numargs,sexp *values);
//not yet implemented
sexp lisp_logand_driver(uint64_t numargs,sexp *values);
sexp lisp_logior_driver(uint64_t numargs,sexp *values);
sexp lisp_logxor_driver(uint64_t numargs,sexp *values);
sexp lisp_logeqv_driver(uint64_t numargs,sexp *values);
sexp arith_driver(uint64_t numargs,sexp *values,enum operator op);

//implementaitons should be moved to seperate file
sexp lisp_init_rand(sexp seed);
sexp lisp_init_randr(sexp seed);
sexp lisp_randint(sexp sfmt,sexp un_signed);
sexp lisp_randfloat(sexp sfmt,sexp scale);

sexp ash(sexp x,sexp y);
sexp lisp_sll(sexp x, sexp y);//<<
sexp lisp_sar(sexp x, sexp y);//>>
sexp lisp_slr(sexp x, sexp y);//~>>
sexp lisp_oddp(sexp obj);
sexp lisp_evenp(sexp obj);
sexp lisp_zerop(sexp obj);
sexp lisp_numeq(sexp x,sexp y);
sexp lisp_abs(sexp x);
sexp lisp_mod(sexp x,sexp y);
sexp lisp_dec(sexp val);
sexp lisp_inc(sexp val);
sexp lisp_sqrt(sexp x);
sexp lisp_log(sexp obj);
sexp lisp_exp(sexp obj);
sexp lisp_log10(sexp obj);
sexp lisp_exp10(sexp obj);
sexp lisp_log2(sexp obj);
sexp lisp_exp2(sexp obj);
sexp lisp_cos(sexp obj);
sexp lisp_sin(sexp obj);
sexp lisp_tan(sexp obj);
sexp lisp_acos(sexp obj);
sexp lisp_asin(sexp obj);
sexp lisp_atan(sexp obj);
sexp lisp_log1p(sexp obj);
sexp lisp_expm1(sexp obj);
sexp lisp_lgamma(sexp obj);
sexp lisp_erf(sexp obj);
sexp lisp_erfc(sexp obj);
sexp lisp_j0(sexp obj);
sexp lisp_j1(sexp obj);
sexp lisp_jn(sexp obj);
sexp lisp_y0(sexp obj);
sexp lisp_y1(sexp obj);
sexp lisp_yn(sexp obj);

//in bits.c, need to add typecheking first
//sexp bit_scan_forward(sexp x);
//sexp bit_scan_reverse(sexp x);
//sexp population_count(sexp x);
//sexp bit_rotate(sexp x);

#endif
