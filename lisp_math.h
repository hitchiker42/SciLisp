#ifndef __LISP_MATH_H
#define __LISP_MATH_H
sexp lisp_add_driver(sexp required,sexp values);
sexp lisp_sub_driver(sexp required,sexp values);
sexp lisp_mul_driver(sexp required,sexp values);
sexp lisp_div_driver(sexp required,sexp values);
sexp lisp_pow_driver(sexp required,sexp values);
sexp lisp_min_driver(sexp required,sexp values);
sexp lisp_max_driver(sexp required,sexp values);
sexp lisp_gt_driver(sexp required,sexp values);
sexp lisp_ge_driver(sexp required,sexp values);
sexp lisp_lt_driver(sexp required,sexp values);
sexp lisp_le_driver(sexp required,sexp values);
sexp lisp_eq_driver(sexp required,sexp values);
sexp lisp_ne_driver(sexp required,sexp values);
sexp arith_driver(sexp required,sexp values,enum operator op);
sexp lisp_abs(sexp x);
sexp lisp_mod(sexp x,sexp y);
sexp lisp_dec(sexp val);
sexp lisp_inc(sexp val);
#endif
