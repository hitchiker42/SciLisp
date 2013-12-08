#ifndef __LISP_MATH_H
#define __LISP_MATH_H
sexp lisp_add_driver(sexp required,sexp values);
sexp lisp_sub_driver(sexp required,sexp values);
sexp lisp_mul_driver(sexp required,sexp values);
sexp lisp_div_driver(sexp required,sexp values);
sexp lisp_pow_driver(sexp required,sexp values);
sexp lisp_min_driver(sexp required,sexp values);
sexp lisp_max_driver(sexp required,sexp values);
sexp arith_driver(sexp required,sexp values,enum operator op);
sexp lisp_abs(sexp x);
sexp lisp_mod(sexp x,sexp y);
#endif
