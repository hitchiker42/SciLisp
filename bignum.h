#ifndef __BIGNUM_H__
#define __BIGNUM_H__
sexp lisp_bigint(sexp init);
sexp lisp_bigfloat(sexp init,sexp prec,sexp rnd);
sexp asDouble(sexp obj);
sexp asLong(sexp obj);
sexp promoteNum(sexp obj1,sexp obj2);
#endif
