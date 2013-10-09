/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/*Refactor special forms to use enum type*/
#ifndef __COMMON_H__
#define __COMMON_H__
#include <stdio.h>
#include <stdlib.h>
//#define GC_DEBUG
#include <gc.h>
#include <ctype.h>
#include <unistd.h>
#include <stdarg.h>
#include <setjmp.h>
#include <math.h>
#include <signal.h>
/*types.h has
#include "include/cord.h"
#include <string.h>
#include <uthash.h>
#include <wchar.h>*/
#include "types.h"
//#include "lex.yy.h"
#define HERE_ON
//#define VERBOSE_LEXING
#include "debug.h"
#define my_abort(str,fmt...) fprintf(stderr,str,##fmt);abort()
#define my_err(str,fmt...) fprintf(stderr,str,##fmt);return(NIL)
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
#define double_sexp(double_val) (sexp){.tag=_double,.val={.real64=double_val}}
#define long_sexp(long_val) (sexp){.tag=_long,.val={.int64=long_val}}
#define cons_sexp(cons_val) (sexp){.tag=_cons,.val={.cons = cons_val}}
#define symVal(symref_sexp) symref_sexp.val.var->val.val
#define CORD_strdup(str) CORD_from_char_star(str)
static const sexp NIL={.tag = -1,.val={.int64 = 0}};
static const sexp UNBOUND={.tag = -2,.val={.int64 = -0xff}};
static const sexp LISP_TRUE={.tag = -2,.val={.meta = 11}};
sexp* yylval;
FILE* yyin;
static int evalError=0;
#include "print.h"
sexp lisp_print(sexp obj);
extern sexp yyparse(FILE* input);
sexp eval(sexp expr,env cur_env);
CORD error_str;
static c_string output_file="a.out";
static inline double getDoubleVal(sexp x){
  switch(x.tag){
    case _double:
      return x.val.real64;
    case _long:
      return (double)x.val.int64;
  }
}
c_string typeName(sexp obj);
#endif
