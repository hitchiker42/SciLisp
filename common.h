/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//global includes
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
#include "print.h"
//#include "lex.yy.h"
//enable/disable debugging/lexing output
#define HERE_ON
//#define VERBOSE_LEXING
#include "debug.h"
//common macros, & memory allocation macros
#define my_abort(str,fmt...) fprintf(stderr,str,##fmt);abort()
#define my_err(str,fmt...) fprintf(stderr,str,##fmt);return(NIL)
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
#define double_sexp(double_val) (sexp){.tag=_double,.val={.real64=double_val}}
#define long_sexp(long_val) (sexp){.tag=_long,.val={.int64=long_val}}
#define cons_sexp(cons_val) (sexp){.tag=_cons,.val={.cons = cons_val}}
#define error_sexp(error_string) (sexp){.tag= _error,.val={.cord=error_string}}
#define symVal(symref_sexp) symref_sexp.val.var->val.val
#define CORD_strdup(str) CORD_from_char_star(str)
#define CORD_append(val,ext) val=CORD_cat(val,ext)
#define CORD_cat_line(cord1,cord2) CORD_catn(3,cord1,cord2,"\n")
#define CORD_append_line(val,ext) val=CORD_cat_line(val,ext)
#define NIL_MACRO() {.tag = -1,.val={.int64 = 0}}
//lisp constants needed in c
static const sexp NIL={.tag = -1,.val={.int64 = 0}};
static const sexp UNBOUND={.tag = -2,.val={.int64 = -0xff}};
static const sexp LISP_TRUE={.tag = -2,.val={.meta = 11}};
static const sexp LISP_FALSE={.tag = -3,.val={.meta = -3}};
static cons EmptyList={.car={.tag = -1,.val={.int64 = 0}},
                             .cdr={.tag = -1,.val={.int64 = 0}}};
static const sexp LispEmptyList={.tag=_cons,.val={.cons=&EmptyList}};
sexp* yylval;
FILE* yyin;
extern int evalError;
extern sexp yyparse(FILE* input);
//only function externed from eval, so I just put it here
sexp eval(sexp expr,env cur_env);
sexp lispRead(CORD code);// __attribute__((pure));
//global localtion of error messages
CORD error_str;
//jmp_buf error_buf;
static c_string output_file=NULL;
static inline double getDoubleVal(sexp x){
  switch(x.tag){
    case _double:
      return x.val.real64;
    case _long:
      return (double)x.val.int64;
    default:
      return NAN;
  }
}
extern/*C++*/ void initialize_llvm();
#endif
