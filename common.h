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
//#define GC_PRINT_STATS
#include <pthread.h>
#include <sched.h>
#include <sys/wait.h>
#include <dlfcn.h>
#define GC_THREADS
#include "gc/include/gc/gc.h"
#include <ctype.h>
#include <unistd.h>
#include <stdarg.h>
#include <setjmp.h>
#include <math.h>
#include <signal.h>
#include <errno.h>
/*types.h has
#include "include/cord.h"
#include <string.h>
#include <uthash.h>
#include <wchar.h>*/
#include "types.h"
#include "env.h"
#include "print.h"
#include "bignum.h"
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
#define symVal(symref_sexp) symref_sexp.val.var->val.val
#define double_sexp(double_val) (sexp){.tag=_double,.val={.real64=double_val}}
#define long_sexp(long_val) (sexp){.tag=_long,.val={.int64=long_val}}
#define cons_sexp(cons_val) (sexp){.tag=_cons,.val={.cons = cons_val}}
#define string_sexp(string_val) (sexp){.tag= _str,.val={.cord=string_val}}
#define error_sexp(error_string) (sexp){.tag= _error,.val={.cord=error_string}}
#define cord_sexp(cord_val) string_sexp(cord_val)
#define bigint_sexp(bigint_ptr) (sexp){.tag= _bigint,.val={.bigint=bigint_ptr}}
#define bigfloat_sexp(bigfloat_ptr) (sexp){.tag= _bigfloat,\
      .val={.bigfloat=bigfloat_ptr}}
#define symref_sexp(symref_val) (sexp) {.tag=_sym,.val={.var=symref_val}}
#define obarray_sexp(obarray_val) (sexp){.tag=_obarray,.val={.ob=obarray_val}}
#define function_sexp(function_val) (sexp) {.tag=_fun,.val={.fun=function_val}}
#define CORD_strdup(str) CORD_from_char_star(str)
#define CORD_append(val,ext) val=CORD_cat(val,ext)
#define CORD_cat_line(cord1,cord2) CORD_catn(3,cord1,cord2,"\n")
#define CORD_append_line(val,ext) val=CORD_cat_line(val,ext)
#define NIL_MACRO() {.tag = -1,.val={.int64 = 0}}
//lisp constants needed in c
static const sexp NIL={.tag = -1,.val={.int64 = 0}};
static const sexp UNBOUND={.tag = -2,.val={.meta = -0xf}};
static const sexp LISP_TRUE={.tag = -2,.val={.meta = 11}};
static const sexp LISP_FALSE={.tag = -3,.val={.meta = -3}};
static cons EmptyList={.car={.tag = -1,.val={.int64 = 0}},
                             .cdr={.tag = -1,.val={.int64 = 0}}};
static const sexp LispEmptyList={.tag=_cons,.val={.cons=&EmptyList}};
//global variables
sexp* yylval;
FILE* yyin;
//flag for errors at repl
extern int evalError;
//global localtion of error messages
CORD error_str;
jmp_buf error_buf;
sexp error_val;
static int initPrimsFlag=1;
//from C++ code for llvm
//sexp llvmEvalJIT(sexp expr,env cur_env);
//void initialize_llvm();
extern sexp yyparse(FILE* input);
//only function externed from eval, so I just put it here
extern sexp eval(sexp expr,env *cur_env);
extern sexp lispRead(CORD code);// __attribute__((pure));
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
static inline double getDoubleValUnsafe(sexp x){
  return (x.tag == _double ? x.val.real64 : (double)x.val.int64);
}
//CORD_to_const_char_star is this ha;
static inline char* CORD_as_cstring(CORD cord){
  if(CORD_IS_STRING(cord)){return (char*)cord;}
  else{return (char*)CORD_to_char_star(cord);}
}
extern/*C++*/ void initialize_llvm();
#define return_errno(fn_name)                   \
  int ___errsave=errno;                            \
  char* ___errmsg=strerror(errno);                 \
  CORD ___errorstr;                                                        \
  CORD_sprintf(&___errorstr,"%s failed with error number %d, %s",fn_name,___errsave,___errmsg); \
  return error_sexp(___errorstr)                                          
#endif
