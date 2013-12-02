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
#include <sched.h>
#include <sys/wait.h>
#define GC_THREADS
#define GC_PTHREADS
#include "gc/include/gc/gc.h"
#include <pthread.h>
#include <ctype.h>
#include <unistd.h>
#include <stdarg.h>
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
#include "cffi.h"
#include "llvm_externs.h"
//#include "lex.yy.h"
//enable/disable debugging/lexing output
#define HERE_ON
//enable/disable SciLisp multithreading (gc is always multithreaded)
//#define MULTI_THREADED
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
//type_sexp macros for convience (kinda like constructors I suppose)
#define typed_array_sexp(array_val,array_tag,array_len)              \
  (sexp){.tag=_typed_array,.meta=array_tag,.len=array_len,           \
      .val={.typed_array=array_val},.is_ptr=1}
#define bigfloat_sexp(bigfloat_ptr) (sexp){.tag= _bigfloat,\
      .val={.bigfloat=bigfloat_ptr},.is_ptr=1}
#define bigint_sexp(bigint_ptr) (sexp){.tag= _bigint,.val={.bigint=bigint_ptr},.is_ptr=1}
#define cons_sexp(cons_val) (sexp){.tag=_cons,.val={.cons = cons_val},.is_ptr=1}
#define cord_sexp(cord_val) string_sexp(cord_val)
#define c_data_sexp(c_data_val) (sexp){.tag=_cdata,.val={.c_val=c_data_val}}
#define double_sexp(double_val) (sexp){.tag=_double,.val={.real64=double_val}}
#define env_sexp(env_val) (sexp) {.tag=_env,.val={.cur_env=env_val},.is_ptr=1}
#define error_sexp(error_string) (sexp){.tag= _error,.val={.cord=error_string}}
#define float_sexp(float_val) (sexp){.tag=_float,.val={.real32=float_val}}
#define funargs_sexp(funargs_val) (sexp) {.tag=_funargs,.val={.funargs=funargs_val},.is_ptr=1}
#define function_sexp(function_val) (sexp) {.tag=_fun,.val={.fun=function_val},.is_ptr=1}
#define hashTable_sexp(hashtable_val) (sexp){.tag=_hashtable,\
      .val={.hashtable=hashtable_val},.is_ptr=1}
#define int_n_sexp(int_n_val,n) (sexp) {.tag=_int##n,\
      .val={.int##n=int_n_val}}
#define uint_n_sexp(uint_n_val,n) (sexp) {.tag=_uint##n,\
      .val={.uint##n=uint_n_val}}
#define keyword_sexp(keyword_val) (sexp){.tag=_keyword,\
      .val={.keyword=keyword_val}}
#define list_sexp(list_val) (sexp){.tag=_list,.val={.cons = list_val},.is_ptr=1}
#define list_len_sexp(list_val,_len) (sexp){.tag=_list,.val={.cons = list_val},\
      .is_ptr=1,.len=_len}
#define long_sexp(long_val) (sexp){.tag=_long,.val={.int64=long_val}}
#define ulong_sexp(ulong_val) (sexp){.tag=_ulong,.val={.uint64=ulong_val}}
#define macro_sexp(macro_val) (sexp) {.tag = _macro,.val={.mac=macro_val},.is_ptr=1}
#define meta_sexp(meta_val) (sexp) {.tag =_meta,.val={.meta=meta_val}}
#define obarray_sexp(obarray_val) (sexp){.tag=_obarray,.val={.ob=obarray_val},.is_ptr=1}
#define opaque_sexp(opaque_val) (sexp){.tag=_opaque,.val={.opaque=opaque_val},.is_ptr=1}
#define re_match_sexp(re_match_val) (sexp){.tag=_re_data,\
      .val={.re_data=re_match_val},.is_ptr=1}
#define array_sexp(sarray_val,array_len)                        \
  (sexp){.tag=_array,.len=array_len,.val={.array=sarray_val},.is_ptr=1}
#define spec_sexp(spec_tag) (sexp) {.tag = _special,.val={.special=spec_tag}}
#define stream_sexp(stream_val) (sexp){.tag=_env,.val={.stream=stream_val},.is_ptr=1}
#define string_sexp(string_val) (sexp){.tag= _str,.val={.cord=string_val},.is_ptr=1}
#define symref_sexp(symref_val) (sexp) {.tag=_sym,.val={.var=symref_val},.is_ptr=1}
#define tree_sexp(tree_val) (sexp){.tag=_tree,.val={.tree=tree_val},.is_ptr=1}
#define CORD_strdup(str) CORD_from_char_star(str)
#define CORD_append(val,ext) val=CORD_cat(val,ext)
#define CORD_cat_line(cord1,cord2) CORD_catn(3,cord1,cord2,"\n")
#define CORD_append_line(val,ext) val=CORD_cat_line(val,ext)
#define NIL_MACRO() {.tag = -1,.val={.meta = -1}}
#define format_error_str(format,args...) CORD_sprintf(&error_str,format,##args)
#define format_error_sexp(format,args...)            \
  format_error_str(format,args),                     \
    error_sexp(CORD_to_char_star(error_str))
//lisp constants needed in c
static const sexp NIL={.tag = -1,.val={.meta = -1},.len=0};
static const sexp UNBOUND={.tag = -2,.val={.meta = -0xf}};
static const sexp LISP_TRUE={.tag = -2,.val={.meta = 11}};
static const sexp LISP_FALSE={.tag = -3,.val={.meta = -3}};
static cons EmptyList={.car={.tag = -1,.val={.meta = -1}},
                             .cdr={.tag = -1,.val={.meta = -1}}};
static const sexp LispEmptyList={.tag=_cons,.val={.cons=&EmptyList}};
static uint64_t global_gensym_counter=0;
//global variables
sexp* yylval;
FILE* yyin;
//flag for errors at repl
extern int evalError;
//global localtion of error messages
CORD error_str;
CORD type_error_str;
jmp_buf error_buf;
sexp error_val;
static int initPrimsFlag=1;
//functions to print (or not print) debug info
void (*debug_printf)(const char*,...);
void (*CORD_debug_printf)(CORD,...);
//allow for error handler to be changed at runtime
sexp (*handle_error_fp)();
extern sexp yyparse(FILE* input);
extern _tag parse_tagname(CORD tagname);
//maybe I need an eval.h?
extern sexp eval(sexp expr,env *cur_env);
extern sexp call_builtin(sexp expr,env *cur_env);
extern sexp call_lambda(sexp expr,env *cur_env);
extern sexp lisp_funcall(sexp expr,env *cur_env);
extern function_args *getFunctionArgs(sexp arglist,function_args *args,env *cur_env);
extern sexp lisp_apply(sexp function,sexp arguments,sexp envrionment);
extern sexp lisp_macroexpand(sexp cur_macro,env *cur_env);
extern sexp lispRead(CORD code);// __attribute__((pure));
//I don't need to pull in all of the hash functions
extern uint64_t fnv_hash(const void* key,int keylen);
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
static inline sexp lisp_id(sexp obj){return obj;}
static inline sexp lisp_not(sexp obj){
  return (isTrue(obj)?LISP_FALSE:LISP_TRUE);
}
#endif
