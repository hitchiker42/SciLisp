/* Global header file for standard includes and macros

Copyright (C) 2014 Tucker DiNapoli

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
//global includes
#ifndef __COMMON_H__
#define __COMMON_H__
#include <stdio.h>
#include <stdlib.h>
//#define GC_DEBUG
//#define GC_PRINT_STATS
#include <sched.h>
#include <sys/wait.h>
#include "config.h"
//several other headers are included in types.h
#include "types.h"
#if (defined (GC_THREADS) && defined (GC_THREAD_LOCAL))
#define GC_REDIRECT_TO_LOCAL
#endif
//avoid including lex.yy.h in the lexer itself, because it messes up
//some macro defines / undefs
#ifdef MULTI_THREADED
static pthread_once_t pthread_prims_initialized=PTHREAD_ONCE_INIT;
#define thread_local __thread
#define multithreaded_only(code) code
#define LISP_RWLOCK_RDLOCK(env) (pthread_rwlock_rdlock(env->lock))
#define LISP_RWLOCK_WRLOCK(env) (pthread_rwlock_wrlock(env->lock))
#define LISP_RWLOCK_UNLOCK(env) (pthread_rwlock_unlock(env->lock))
#else
#define thread_local  
#define multithreaded_only(code)
#define LISP_RWLOCK_RDLOCK(env)
#define LISP_RWLOCK_WRLOCK(env)
#define LISP_RWLOCK_UNLOCK(env)
#endif
#ifndef IN_LEXER
#include "read_lex.h"
#endif
#include "gc/gc.h"
#include <pthread.h>
#include <ctype.h>
#include <unistd.h>
#include <stdarg.h>
#include <math.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include "print.h"
#include "bignum.h"
#include "cffi.h"
#include "llvm_externs.h"
#include "env.h"
//print what is being lexed, very verbose, rarely useful
//#define VERBOSE_LEXING
#include "debug.h"
//common macros, & memory allocation macros
#define MAX(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a > _b ? _a : _b;})
#define MIN(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a > _b ? _a : _b;})
#define CORD_asprintf(format,args...)           \
  ({CORD retval;                                \
  CORD_sprintf(&retval,format,##args);          \
  retval;})
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
#define construct_sexp_generic(sexp_val,sexp_tag,                       \
                               sexp_field,is_ptr_val,sexp_cast)         \
  sexp_cast {.tag=sexp_tag,.val={.sexp_field=sexp_val},                 \
      .is_ptr=is_ptr_val,.meta=0,.simple_len=0}
#define construct_sexp_generic_len(sexp_val,sexp_tag,len_val,           \
                                   sexp_field,is_ptr_val,sexp_cast)     \
  sexp_cast {.tag=sexp_tag,.val={.sexp_field=sexp_val},                 \
      .is_ptr=is_ptr_val,.simple_len=len_val,.meta=0}
#define construct_sexp_len(sexp_val,sexp_tag,sexp_field,is_ptr_val,len_val) \
  construct_sexp_generic_len(sexp_val,sexp_tag,len_val,                 \
                             sexp_field,is_ptr_val,(sexp))
#define construct_sexp(sexp_val,sexp_tag,sexp_field,is_ptr_val)         \
  construct_sexp_generic(sexp_val,sexp_tag,sexp_field,is_ptr_val,(sexp))
#define construct_const_sexp(sexp_val,sexp_tag,sexp_field,is_ptr_val)   \
  construct_sexp_generic(sexp_val,sexp_tag,sexp_field,is_ptr_val,)
#define construct_simple(sexp_val,name,is_ptr_val)      \
  construct_sexp(sexp_val,sexp_##name,name,is_ptr_val)
#define construct_ptr(sexp_val,name)            \
  construct_simple(sexp_val,name,1)
#define construct_atom(sexp_val,name)           \
  construct_simple(sexp_val,name,0)
#define construct_simple_const(sexp_val,name)   \
  construct_const_sexp(sexp_val,sexp_##name,name,0)
//type_sexp macros for convience (kinda like constructors I suppose)
/*#define typed_array_sexp(array_val,array_tag,array_len)       \
  (sexp){.tag=_typed_array,.meta=array_tag,.len=array_len,      \
  .val={.typed_array=array_val},.is_ptr=1}*/
#define bigfloat_sexp(bigfloat_ptr) construct_ptr(bigfloat_ptr,bigfloat)
#define bigint_sexp(bigint_ptr) construct_ptr(bigint_ptr,bigint)
#define cons_sexp(cons_val) construct_ptr(cons_val,cons)
#define cord_sexp(cord_val) string_sexp(cord_val)
#define c_data_sexp(c_data_val) construct_sexp(c_data_val,sexp_cdata,c_val,1)
#define double_sexp(double_val) construct_atom(double_val,real64)
#define env_sexp(env_val) construct_sexp(env_val,sexp_env,cur_env,1)
#define error_sexp(error_msg) construct_sexp(error_msg,sexp_error,simple_string,1)
#define float_sexp(float_val) construct_atom(float_val,real32)
#define funargs_sexp(funargs_val) construct_ptr(funargs_val,funargs)
#define function_sexp(fun_val) construct_ptr(fun_val,fun)
#define hashtable_sexp(ht_val) construct_ptr(ht_val,hashtable)
#define heap_sexp(heap_val) construct_ptr(heap_val,heap)
#define int_n_sexp(int_val,n) construct_atom(int_val,int##n)
#define int64_sexp(int64_val) int_n_sexp(int64_val,64)
#define uint_n_sexp(uint_val,n) construct_atom(uint_val,uint##n)
#define uint64_sexp(uint64_val) uint_n_sexp(uint64_val,64)
#define keyword_sexp(keyword_val) construct_atom(keyword_val,keyword)
#define list_sexp(list_val) construct_sexp(list_val,sexp_cons,cons,1)
#define list_len_sexp(list_val,_len)                    \
  construct_sexp_len(list_val,_list,cons,1,_len);
#define long_sexp(long_val) construct_atom(long_val,int64)
#define ulong_sexp(ulong_val) construct_atom(ulong_val,uint64)
//#define macro_sexp(macro_val) construct_sexp(macro_val,_macro,mac,1)
#define meta_sexp(meta_val) construct_atom(meta_val,meta)
#define obarray_sexp(ob_val) construct_sexp(ob_val(sexp),_obarray,ob,1)
#define opaque_sexp(opaque_val) construct_ptr(opaque_val,opaque)
#define re_match_sexp(match_val) construct_ptr(match_val,re_data)
#define regex_sexp(regex_val) construct_ptr(regex_val,regex)
#define real64_sexp(real64_val) construct_atom(real64_val,real64)
#define array_sexp(array_val,array_len)                         \
  construct_sexp_len(array_val,sexp_array,array,1,array_len)
#define spec_sexp(spec_tag) construct_atom(spec_tag,special)
#define stream_sexp(stream_val) construct_ptr(stream_val,stream)
#define string_sexp(string_val) construct_ptr(string_val,string)
#define symref_sexp(symref_val) construct_ptr(symref_val,sym)
#define tree_sexp(tree_val) construct_ptr(tree_val,tree)
//#define type_sexp(type_val) construct_sexp(type_val,sexp_type,meta,0)
#define uchar_sexp(uchar_val) construct_atom(uchar_val,uchar)
#define CORD_strdup(str) CORD_from_char_star(str)
#define CORD_append(val,ext) val=CORD_cat(val,ext)
#define CORD_cat_line(cord1,cord2) CORD_catn(3,cord1,cord2,"\n")
#define CORD_append_line(val,ext) val=CORD_cat_line(val,ext)
#define NIL_MACRO() {.tag = -1,.val={.meta = -1}}
#define format_error_str(format,args...) (CORD_sprintf(&error_str,format,##args))
#define format_error_sexp(format,args...)       \
  format_error_str(format,args),                \
    error_sexp(CORD_to_char_star(error_str))
#define init_sigstk()                                                   \
  sigstk.ss_sp=malloc(SIGSTKSZ);                                        \
  if(!sigstk.ss_sp){                                                    \
  fprintf(stderr,"error, virtual memory exhausted\n");                  \
  exit(EXIT_FAILURE);                                                   \
  }                                                                     \
  sigstk.ss_size=SIGSTKSZ;                                              \
  sigaltstack(&sigstk,NULL)
//lisp constants needed in c
static const sexp NIL={.val={0}};//NIL is all 0s
static const sexp UNBOUND={.tag = sexp_unbound,.val={0}};
static const sexp LISP_TRUE={.tag = sexp_true,.val={1}};
static const sexp LISP_FALSE={.tag = sexp_false,.val={0}};
static const lisp_string LISP_EMPTY_STRING={.string=NULL,.len=0};
static const sexp LISP_EMPTY_STRING_SEXP={.tag=sexp_string,.
                                          val={.string=(lisp_string*)&LISP_EMPTY_STRING},
                                          .is_ptr=1};
//(defconst empty-list (cons nil nil))
static const cons EmptyList={.car={.val={0}},
                             .cdr={.val={0}}};
static const sexp LispEmptyList={.tag=sexp_cons,.val={.cons=(cons*)&EmptyList},.is_ptr=1};
//global variables(not for long)
//sexp* yylval;
//FILE* yyin;
yyscan_t global_scanner;

//flag for errors at repl
extern int evalError;
//probably don't need anymore, what with pthread_once
static int initPrimsFlag=1;
static uint64_t global_gensym_counter=0;
//global localtion of error messages
CORD error_str;
CORD type_error_str;
jmp_buf error_buf;
sexp error_val;
//static __thread env *cur_env_ptr;
/*from env.h
static thread_local struct obarray *current_obarray;
static thread_local struct environment *current_environment;*/
//functions to print (or not print) debug info
void (*debug_printf)(const char*,...);
void (*CORD_debug_printf)(CORD,...);
//allow for error handler to be changed at runtime
sexp (*handle_error_fp)();
//from eval.c(maybe I need an eval.h?)
extern sexp eval(sexp expr,env_ptr env);
extern sexp call_builtin(sexp expr,env_ptr env);
extern sexp call_lambda(sexp expr,env_ptr env);
extern sexp lisp_funcall(sexp expr,env_ptr env);
extern sexp lisp_apply(sexp function,sexp arguments,sexp envrionment);
extern sexp lisp_macroexpand(sexp cur_macro,env_ptr env);
//from parser.c
extern sexp read_string(CORD code);
extern sexp lisp_read(sexp code);
extern sexp yyparse(FILE* input,yyscan_t scanner);
extern sexp_tag parse_tagname(lisp_string tagname) __attribute__((const));
extern void initialize_llvm();
//some of these could be moved to different files,
//some can't
static thread_local const char *output_file=NULL;
static inline double get_double_val(sexp x){
  switch(x.tag){
    case sexp_real64:
      return x.val.real64;
    case sexp_uint64:
    case sexp_int64:
      return (double)x.val.int64;
    default:
      return NAN;
  }
}
static inline double get_double_val_unsafe(sexp x){
  return (x.tag == sexp_real64 ? x.val.real64 : (double)x.val.int64);
}
//#define return_errno(fn_name), was here, now in debug.h
//rather simple but fairly useful functions
static inline sexp lisp_id(sexp obj){return obj;}
static inline sexp lisp_not(sexp obj){
  return (is_true(obj)?LISP_FALSE:LISP_TRUE);
}
//reallocate memory and set any newly allocated memory to 0
static inline void* xrecalloc(void *ptr,uint64_t old_size,uint64_t size){
  ptr=xrealloc(ptr,size);
  if(size>old_size){
    memset(ptr+old_size,(size-old_size),'\0');
  }
  return ptr;
}
//default values for condition handlers,
//in general sigusr1 & sigusr2 should be more than enough to implement conditions
//NOTE: conditions are not errors or labols, conditions are morally equivlent to
//ucontext values in c
static void __attribute__((noreturn))default_condition_handler(int signum){
  longjmp(error_buf,-1);
}
static sexp lisp_eval(sexp obj,sexp env){
  return eval(obj,current_environment);
}
static const struct sigaction sigusr1_object={.sa_handler=default_condition_handler};
static const struct sigaction sigusr2_object={.sa_handler=default_condition_handler};
static const struct sigaction *sigusr1_action=&sigusr1_object;
static const struct sigaction *sigusr2_action=&sigusr2_object;
void SciLisp_init();
#endif
