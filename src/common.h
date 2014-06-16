/* Global header file for standard includes and macros

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
//global includes
#ifndef __COMMON_H__
#define __COMMON_H__
//system includes
#include <stdio.h>
#include <stdlib.h>
//#define GC_DEBUG
//#define GC_PRINT_STATS
#include <sched.h>
#include <sys/wait.h>
#include <pthread.h>
#include <ctype.h>
#include <unistd.h>
#include <stdarg.h>
#include <math.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include "config.h"
//types.h is effectively part of this file, just seperated out
//to help modularize code
#include "types.h"
//not sure if I need this, need to check in gc.h
#if (defined (GC_THREADS) && defined (GC_THREAD_LOCAL))
#define GC_REDIRECT_TO_LOCAL
#endif
//define some macros to make code work when multithreaded or not
#ifdef MULTI_THREADED
#include <pthread.h>
static pthread_once_t pthread_prims_initialized = PTHREAD_ONCE_INIT;
static pthread_once_t signal_handlers_initialized = PTHREAD_ONCE_INIT;
//generic global lock, in case it's needed
static pthread_mutex_t global_lock = PTHREAD_MUTEX_INITIALIZER;
#define thread_local __thread
#define static_thread_local static __thread
#define multithreaded_only(code) code
#define LISP_RWLOCK_RDLOCK(env) (pthread_rwlock_rdlock(env->lock))
#define LISP_RWLOCK_WRLOCK(env) (pthread_rwlock_wrlock(env->lock))
#define LISP_RWLOCK_UNLOCK(env) (pthread_rwlock_unlock(env->lock))
#define LOCK_GLOBAL_LOCK() (pthread_mutex_lock(&global_lock))
#define UNLOCK_GLOBAL_LOCK() (pthread_mutex_unlock(&global_lock))
#define pthread_create_checked(thread, attr, start_routine, arg)         \
  (if((pthread_create(thread, attr, start_routine, arg))){               \
    perror("Program error, exiting:\nthread creation failed");        \
    exit(4);                                                          \
  })                                                                  
#else
#define thread_local
#define multithreaded_only(code)
#define LISP_RWLOCK_RDLOCK(env)
#define LISP_RWLOCK_WRLOCK(env)
#define LISP_RWLOCK_UNLOCK(env)
#define LOCK_GLOBAL_LOCK()
#define UNLOCK_GLOBAL_LOCK()
#endif
//declare allocation routines
#include "gc/gc.h"
#ifdef UNSAFE_ALLOCATION
//unchecked allocation
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
#else
static void *xmalloc(size_t sz)__attribute__ ((warn_unused_result));
static void *xrealloc(void *ptr, size_t sz)__attribute__ ((warn_unused_result));
static void *xmalloc_atomic(size_t sz)__attribute__ ((warn_unused_result));
#endif
//current dynamic environment
extern thread_local struct obarray *current_obarray;
extern thread_local struct environment *current_env;
//includes from SciLisp files
#include "debug.h"
#include "read.h"
#include "print.h"
#include "bignum.h"
#include "cffi.h"
#include "llvm_externs.h"
//we need NIL in env.h
static const sexp NIL = {.val = {0}};//NIL is all 0s
#include "env.h"
#define top_level_frame current_env->protect_frame
//temporary, bulitin_symbols.h should be generated and placed in the src dir
#include "prim.h"
#include "frames.h"
/*actually define allocation routines
  SciLisp allocation routines will set the current environment's error_num
  field to ENOMEM and raise SIGUSR1 which will call a generic error handler
  for lisp errors that use C signals.
*/
#ifndef UNSAFE_ALLOCATION
static inline void *xmalloc(size_t sz){
  void *test = GC_MALLOC(sz);
  if(!test && sz){
    current_env->error_num = ENOMEM;
    raise(SIGUSR1);
  }
  return test;
}
static inline void *xrealloc(void *ptr, size_t sz){
  void *test = GC_REALLOC(ptr, sz);
  if(!test && sz){
    current_env->error_num = ENOMEM;
    raise(SIGUSR1);
  }
  return test;
}
#define xfree(sz) GC_free
static inline void *xmalloc_atomic(size_t sz){
  void *test = GC_MALLOC_ATOMIC(sz);
  if(!test && sz){
    current_env->error_num = ENOMEM;
    raise(SIGUSR1);
  }
  return test;
}
//use for large (>100kb) objects, disables interior pointers
//beyond the first 256 bytes
static inline void *xmalloc_large(size_t sz){
  void *test = GC_MALLOC_IGNORE_OFF_PAGE(sz);
  if(!test && sz){
    current_env->error_num = ENOMEM;
    raise(SIGUSR1);
  }
  return test;
}
  
//version unexposed to user code or anything
//taken from the code for GC_memalign
//but opimized to assume align is newer anywhere
//near the page size and  all_interiror_pointers
//is true (which it is, if I change that I'll need to change this)
//get default alignment
#include "gc/gc_tiny_fl.h"//for GC_GRANULE_BYTES
#define DEFAULT_ALIGNMENT GC_GRANULE_BYTES
static void *xmemalign(size_t align, size_t sz){
  void *result;
  if(align <= DEFAULT_ALIGNMENT){
    result = xmalloc(sz);
  } else {
    size_t new_sz = sz+align-1;
    result = xmalloc(new_sz);
    size_t offset = (size_t)result % align;
    //technically a gcc extension to add to a void*
    result = result+offset;
  }
  return result;
}
//needs to be a macro for obvious reasons
//assume you're not an idiot and sz is > initial sizeof ptr
#define re_alloca(ptr, old_sz, sz)                \
  ({void *tmp = alloca(sz);                       \
    memcpy(tmp, ptr, old_sz);                     \
    ptr = tmp;})
#endif /*not unsafe_allocation*/
//common macros, made somewhat more typesafe using __typeof__
//and statement exprs
#define MAX(a, b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a > _b ? _a : _b;})
#define MIN(a, b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a > _b ? _a : _b;})
#define SWAP(a, b)                               \
  ({ __typeof__ (a) _a = a;                     \
    a = b;                                        \
    b=_a;                                       \
    ;})
#define ARR_SWAP(arr, i, j)                       \
  SWAP(arr[i], arr[j])
#define CORD_asprintf(format, args...)           \
  ({CORD retval;                                \
  CORD_sprintf(&retval, format, ##args);          \
  retval;})
#define scilisp_unlikely(expr) __builtin_expect(expr, 0)
#define scilisp_likely(expr) __builtin_expect(expr, 1)
#define scilisp_expect(expr, c) __builtin_expect(expr, c)

#define make_struct_ptr (type, value)                    \
  ({type _temp = xmalloc(sizeof(type));                 \
    *_temp = (type)value;                               \
    _temp})
#define set_struct_ptr (name, value)                    \
  (*name = (__typeof__ (name))value)
//Macros used in defining sexp constructor macros
#define construct_sexp_generic(sexp_val, sexp_tag,                       \
                               sexp_field, is_ptr_val, sexp_cast)         \
  sexp_cast {.tag = sexp_tag, .val = {.sexp_field = sexp_val},                 \
      .is_ptr = is_ptr_val, .meta = 0, .simple_len = 0}
#define construct_sexp_generic_len(sexp_val, sexp_tag, len_val,           \
                                   sexp_field, is_ptr_val, sexp_cast)     \
  sexp_cast {.tag = sexp_tag, .val = {.sexp_field = sexp_val},                 \
      .is_ptr = is_ptr_val, .simple_len = len_val, .meta = 0}
#define construct_sexp_len(sexp_val, sexp_tag, sexp_field, is_ptr_val, len_val) \
  construct_sexp_generic_len(sexp_val, sexp_tag, len_val,                 \
                             sexp_field, is_ptr_val, (sexp))
#define construct_sexp(sexp_val, sexp_tag, sexp_field, is_ptr_val)         \
  construct_sexp_generic(sexp_val, sexp_tag, sexp_field, is_ptr_val, (sexp))
#define construct_const_sexp(sexp_val, sexp_tag, sexp_field, is_ptr_val)   \
  construct_sexp_generic(sexp_val, sexp_tag, sexp_field, is_ptr_val, )
#define construct_simple(sexp_val, name, is_ptr_val)      \
  construct_sexp(sexp_val, sexp_##name, name, is_ptr_val)
#define construct_ptr(sexp_val, name)            \
  construct_simple(sexp_val, name, 1)
#define construct_atom(sexp_val, name)           \
  construct_simple(sexp_val, name, 0)
#define construct_simple_const(sexp_val, name)   \
  construct_const_sexp(sexp_val, sexp_##name, name, 0)
//Actual type specific sexp constructor macros
/*#define typed_array_sexp(array_val,array_tag,array_len)       \
  (sexp){.tag=_typed_array, .meta = array_tag, .len = array_len,      \
  .val = {.typed_array = array_val}, .is_ptr = 1}*/
#define bigfloat_sexp(bigfloat_ptr) construct_ptr(bigfloat_ptr, bigfloat)
#define bigint_sexp(bigint_ptr) construct_ptr(bigint_ptr, bigint)
#define cons_sexp(cons_val) construct_ptr(cons_val, cons)
#define cord_sexp(cord_val) string_sexp(make_string(cord_val))
#define c_data_sexp(c_data_val) construct_sexp(c_data_val, sexp_cdata, c_val, 1)
#define c_char_sexp(c_char_val) construct_atom(c_char_val, c_char)
#define double_sexp(double_val) construct_atom(double_val, real64)
#define env_sexp(env_val) construct_sexp(env_val, sexp_env, cur_env, 1)
#define error_sexp(error_msg) construct_sexp(error_msg, sexp_error, c_string, 1)
#define float_sexp(float_val) construct_atom(float_val, real32)
#define funargs_sexp(funargs_val) construct_ptr(funargs_val, funargs)
#define function_sexp(fun_val) construct_ptr(fun_val, fun)
#define hashtable_sexp(ht_val) construct_ptr(ht_val, hashtable)
#define heap_sexp(heap_val) construct_ptr(heap_val, heap)
#define int_n_sexp(int_val, n) construct_atom(int_val, int##n)
#define int64_sexp(int64_val) int_n_sexp(int64_val, 64)
#define uint_n_sexp(uint_val, n) construct_atom(uint_val, uint##n)
#define uint64_sexp(uint64_val) uint_n_sexp(uint64_val, 64)
#define keyword_sexp(keyword_val) construct_atom(keyword_val, keyword)
#define list_sexp(list_val) construct_sexp(list_val, sexp_cons, cons, 1)
#define list_len_sexp(plist_val, _len)                    \
  construct_sexp_len(list_val, _list, cons, 1, _len);
#define long_sexp(long_val) construct_atom(long_val, int64)
#define ulong_sexp(ulong_val) construct_atom(ulong_val, uint64)
//#define macro_sexp(macro_val) construct_sexp(macro_val,_macro,mac,1)
#define meta_sexp(meta_val) construct_atom(meta_val, meta)
#define obarray_sexp(ob_val) construct_sexp(ob_val, sexp_obarray, ob, 1)
#define opaque_sexp(opaque_val) construct_ptr(opaque_val, opaque)
#define re_match_sexp(match_val) construct_ptr(match_val, re_data)
#define regex_sexp(regex_val) construct_ptr(regex_val, regex)
#define real32_sexp(real32_val) construct_atom(real32_val, real32)
#define real64_sexp(real64_val) construct_atom(real64_val, real64)
#define array_sexp(array_val) construct_ptr(array_val, array)
//#define spec_sexp(spec_tag) construct_atom(spec_tag,special)
#define stream_sexp(stream_val) construct_ptr(stream_val, stream)
#define string_sexp(string_val) construct_ptr(string_val, string)
#define subr_sexp(subr_val) construct_ptr(subr_val, subr)
#define const_subr_sexp(subr_val) construct_const_sexp(subr_val, sexp_subr, subr, 1)
//
#define c_string_sexp(c_string_val) construct_ptr(c_string_val, c_string)
#define symref_sexp(symref_val) construct_ptr(symref_val, sym)
#define const_symref_sexp(symref_val) construct_const_sexp(symref_val, sexp_symbol, sym, 1)
#define tree_sexp(tree_val) construct_ptr(tree_val, tree)
#define type_sexp(type_val) construct_sexp(type_val, sexp_type, sym, 1)
#define uchar_sexp(uchar_val) construct_atom(uchar_val, uchar)
#define mb_char_sexp(mb_char_val) construct_sexp(mb_char_val, sexp_mb_char, uint64, 0)
#define NIL_MACRO() {.tag = -1, .val = {.meta = -1}}
#define format_error_str(format, args...) (CORD_sprintf(&error_str, format, ##args))
#define format_error_sexp(format, args...)       \
  format_error_str(format, args),                \
    error_sexp(CORD_to_char_star(error_str))
//We use a seperate stack for handling signals so that we don't die on stack
//overflow, this macro initializes that stack
#define init_sigstk(sigstk)                     \
  ({sigstk->ss_sp = malloc(SIGSTKSZ);                                      \
  if(!sigstk->ss_sp){                                                    \
  fprintf(stderr, "error, virtual memory exhausted\n");                  \
  exit(EXIT_FAILURE);                                                   \
  }                                                                     \
  sigstk->ss_size = SIGSTKSZ;                                              \
  sigaltstack(sigstk, NULL);})
//lisp constants needed in c
static const sexp UNBOUND = {.tag = sexp_unbound, .val = {0}};
static const sexp LISP_TRUE = {.tag = sexp_true, .val = {1}};
static const sexp LISP_FALSE = {.tag = sexp_false, .val = {0}};
static const lisp_string LISP_EMPTY_STRING = {.string = NULL, .len = 0};
static const sexp LISP_EMPTY_STRING_SEXP = {.tag = sexp_string, .
                                          val = {.string = (lisp_string*)&LISP_EMPTY_STRING},
                                          .is_ptr = 1};
//(defconst empty-list (cons nil nil))
static const cons EmptyList = {.car = {.val = {0}},
                             .cdr = {.val = {0}}};
static const sexp LispEmptyList = {.tag = sexp_cons, .val = {.cons = (cons*)&EmptyList}, .is_ptr = 1};
//global variables
static uint64_t gensym_counter = 0;//only modify this using atomic fetch and add
//probably don't need anymore, what with pthread_once
static int init_prims_flag = 1;
//These are depreciated, will be removed soon
CORD error_str;
CORD type_error_str;
jmp_buf error_buf;
sexp error_val;
/*from env.h
static thread_local struct obarray *current_obarray;
static thread_local struct environment *current_env;*/
//functions to print (or not print) debug info
static void (*debug_printf)(const char*, ...) = default_debug_printf;
static void (*CORD_debug_printf)(CORD, ...) = default_CORD_debug_printf;
//allow for error handler to be changed at runtime
sexp (*handle_error_fp)();
//These are currently invalid
//from eval.c(maybe I need an eval.h?)
extern sexp eval(sexp expr, env_ptr env);
extern sexp call_builtin(sexp expr, env_ptr env);
extern sexp call_lambda(sexp expr, env_ptr env);
extern sexp lisp_funcall(sexp expr, env_ptr env);
extern sexp lisp_apply(sexp function, sexp arguments, sexp envrionment);
extern sexp lisp_macroexpand(sexp cur_macro, env_ptr env);
//from parser.c
extern sexp read_string(CORD code);
extern sexp lisp_read(sexp code);
extern sexp_tag parse_tagname(lisp_string tagname) __attribute__((const));
extern void initialize_llvm();
//some of these could be moved to different files,some can't
static thread_local const char *output_file = NULL;

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
static inline void* xrecalloc(void *ptr, uint64_t old_size, uint64_t size){
  ptr = xrealloc(ptr, size);
  if(size>old_size){
    memset(ptr+old_size, (size-old_size), '\0');
  }
  return ptr;
}
//default values for condition handlers,
//in general sigusr1 & sigusr2 should be more than enough to implement conditions
//NOTE: conditions are not errors or labols, conditions are morally equivlent to
//ucontext values in c
static void __attribute__((noreturn))default_condition_handler(int signum){
  raise_simple_error(signum,
                     CORD_asprintf("Received signal number %d:\n%s",
                                   signum, strsignal(signum)));
}
sexp lisp_eval(sexp obj, sexp env);
static const struct sigaction sigusr1_object = {.sa_handler = default_condition_handler};
static const struct sigaction sigusr2_object = {.sa_handler = default_condition_handler};
static const struct sigaction *sigusr1_action=&sigusr1_object;
static const struct sigaction *sigusr2_action=&sigusr2_object;
void SciLisp_init();
/* Defined externally (uses an optimized assembly version for x86_64)
   copies size 8 bytes chunks of memory from src to dest, data is taken
   from src at intervals of stride*8 bytes, thus src should be
   size*stride*8 bytes large. If stride is 2 then this should be about
   as fast as a normal memcpy, for larger strides it will be notablely
   slower (stride 2 is optimized specially).Returns dest.   

   for example given src [1, 2, 3, 4, 5, 6, 7, 8] and some large enough dest 
   memcpy_stride(dest, src, 4, 2) == [1, 3, 5, 7] and
   memcpy_stride(dest, src, 2, 4) == [1, 5] or
   memcpy_stride(dest, src+1, 4, 2) == [2, 4, 6, 8]

   In SciLisp the biggest use of this is to copy only the data fields out
   of an array of sexps, for example:
   given an sexp* of length 8 and an data* of length 8
   memcpy_stride(dest, src, 8, 2); will copy the raw data from src to dest
*/
extern void *memcpy_stride
(void *dest, const void *src, size_t size, size_t stride) __attribute__((leaf));
//same as above but with 32 bit chunks
extern void *memcpy_stride_32
(void *dest, const void *src, size_t size, size_t stride) __attribute__((leaf));
#endif
