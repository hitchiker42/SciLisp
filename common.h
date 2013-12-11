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
#define THREAD_LOCAL_ALLOC
#define USE_MMAP
#include "gc/include/gc/gc.h"
#include <pthread.h>
#include <ctype.h>
#include <unistd.h>
#include <stdarg.h>
#include <math.h>
#include <signal.h>
#include <errno.h>
//several other headers are included in types.h
#include "types.h"
#include "env.h"
#include "print.h"
#include "bignum.h"
#include "cffi.h"
#include "llvm_externs.h"
//enable/disable debugging/lexing output
#define HERE_ON
#define DEBUG
//enable/disable SciLisp multithreading (gc is always multithreaded)
//#define MULTI_THREADED
//print what is being lexed, very verbose, rarely useful
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
#define construct_sexp_generic(sexp_val,sexp_tag,                       \
                               sexp_field,is_ptr_val,sexp_cast)         \
  sexp_cast {.tag=sexp_tag,.val={.sexp_field=sexp_val},.is_ptr=is_ptr_val}
#define construct_sexp_generic_len(sexp_val,sexp_tag,len_val,           \
                                   sexp_field,is_ptr_val,sexp_cast)     \
  sexp_cast {.tag=sexp_tag,.val={.sexp_field=sexp_val},                 \
      .is_ptr=is_ptr_val,.len=len_val}
#define construct_sexp_len(sexp_val,sexp_tag,sexp_field,is_ptr_val,len_val) \
  construct_sexp_generic_len(sexp_val,sexp_tag,len_val,                 \
                         sexp_field,is_ptr_val,(sexp))
#define construct_sexp(sexp_val,sexp_tag,sexp_field,is_ptr_val)         \
  construct_sexp_generic(sexp_val,sexp_tag,sexp_field,is_ptr_val,(sexp))
#define construct_const_sexp(sexp_val,sexp_tag,sexp_field,is_ptr_val)   \
  construct_sexp_generic(sexp_val,sexp_tag,sexp_field,is_ptr_val,)
#define construct_simple(sexp_val,name,is_ptr_val)                 \
  construct_sexp(sexp_val,_##name,name,is_ptr_val)
#define construct_ptr(sexp_val,name)                        \
  construct_simple(sexp_val,name,1)
#define construct_atom(sexp_val,name)                        \
  construct_simple(sexp_val,name,0)
#define construct_simple_const(sexp_val,name)                           \
  construct_const_sexp(sexp_val,_##name,name,0)
//type_sexp macros for convience (kinda like constructors I suppose)
#define typed_array_sexp(array_val,array_tag,array_len)              \
  (sexp){.tag=_typed_array,.meta=array_tag,.len=array_len,           \
      .val={.typed_array=array_val},.is_ptr=1}
#define bigfloat_sexp(bigfloat_ptr) construct_ptr(bigfloat_ptr,bigfloat)
#define bigint_sexp(bigint_ptr) construct_ptr(bigint_ptr,bigint)
#define cons_sexp(cons_val) construct_ptr(cons_val,cons)
#define cord_sexp(cord_val) string_sexp(cord_val)
#define c_data_sexp(c_data_val) construct_sexp(c_data_val,_cdata,c_val,1)
#define double_sexp(double_val) construct_atom(double_val,real64)
#define env_sexp(env_val) construct_sexp(env_val,_env,cur_env,1)
#define error_sexp(error_msg) construct_sexp(error_msg,_error,cord,1)
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
#define list_sexp(list_val) construct_sexp(list_val,_list,cons,1)
#define list_len_sexp(list_val,_len)                    \
  construct_sexp_len(list_val,_list,cons,1,_len);
#define long_sexp(long_val) construct_atom(long_val,int64)
#define ulong_sexp(ulong_val) construct_atom(ulong_val,uint64)
#define macro_sexp(macro_val) construct_sexp(macro_val,_macro,mac,1)
#define meta_sexp(meta_val) construct_atom(meta_val,meta)
#define obarray_sexp(ob_val) construct_sexp(ob_val(sexp),_obarray,ob,1)
#define opaque_sexp(opaque_val) construct_ptr(opaque_val,opaque)
#define re_match_sexp(match_val) construct_ptr(match_val,re_data)
#define real64_sexp(real64_val) construct_atom(real64_val,real64)
#define array_sexp(array_val,array_len)                         \
  construct_sexp_len(array_val,_array,array,1,array_len)
#define spec_sexp(spec_tag) construct_atom(spec_tag,special)
#define stream_sexp(stream_val) construct_ptr(stream_val,stream)
#define string_sexp(string_val) construct_sexp(string_val,_str,cord,1)
#define symref_sexp(symref_val) construct_sexp(symref_val,_sym,var,1)
#define tree_sexp(tree_val) construct_ptr(tree_val,tree)
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
//global variables
sexp* yylval;
FILE* yyin;
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
stack_t sigstk;
//functions to print (or not print) debug info
void (*debug_printf)(const char*,...);
void (*CORD_debug_printf)(CORD,...);
//allow for error handler to be changed at runtime
sexp (*handle_error_fp)();
//from eval.c(maybe I need an eval.h?)
extern sexp eval(sexp expr,env *cur_env);
extern sexp call_builtin(sexp expr,env *cur_env);
extern sexp call_lambda(sexp expr,env *cur_env);
extern sexp lisp_funcall(sexp expr,env *cur_env);
extern function_args *getFunctionArgs(sexp arglist,function_args *args,env *cur_env);
extern sexp lisp_apply(sexp function,sexp arguments,sexp envrionment);
extern sexp lisp_macroexpand(sexp cur_macro,env *cur_env);
//from parser.c
extern sexp read_string(CORD code);
extern sexp lisp_read(sexp code);
extern sexp yyparse(FILE* input);
extern _tag parse_tagname(CORD tagname) __attribute__((const));
//I don't need to pull in all of the hash functions
extern uint64_t fnv_hash(const void* key,int keylen);
extern void initialize_llvm();
//some of these could be moved to different files,
//some can't
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
//#define return_errno(fn_name), was here, now in debug.h
//rather simple but fairly useful functions
static inline sexp lisp_id(sexp obj){return obj;}
static inline sexp lisp_not(sexp obj){
  return (isTrue(obj)?LISP_FALSE:LISP_TRUE);
}
//Implements a stack for jmp_buf objects to allow eaiser nesting of
//setjmp/longjmp pairs
//non portable and it uses a fixed size array, not good, fix?
struct __jmp_buf_tag jmp_buf_stack[8];
static int jmp_buf_stack_len=0;
static void jmp_buf_hack(jmp_buf buf){
  //(hopefully temporary) but very ugly hack
  //but it's better than random segfaults
    jmp_buf_stack[0]=buf[0];
    struct __jmp_buf_tag temp=jmp_buf_stack[0];
    int i;
    for(i=0;i<7;i++){
      temp=jmp_buf_stack[i+1];
      jmp_buf_stack[i+1]=jmp_buf_stack[i];
      jmp_buf_stack[i]=temp;
    }
    return;
}
static inline void push_jmp_buf(jmp_buf buf){
  if(jmp_buf_stack_len>=8){
    jmp_buf_hack(buf);
  }
  jmp_buf_stack[jmp_buf_stack_len]=buf[0];
  jmp_buf_stack_len++;
  return;
}
static inline struct __jmp_buf_tag pop_jmp_buf(){
  jmp_buf_stack_len--;
  return jmp_buf_stack[jmp_buf_stack_len];
}
static inline struct __jmp_buf_tag peek_jmp_buf(){
  return jmp_buf_stack[jmp_buf_stack_len];
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
static void __attribute__((noreturn))default_condition_handler(int signum){
  longjmp(error_buf,-1);
}
static sexp lisp_eval(sexp obj,sexp env){
  return eval(obj,topLevelEnv);
}
static const struct sigaction sigusr1_object={.sa_handler=default_condition_handler};
static const struct sigaction sigusr2_object={.sa_handler=default_condition_handler};
static const struct sigaction *sigusr1_action=&sigusr1_object;
static const struct sigaction *sigusr2_action=&sigusr2_object;
#endif
