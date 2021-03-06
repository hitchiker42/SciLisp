/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/* This file is autogenerated by generate_prims.el do not edit */
/*"standard library" of SciLisp as it were.*/
#include "array.h"
#include "common.h"
#include "cons.h"
#include "hash.h"
#include "hash_fn.h"
#include "lisp_math.h"
#include "prim.h"
#include "print.h"
#include "regex.h"
#include "sequence.h"
#include "cffi.h"
//NOTE: Most of these macros are non hygenic and rely on the presense
//of an obarray named ob, used outside of this file at your own risk
#define DEFUN(l_name,c_name,reqargs,optargs,keyargs,restarg,maxargs,sig) \
  function_args c_name##_args=                                          \
    { .num_req_args=reqargs,.num_opt_args=optargs,.num_keyword_args=keyargs, \
      .has_rest_arg=restarg,.args=0,.max_args=maxargs };                \
  function c_name##_call=                                               \
    { .args=&c_name##_args,.lname=l_name,.cname=#c_name,                \
      .comp = {.f##maxargs=c_name}, .signature=sig,                     \
      .type = _compiled_fun };
#define DEFMACRO(l_name,c_name,reqargs,optargs,keyargs,restarg,maxargs,cargs) \
  function_args c_name##_args=                                          \
    { .num_req_args=reqargs,.num_opt_args=optargs,.num_keyword_args=keyargs, \
      .has_rest_arg=restarg,.args=0,.max_args=maxargs };                \
  function c_name##_expander=                                           \
    {.args=&c_name##_args,.lname=l_name,.cname=#c_name,                 \
     .comp = {.f##cargs=c_name},.type=_compiled_fun}
#define MAKE_SYMBOL(l_name,c_name,hash_v)                               \
  symbol c_name ## _sym = {.name=l_name,.val={.tag=_fun,.val={.fun=0}}}; \
  symref c_name ## _ptr=0;                                              \
  obarray_entry c_name ##_ob_entry={.prev=0,.next=0,.ob_symbol=0,       \
                                    .hashv=hash_v}
#define MAKE_MACRO_SYMBOL(l_name,c_name,hash_v)                         \
  symbol c_name ## _sym = {.name=l_name,.val=                           \
                           {.tag=_fun,.meta=_builtin_macro,.val={.fun=0}}}; \
  symref c_name ## _ptr=0;                                              \
  obarray_entry c_name ##_ob_entry={.prev=0,.next=0,.ob_symbol=0,       \
                                    .hashv=hash_v}
#define MAKE_GLOBAL(l_name,c_name,constant)                             \
  symbol c_name ## _sym = {.name=l_name,.val=c_name,                    \
                           .props={.is_const=constant}};                \
  symref c_name ## _ptr = 0;                                            \
  obarray_entry c_name##_ob_entry={.prev=0,.next=0,.ob_symbol=0,.hashv=0}
#define MAKE_TYPE(l_name,l_tag)                                         \
  symref l_name##_ptr=0;                                                \
  symbol l_name##_sym = {.name="#<"#l_name">",                          \
                         .val={.tag=_type,.val={.meta = l_tag}},        \
                         .props={.is_const=1,.global=1,.type=_type}};   \
  obarray_entry l_name##_ob_entry={.prev=0,.next=0,.ob_symbol=0,.hashv=0}
#define INIT_SYMBOL(c_name)                                     \
  c_name##_ptr=&c_name##_sym;                                   \
  c_name##_ptr->val.val.fun=&c_name##_call;                     \
  c_name##_ob_entry.ob_symbol=c_name##_ptr;                     \
  prim_obarray_add_entry(ob,c_name##_ptr,&c_name##_ob_entry)
#define INIT_MACRO_SYMBOL(c_name)                               \
  c_name##_ptr=&c_name##_sym;                                   \
  c_name##_ptr->val.val.fun=&c_name##_expander;                 \
  c_name##_ob_entry.ob_symbol=c_name##_ptr;                     \
  prim_obarray_add_entry(ob,c_name##_ptr,&c_name##_ob_entry)
#define INIT_GLOBAL(c_name)                                     \
  c_name##_ptr=&c_name##_sym;                                   \
  c_name##_ob_entry.ob_symbol=c_name##_ptr;                     \
  prim_obarray_add_entry(ob,c_name##_ptr,&c_name##_ob_entry)
#define INIT_SYNONYM(c_name,l_name,gensym_counter)                      \
  symref c_name##_ptr_syn ##gensym_counter= xmalloc(sizeof(symbol));    \
  * c_name##_ptr_syn ## gensym_counter = *c_name ##_ptr;                \
  c_name##_ptr_syn ## gensym_counter -> name = l_name;                  \
  obarray_entry *c_name##_ob_entry##gensym_counter=                     \
    xmalloc(sizeof(obarray_entry));                                     \
  c_name##_ob_entry##gensym_counter->ob_symbol=c_name##_ptr_syn##gensym_counter; \
  prim_obarray_add_entry(globalObarray,c_name##_ptr_syn ## gensym_counter, \
                         c_name##_ob_entry##gensym_counter)
#define MAKE_KEYSYM(c_name) keyword_symbol c_name##_key
 //kinda a lazy way to do this
#define INIT_KEYSYM(c_name,l_name)              \
  c_name##_key.val.uint64=getKeySymSexp(l_name)
void hello_world(){
  printf("hello, world!\n");
  return;
}
#define set_global_vars()                                               \
  lisp_stderr_sym.val.val.stream=stderr;                                \
  lisp_stdout_sym.val.val.stream=stdout;                                \
  lisp_stdin_sym.val.val.stream=stdin;                                  \
  mpz_t *mpz_const_1=xmalloc(sizeof(mpz_t));                            \
  mpz_t *mpz_const_0=xmalloc(sizeof(mpz_t));                            \
  mpfr_t *mpfr_const_1=xmalloc(sizeof(mpfr_t));                         \
  mpfr_t *mpfr_const_0=xmalloc(sizeof(mpfr_t));                         \
  mpfr_t *mpfr_const_e=xmalloc(sizeof(mpfr_t));                         \
  mpfr_t *mpfr_const_pi_var=xmalloc(sizeof(mpfr_t));                    \
  mpfr_t *mpfr_const_nan=xmalloc(sizeof(mpfr_t));                       \
  mpfr_t *mpfr_const_inf=xmalloc(sizeof(mpfr_t));                       \
  mpz_init((*mpz_const_0));                                             \
  mpfr_init((*mpfr_const_0));                                           \
  mpfr_init((*mpfr_const_nan));                                         \
  mpfr_init((*mpfr_const_inf));                                         \
  mpz_init_set_ui((*mpz_const_1),1);                                    \
  mpfr_init_set_ui((*mpfr_const_1),1,MPFR_RNDN);                        \
  mpfr_init((*mpfr_const_e));                                           \
  mpfr_init((*mpfr_const_pi_var));                                      \
  mpfr_exp(*mpfr_const_e,*mpfr_const_1,MPFR_RNDN);                      \
  mpfr_const_pi(*mpfr_const_pi_var,MPFR_RNDN);                          \
  mpfr_set_nan(*mpfr_const_nan);                                        \
  mpfr_set_inf(*mpfr_const_inf,1);                                      \
  lisp_bigint_0_sym.val.val.bigint=mpz_const_0;                         \
  lisp_bigint_1_sym.val.val.bigint=mpz_const_1;                         \
  lisp_bigfloat_0_sym.val.val.bigfloat=mpfr_const_0;                    \
  lisp_bigfloat_1_sym.val.val.bigfloat=mpfr_const_1;                    \
  lisp_bigfloat_e_sym.val.val.bigfloat=mpfr_const_e;                    \
  lisp_bigfloat_pi_sym.val.val.bigfloat=mpfr_const_pi_var

#define lisp_stderr {.tag = _stream,.val={.stream=0}}
#define lisp_stdout {.tag = _stream,.val={.stream=0}}
#define lisp_stdin {.tag = _stream,.val={.stream=0}}
#define lisp_mach_eps  {.tag=_double,.val={.real64=1.41484755040568800000e-16}}
#define lisp_pi  {.tag=_double,.val={.real64=3.14159265358979323846}}
#define lisp_euler {.tag=_double,.val={.real64=2.7182818284590452354}}
#define lisp_max_long  {.tag=_long,.val={.int64=LONG_MAX}}
#define lisp_double_0  {.tag=_double,.val={.real64=0.0}}
#define lisp_double_1  {.tag=_double,.val={.real64=1.0}}
#define lisp_long_0  {.tag=_long,.val={.int64=0}}
#define lisp_long_1  {.tag=_long,.val={.int64=1}}
//allocating static space for pointers, not actually initalizing constants
#define lisp_bigint_0  {.tag=_bigint,.val={.bigint=0}}
#define lisp_bigint_1  {.tag=_bigint,.val={.bigint=0}}
#define lisp_bigfloat_0   {.tag=_bigfloat,.val={.bigfloat=0}}
#define lisp_bigfloat_1   {.tag=_bigfloat,.val={.bigfloat=0}}
#define lisp_bigfloat_e {.tag=_bigfloat,.val={.bigfloat=0}}
#define lisp_bigfloat_pi {.tag=_bigfloat,.val={.bigfloat=0}}
#define lisp_NIL {.tag = -1,.val={.meta = -1}}
#define lisp_LISP_TRUE {.tag = -2,.val={.meta = 11}}
#define lisp_LISP_FALSE {.tag = -3,.val={.meta = -3}}
#define lisp_ans {.tag=-1,.val={.meta=-1},.quoted=0}
void SciLisp_init(){
    //setup handler for sigsegv, so we can exit gracefully on a segfault
#ifdef DEBUG
  debug_printf=default_debug_printf;
  CORD_debug_printf=default_CORD_debug_printf;
#endif
  //allocate signal stack before gc init so gc doesn't have to worry about it
  init_sigstk();
  GC_set_all_interior_pointers(1);
  GC_set_handle_fork(1);
  GC_init();
  pthread_once_t pthread_prims_initialized = PTHREAD_ONCE_INIT;
  pthread_once(&pthread_prims_initialized,initPrims);
}
