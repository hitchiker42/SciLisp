/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/* This File is automatically generated do not edit */
#ifndef __PRIM_H_
#define __PRIM_H_
#include "common.h"
#include "cons.h"
#include "array.h"
#include "print.h"
#include "env.h"
#include "lisp_math.h"
#include <time.h>
#define DEFUN_INTERN_OBARRAY(l_name,c_name)                             \
  symbol c_name ## _sym=                                                \
    (symbol){.name = l_name,.val =                                      \
             {.tag=_fun,.val={.fun = &c_name##_call}},                  \
             .symbol_env=ob_env};                                       \
  symref c_name ##_ptr=&c_name##_sym;                                   \
  obarray_add_entry(ob,c_name##_ptr)

#define DEFUN_INTERN(l_name,c_name)                                     \
  global_symbol c_name ## _sym=                                         \
    (global_symbol){.name = l_name,.val =                               \
                    {.tag=_fun,.val={.fun = &c_name##_call}},           \
                    .symbol_env=&topLevelEnv};                          \
  global_symref c_name ## _ptr=&c_name##_sym;                           \
  addGlobalSymMacro(c_name##_ptr)

#define INTERN_ALIAS(l_name,c_name,gensym)                              \
  global_symbol c_name ## _sym ## gensym=                               \
    (global_symbol){.name = l_name,.val =                               \
                    {.tag=_fun,.val={.fun = &c_name##_call}},           \
                    .symbol_env=&topLevelEnv};                          \
  global_symref c_name ## _ptr ## gensym =&c_name##_sym##gensym;        \
  addGlobalSymMacro(c_name##_ptr##gensym)
#define DEFCONST(lisp_name,c_name)                                      \
  global_symbol c_name ## _sym={.name = lisp_name,.val = c_name};       \
  global_symref c_name ## _ptr=&c_name##_sym;                           \
  addGlobalSymMacro(c_name##_ptr);
#define DEFUN_ARGS_0	(void)
#define DEFUN_ARGS_1	(sexp)
#define DEFUN_ARGS_2	(sexp, sexp)
#define DEFUN_ARGS_3	(sexp, sexp, sexp)
#define DEFUN_ARGS_4	(sexp, sexp, sexp, sexp)
#define DEFUN_ARGS_5	(sexp, sexp, sexp, sexp,        \
                         sexp)
#define DEFUN_ARGS_6	(sexp, sexp, sexp, sexp,        \
                         sexp, sexp)
#define DEFUN_ARGS_7	(sexp, sexp, sexp, sexp,        \
                         sexp, sexp, sexp)
#define DEFUN_ARGS_8	(sexp, sexp, sexp, sexp,        \
                         sexp, sexp, sexp, sexp)
#define DEFUN_ARGS_MANY (...)
#define DEFUN(cname,numargs)                     \
  sexp cname DEFUN_ARGS_##numargs ;              \
  extern function cname ## _call
static void* GC_REALLOC_3(void* ptr,size_t old,size_t new){
  return GC_REALLOC(ptr,new);
}
static void GC_FREE_2(void* ptr,size_t size){
  return GC_FREE(ptr);
}
static void* GC_MALLOC_1(size_t size){
  return GC_MALLOC(size);
}
extern const sexp lisp_pi;
extern const sexp lisp_euler;
extern const sexp lisp_max_long;
extern const sexp lisp_mach_eps;
void initPrims();
//create prototypes for functions in prim.c 
//so primitives can be used in the c source
DEFUN(lisp_add,2);
DEFUN(lisp_sub,2);
DEFUN(lisp_mul,2);
DEFUN(lisp_div,2);
DEFUN(lisp_lt,2);
DEFUN(lisp_gt,2);
DEFUN(lisp_gte,2);
DEFUN(lisp_lte,2);
DEFUN(lisp_ne,2);
DEFUN(lisp_numeq,2);
DEFUN(lisp_inc,1);
DEFUN(lisp_dec,1);
DEFUN(Cons,2);
DEFUN(set_car,2);
DEFUN(set_cdr,2);
DEFUN(last,1);
DEFUN(push_cons,2);
DEFUN(pop_cons,1);
DEFUN(mapcar,2);
DEFUN(reduce,2);
DEFUN(qsort_cons,2);
DEFUN(lisp_length,1);
DEFUN(lisp_pow,2);
DEFUN(lisp_sqrt,1);
DEFUN(lisp_cos,1);
DEFUN(lisp_sin,1);
DEFUN(lisp_tan,1);
DEFUN(lisp_exp,1);
DEFUN(lisp_log,1);
DEFUN(lisp_min,2);
DEFUN(lisp_max,2);
DEFUN(lisp_mod,2);
DEFUN(lisp_abs,1);
DEFUN(lisp_eql,2);
DEFUN(lisp_equal,2);
DEFUN(lisp_evenp,1);
DEFUN(lisp_oddp,1);
DEFUN(lisp_zerop,1);
DEFUN(lisp_sum,2);
DEFUN(lisp_iota,5);
DEFUN(aref,2);
DEFUN(array_to_list,1);
DEFUN(array_iota,4);
DEFUN(lisp_typeName,1);
DEFUN(typeOf,1);
DEFUN(lisp_print,1);
DEFUN(lisp_println,1);
DEFUN(lisp_eval,2);
DEFUN(lisp_open,2);
DEFUN(lisp_close,1);
DEFUN(lisp_fputs,2);
DEFUN(lisp_fprint,2);
DEFUN(lisp_fprintln,2);
DEFUN(lisp_cat,2);
DEFUN(lisp_getcwd,0);
DEFUN(lisp_system,2);
DEFUN(lisp_eq,2);
DEFUN(ccall,5);
DEFUN(lisp_xor,2);
DEFUN(lisp_logand,2);
DEFUN(lisp_logor,2);
DEFUN(ash,2);
DEFUN(lisp_round,2);
DEFUN(lisp_randfloat,1);
DEFUN(lisp_randint,0);
DEFUN(lisp_bigint,1);
DEFUN(lisp_bigfloat,3);
DEFUN(lisp_apply,3);
DEFUN(lisp_bigfloat_add,2);
DEFUN(lisp_bigfloat_sub,2);
DEFUN(lisp_bigfloat_mul,2);
DEFUN(lisp_bigfloat_div,2);
DEFUN(lisp_bigfloat_pow,2);
DEFUN(lisp_bigfloat_gt,2);
DEFUN(lisp_bigfloat_eq,2);
DEFUN(lisp_bigfloat_lt,2);
DEFUN(lisp_bigfloat_ge,2);
DEFUN(lisp_bigfloat_le,2);
DEFUN(lisp_bigfloat_ne,2);
DEFUN(lisp_arrayp,1);
DEFUN(lisp_consp,1);
DEFUN(lisp_numberp,1);
DEFUN(lisp_nilp,1);
DEFUN(lisp_symbolp,1);
DEFUN(lisp_bigintp,1);
DEFUN(lisp_bigfloatp,1);
DEFUN(lisp_stringp,1);
DEFUN(lisp_bignump,1);
DEFUN(lisp_errorp,1);
DEFUN(lisp_functionp,1);
DEFUN(lisp_streamp,1);
DEFUN(lisp_add_driver,2);
DEFUN(lisp_sub_driver,2);
DEFUN(lisp_mul_driver,2);
DEFUN(lisp_div_driver,2);
DEFUN(lisp_pow_driver,2);
DEFUN(lisp_min_driver,2);
DEFUN(lisp_max_driver,2);
DEFUN(cdr,1);
DEFUN(cddr,1);
DEFUN(cdddr,1);
DEFUN(cddddr,1);
DEFUN(cdddar,1);
DEFUN(cddar,1);
DEFUN(cddadr,1);
DEFUN(cddaar,1);
DEFUN(cdar,1);
DEFUN(cdadr,1);
DEFUN(cdaddr,1);
DEFUN(cdadar,1);
DEFUN(cdaar,1);
DEFUN(cdaadr,1);
DEFUN(cdaaar,1);
DEFUN(car,1);
DEFUN(cadr,1);
DEFUN(caddr,1);
DEFUN(cadddr,1);
DEFUN(caddar,1);
DEFUN(cadar,1);
DEFUN(cadadr,1);
DEFUN(cadaar,1);
DEFUN(caar,1);
DEFUN(caadr,1);
DEFUN(caaddr,1);
DEFUN(caadar,1);
DEFUN(caaar,1);
DEFUN(caaadr,1);
DEFUN(caaaar,1);
#undef DEFUN
#endif
