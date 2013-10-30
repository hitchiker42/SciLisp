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
#include <time.h>
#define DEFUN_INTERN_OBARRAY(l_name,c_name)                             \
  symbol c_name ## _sym=                                                \
    (symbol){.name = l_name,.val =                                      \
             {.tag=_fun,.val={.fun = &c_name##call}},                   \
             .symbol_env=ob_env};                                      \
  symref c_name ## _ptr=&c_name##_sym;                                  \
  obarray_add_entry(ob,c_name##_ptr)

#define DEFUN_INTERN(l_name,c_name)                                       \
  global_symbol c_name ## _sym=                                         \
    (global_symbol){.name = l_name,.val =                               \
                    {.tag=_fun,.val={.fun = &c_name##call}},            \
                    .symbol_env=&topLevelEnv};                           \
  global_symref c_name ## _ptr=&c_name##_sym;                             \
  addGlobalSymMacro(c_name##_ptr)
#define INTERN_ALIAS(l_name,c_name,gensym)                                \
  global_symbol c_name ## _sym ## gensym=                                \
    (global_symbol){.name = l_name,.val =                                \
                    {.tag=_fun,.val={.fun = &c_name##call}},            \
                    .symbol_env=&topLevelEnv};                           \
  global_symref c_name ## _ptr ## gensym =&c_name##_sym##gensym;          \
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
#define DEFUN(cname,numargs)                                \
  sexp cname DEFUN_ARGS_##numargs ;              \
  extern function cname ## call
#define lisp_stderr {.tag = _stream,.val={.stream=stderr}}
#define lisp_stdout {.tag = _stream,.val={.stream=stdout}}
#define lisp_stdin {.tag = _stream,.val={.stream=stdin}}
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
DEFUN(lisp_sum,2);
DEFUN(Cons,2);
DEFUN(car,1);
DEFUN(cdr,1);
DEFUN(caar,1);
DEFUN(cadr,1);
DEFUN(cddr,1);
DEFUN(cdar,1);
DEFUN(caaar,1);
DEFUN(caadr,1);
DEFUN(caddr,1);
DEFUN(cdddr,1);
DEFUN(cddar,1);
DEFUN(cdaar,1);
DEFUN(cadar,1);
DEFUN(cdadr,1);
DEFUN(caaaar,1);
DEFUN(caaadr,1);
DEFUN(caadar,1);
DEFUN(caaddr,1);
DEFUN(cadaar,1);
DEFUN(cadadr,1);
DEFUN(caddar,1);
DEFUN(cadddr,1);
DEFUN(cdaaar,1);
DEFUN(cdaadr,1);
DEFUN(cdadar,1);
DEFUN(cdaddr,1);
DEFUN(cddaar,1);
DEFUN(cddadr,1);
DEFUN(cdddar,1);
DEFUN(cddddr,1);
DEFUN(set_car,2);
DEFUN(set_cdr,2);
DEFUN(last,1);
DEFUN(push_cons,2);
DEFUN(pop_cons,1);
DEFUN(mapcar,2);
DEFUN(reduce,2);
DEFUN(qsort_cons,2);
DEFUN(lisp_length,1);
DEFUN(lisp_iota,4);
DEFUN(aref,2);
DEFUN(array_to_list,1);
DEFUN(lisp_typeName,1);
DEFUN(typeOf,1);
DEFUN(lisp_print,1);
DEFUN(lisp_println,1);
DEFUN(lisp_eval,1);
DEFUN(lisp_open,2);
DEFUN(lisp_close,1);
DEFUN(lisp_fputs,2);
DEFUN(lisp_fprint,2);
DEFUN(lisp_fprintln,2);
DEFUN(lisp_cat,2);
DEFUN(lisp_getcwd,0);
DEFUN(lisp_system,2);
DEFUN(lisp_eq,2);
DEFUN(lisp_xor,2);
DEFUN(lisp_logand,2);
DEFUN(lisp_logor,2);
DEFUN(ash,2);
DEFUN(lisp_pow,2);
DEFUN(lisp_sqrt,1);
DEFUN(lisp_cos,1);
DEFUN(lisp_sin,1);
DEFUN(lisp_tan,1);
DEFUN(lisp_exp,1);
DEFUN(lisp_log,1);
DEFUN(lisp_abs,1);
DEFUN(lisp_mod,2);
DEFUN(lisp_min,2);
DEFUN(lisp_max,2);
DEFUN(lisp_round,2);
DEFUN(lisp_randfloat,1);
DEFUN(lisp_randint,0);
DEFUN(lisp_consp,1);
DEFUN(lisp_numberp,1);
DEFUN(lisp_arrayp,1);
DEFUN(lisp_nilp,1);
DEFUN(lisp_stringp,1);
DEFUN(lisp_symbolp,1);
DEFUN(lisp_bigint,1);
DEFUN(lisp_bigfloat,3);
DEFUN(lisp_gmp_add,2);
DEFUN(lisp_gmp_sub,2);
DEFUN(lisp_gmp_mul,2);
DEFUN(lisp_gmp_mod,2);
DEFUN(lisp_gmp_cdiv_q,2);
DEFUN(lisp_gmp_cdiv_r,2);
DEFUN(lisp_gmp_fdiv_q,2);
DEFUN(lisp_gmp_fdiv_r,2);
DEFUN(lisp_gmp_tdiv_r,2);
DEFUN(lisp_gmp_tdiv_q,2);
DEFUN(lisp_gmp_and,2);
DEFUN(lisp_gmp_ior,2);
DEFUN(lisp_gmp_xor,2);
static void initPrimsObarray(obarray *ob,env* ob_env){
DEFUN_INTERN_OBARRAY("+",lisp_add);
DEFUN_INTERN_OBARRAY("-",lisp_sub);
DEFUN_INTERN_OBARRAY("*",lisp_mul);
DEFUN_INTERN_OBARRAY("/",lisp_div);
DEFUN_INTERN_OBARRAY("<",lisp_lt);
DEFUN_INTERN_OBARRAY(">",lisp_gt);
DEFUN_INTERN_OBARRAY(">=",lisp_gte);
DEFUN_INTERN_OBARRAY("<=",lisp_lte);
DEFUN_INTERN_OBARRAY("!=",lisp_ne);
DEFUN_INTERN_OBARRAY("=",lisp_numeq);
DEFUN_INTERN_OBARRAY("++",lisp_inc);
DEFUN_INTERN_OBARRAY("--",lisp_dec);
DEFUN_INTERN_OBARRAY("sum",lisp_sum);
DEFUN_INTERN_OBARRAY("cons",Cons);
DEFUN_INTERN_OBARRAY("car",car);
DEFUN_INTERN_OBARRAY("cdr",cdr);
DEFUN_INTERN_OBARRAY("caar",caar);
DEFUN_INTERN_OBARRAY("cadr",cadr);
DEFUN_INTERN_OBARRAY("cddr",cddr);
DEFUN_INTERN_OBARRAY("cdar",cdar);
DEFUN_INTERN_OBARRAY("caaar",caaar);
DEFUN_INTERN_OBARRAY("caadr",caadr);
DEFUN_INTERN_OBARRAY("caddr",caddr);
DEFUN_INTERN_OBARRAY("cdddr",cdddr);
DEFUN_INTERN_OBARRAY("cddar",cddar);
DEFUN_INTERN_OBARRAY("cdaar",cdaar);
DEFUN_INTERN_OBARRAY("cadar",cadar);
DEFUN_INTERN_OBARRAY("cdadr",cdadr);
DEFUN_INTERN_OBARRAY("caaaar",caaaar);
DEFUN_INTERN_OBARRAY("caaadr",caaadr);
DEFUN_INTERN_OBARRAY("caadar",caadar);
DEFUN_INTERN_OBARRAY("caaddr",caaddr);
DEFUN_INTERN_OBARRAY("cadaar",cadaar);
DEFUN_INTERN_OBARRAY("cadadr",cadadr);
DEFUN_INTERN_OBARRAY("caddar",caddar);
DEFUN_INTERN_OBARRAY("cadddr",cadddr);
DEFUN_INTERN_OBARRAY("cdaaar",cdaaar);
DEFUN_INTERN_OBARRAY("cdaadr",cdaadr);
DEFUN_INTERN_OBARRAY("cdadar",cdadar);
DEFUN_INTERN_OBARRAY("cdaddr",cdaddr);
DEFUN_INTERN_OBARRAY("cddaar",cddaar);
DEFUN_INTERN_OBARRAY("cddadr",cddadr);
DEFUN_INTERN_OBARRAY("cdddar",cdddar);
DEFUN_INTERN_OBARRAY("cddddr",cddddr);
DEFUN_INTERN_OBARRAY("set-car!",set_car);
DEFUN_INTERN_OBARRAY("set-cdr!",set_cdr);
DEFUN_INTERN_OBARRAY("last",last);
DEFUN_INTERN_OBARRAY("push!",push_cons);
DEFUN_INTERN_OBARRAY("pop!",pop_cons);
DEFUN_INTERN_OBARRAY("mapcar",mapcar);
DEFUN_INTERN_OBARRAY("reduce",reduce);
DEFUN_INTERN_OBARRAY("qsort!",qsort_cons);
DEFUN_INTERN_OBARRAY("length",lisp_length);
DEFUN_INTERN_OBARRAY("iota",lisp_iota);
DEFUN_INTERN_OBARRAY("aref",aref);
DEFUN_INTERN_OBARRAY("array->list",array_to_list);
DEFUN_INTERN_OBARRAY("typeName",lisp_typeName);
DEFUN_INTERN_OBARRAY("typeOf",typeOf);
DEFUN_INTERN_OBARRAY("print",lisp_print);
DEFUN_INTERN_OBARRAY("println",lisp_println);
DEFUN_INTERN_OBARRAY("eval",lisp_eval);
DEFUN_INTERN_OBARRAY("fopen",lisp_open);
DEFUN_INTERN_OBARRAY("fclose",lisp_close);
DEFUN_INTERN_OBARRAY("fputs",lisp_fputs);
DEFUN_INTERN_OBARRAY("fprint",lisp_fprint);
DEFUN_INTERN_OBARRAY("fprintln",lisp_fprintln);
DEFUN_INTERN_OBARRAY("cat",lisp_cat);
DEFUN_INTERN_OBARRAY("pwd",lisp_getcwd);
DEFUN_INTERN_OBARRAY("system",lisp_system);
DEFUN_INTERN_OBARRAY("eq",lisp_eq);
DEFUN_INTERN_OBARRAY("logxor",lisp_xor);
DEFUN_INTERN_OBARRAY("logand",lisp_logand);
DEFUN_INTERN_OBARRAY("logor",lisp_logor);
DEFUN_INTERN_OBARRAY("ash",ash);
DEFUN_INTERN_OBARRAY("expt",lisp_pow);
DEFUN_INTERN_OBARRAY("sqrt",lisp_sqrt);
DEFUN_INTERN_OBARRAY("cos",lisp_cos);
DEFUN_INTERN_OBARRAY("sin",lisp_sin);
DEFUN_INTERN_OBARRAY("tan",lisp_tan);
DEFUN_INTERN_OBARRAY("exp",lisp_exp);
DEFUN_INTERN_OBARRAY("log",lisp_log);
DEFUN_INTERN_OBARRAY("abs",lisp_abs);
DEFUN_INTERN_OBARRAY("mod",lisp_mod);
DEFUN_INTERN_OBARRAY("min",lisp_min);
DEFUN_INTERN_OBARRAY("max",lisp_max);
DEFUN_INTERN_OBARRAY("round",lisp_round);
DEFUN_INTERN_OBARRAY("drand",lisp_randfloat);
DEFUN_INTERN_OBARRAY("lrand",lisp_randint);
DEFUN_INTERN_OBARRAY("consp",lisp_consp);
DEFUN_INTERN_OBARRAY("numberp",lisp_numberp);
DEFUN_INTERN_OBARRAY("arrayp",lisp_arrayp);
DEFUN_INTERN_OBARRAY("nilp",lisp_nilp);
DEFUN_INTERN_OBARRAY("stringp",lisp_stringp);
DEFUN_INTERN_OBARRAY("symbolp",lisp_symbolp);
DEFUN_INTERN_OBARRAY("bigint",lisp_bigint);
DEFUN_INTERN_OBARRAY("bigfloat",lisp_bigfloat);
DEFUN_INTERN_OBARRAY("bigint-add",lisp_gmp_add);
DEFUN_INTERN_OBARRAY("bigint-sub",lisp_gmp_sub);
DEFUN_INTERN_OBARRAY("bigint-mul",lisp_gmp_mul);
DEFUN_INTERN_OBARRAY("bigint-mod",lisp_gmp_mod);
DEFUN_INTERN_OBARRAY("bigint-cdiv_q",lisp_gmp_cdiv_q);
DEFUN_INTERN_OBARRAY("bigint-cdiv_r",lisp_gmp_cdiv_r);
DEFUN_INTERN_OBARRAY("bigint-fdiv_q",lisp_gmp_fdiv_q);
DEFUN_INTERN_OBARRAY("bigint-fdiv_r",lisp_gmp_fdiv_r);
DEFUN_INTERN_OBARRAY("bigint-tdiv_r",lisp_gmp_tdiv_r);
DEFUN_INTERN_OBARRAY("bigint-tdiv_q",lisp_gmp_tdiv_q);
DEFUN_INTERN_OBARRAY("bigint-and",lisp_gmp_and);
DEFUN_INTERN_OBARRAY("bigint-ior",lisp_gmp_ior);
DEFUN_INTERN_OBARRAY("bigint-xor",lisp_gmp_xor);
}
#define initPrims()                                                     \
if(initPrimsFlag){                                                    \
initPrimsFlag=0;                                                      \
globalSymbolTable=(global_env){.enclosing=NULL,.head=NULL};           \
topLevelEnv=(env){.tag = 1,.enclosing=NULL,.head={.global = globalSymbolTable.head}}; \
keywordSymbols=(global_env){.enclosing=NULL,.head=NULL};\
mpfr_set_default_prec(256);\
mp_set_memory_functions(GC_MALLOC_1,GC_REALLOC_3,GC_FREE_2);\
DEFUN_INTERN("+",lisp_add);\
DEFUN_INTERN("-",lisp_sub);\
DEFUN_INTERN("*",lisp_mul);\
DEFUN_INTERN("/",lisp_div);\
DEFUN_INTERN("<",lisp_lt);\
DEFUN_INTERN(">",lisp_gt);\
DEFUN_INTERN(">=",lisp_gte);\
DEFUN_INTERN("<=",lisp_lte);\
DEFUN_INTERN("!=",lisp_ne);\
DEFUN_INTERN("=",lisp_numeq);\
DEFUN_INTERN("++",lisp_inc);\
DEFUN_INTERN("--",lisp_dec);\
DEFUN_INTERN("sum",lisp_sum);\
DEFUN_INTERN("cons",Cons);\
DEFUN_INTERN("car",car);\
DEFUN_INTERN("cdr",cdr);\
DEFUN_INTERN("caar",caar);\
DEFUN_INTERN("cadr",cadr);\
DEFUN_INTERN("cddr",cddr);\
DEFUN_INTERN("cdar",cdar);\
DEFUN_INTERN("caaar",caaar);\
DEFUN_INTERN("caadr",caadr);\
DEFUN_INTERN("caddr",caddr);\
DEFUN_INTERN("cdddr",cdddr);\
DEFUN_INTERN("cddar",cddar);\
DEFUN_INTERN("cdaar",cdaar);\
DEFUN_INTERN("cadar",cadar);\
DEFUN_INTERN("cdadr",cdadr);\
DEFUN_INTERN("caaaar",caaaar);\
DEFUN_INTERN("caaadr",caaadr);\
DEFUN_INTERN("caadar",caadar);\
DEFUN_INTERN("caaddr",caaddr);\
DEFUN_INTERN("cadaar",cadaar);\
DEFUN_INTERN("cadadr",cadadr);\
DEFUN_INTERN("caddar",caddar);\
DEFUN_INTERN("cadddr",cadddr);\
DEFUN_INTERN("cdaaar",cdaaar);\
DEFUN_INTERN("cdaadr",cdaadr);\
DEFUN_INTERN("cdadar",cdadar);\
DEFUN_INTERN("cdaddr",cdaddr);\
DEFUN_INTERN("cddaar",cddaar);\
DEFUN_INTERN("cddadr",cddadr);\
DEFUN_INTERN("cdddar",cdddar);\
DEFUN_INTERN("cddddr",cddddr);\
DEFUN_INTERN("set-car!",set_car);\
DEFUN_INTERN("set-cdr!",set_cdr);\
DEFUN_INTERN("last",last);\
DEFUN_INTERN("push!",push_cons);\
DEFUN_INTERN("pop!",pop_cons);\
DEFUN_INTERN("mapcar",mapcar);\
DEFUN_INTERN("reduce",reduce);\
DEFUN_INTERN("qsort!",qsort_cons);\
DEFUN_INTERN("length",lisp_length);\
DEFUN_INTERN("iota",lisp_iota);\
DEFUN_INTERN("aref",aref);\
DEFUN_INTERN("array->list",array_to_list);\
DEFUN_INTERN("typeName",lisp_typeName);\
DEFUN_INTERN("typeOf",typeOf);\
DEFUN_INTERN("print",lisp_print);\
DEFUN_INTERN("println",lisp_println);\
DEFUN_INTERN("eval",lisp_eval);\
DEFUN_INTERN("fopen",lisp_open);\
DEFUN_INTERN("fclose",lisp_close);\
DEFUN_INTERN("fputs",lisp_fputs);\
DEFUN_INTERN("fprint",lisp_fprint);\
DEFUN_INTERN("fprintln",lisp_fprintln);\
DEFUN_INTERN("cat",lisp_cat);\
DEFUN_INTERN("pwd",lisp_getcwd);\
DEFUN_INTERN("system",lisp_system);\
DEFUN_INTERN("eq",lisp_eq);\
DEFUN_INTERN("logxor",lisp_xor);\
DEFUN_INTERN("logand",lisp_logand);\
DEFUN_INTERN("logor",lisp_logor);\
DEFUN_INTERN("ash",ash);\
DEFUN_INTERN("expt",lisp_pow);\
DEFUN_INTERN("sqrt",lisp_sqrt);\
DEFUN_INTERN("cos",lisp_cos);\
DEFUN_INTERN("sin",lisp_sin);\
DEFUN_INTERN("tan",lisp_tan);\
DEFUN_INTERN("exp",lisp_exp);\
DEFUN_INTERN("log",lisp_log);\
DEFUN_INTERN("abs",lisp_abs);\
DEFUN_INTERN("mod",lisp_mod);\
DEFUN_INTERN("min",lisp_min);\
DEFUN_INTERN("max",lisp_max);\
DEFUN_INTERN("round",lisp_round);\
DEFUN_INTERN("drand",lisp_randfloat);\
DEFUN_INTERN("lrand",lisp_randint);\
DEFUN_INTERN("consp",lisp_consp);\
DEFUN_INTERN("numberp",lisp_numberp);\
DEFUN_INTERN("arrayp",lisp_arrayp);\
DEFUN_INTERN("nilp",lisp_nilp);\
DEFUN_INTERN("stringp",lisp_stringp);\
DEFUN_INTERN("symbolp",lisp_symbolp);\
DEFUN_INTERN("bigint",lisp_bigint);\
DEFUN_INTERN("bigfloat",lisp_bigfloat);\
DEFUN_INTERN("bigint-add",lisp_gmp_add);\
DEFUN_INTERN("bigint-sub",lisp_gmp_sub);\
DEFUN_INTERN("bigint-mul",lisp_gmp_mul);\
DEFUN_INTERN("bigint-mod",lisp_gmp_mod);\
DEFUN_INTERN("bigint-cdiv_q",lisp_gmp_cdiv_q);\
DEFUN_INTERN("bigint-cdiv_r",lisp_gmp_cdiv_r);\
DEFUN_INTERN("bigint-fdiv_q",lisp_gmp_fdiv_q);\
DEFUN_INTERN("bigint-fdiv_r",lisp_gmp_fdiv_r);\
DEFUN_INTERN("bigint-tdiv_r",lisp_gmp_tdiv_r);\
DEFUN_INTERN("bigint-tdiv_q",lisp_gmp_tdiv_q);\
DEFUN_INTERN("bigint-and",lisp_gmp_and);\
DEFUN_INTERN("bigint-ior",lisp_gmp_ior);\
DEFUN_INTERN("bigint-xor",lisp_gmp_xor);\
INTERN_ALIAS("cons?",lisp_consp,17);                                  \
INTERN_ALIAS("array?",lisp_arrayp,23);                                \
DEFCONST("Meps",lisp_mach_eps);                                       \
DEFCONST("pi",lisp_pi);                                               \
DEFCONST("e",lisp_euler);                                             \
DEFCONST("nil",NIL);                                                  \
DEFCONST("t",LISP_TRUE);                                              \
DEFCONST("#f",LISP_FALSE);                                            \
DEFCONST("MAX_LONG",lisp_max_long);                                   \
DEFCONST("$$",LispEmptyList);                                         \
DEFCONST("stderr",lisp_stderr);                                       \
DEFCONST("stdout",lisp_stdout);                                       \
DEFCONST("stdin",lisp_stdin);                                         \
srand48(time(NULL));}
#undef DEFUN
#endif