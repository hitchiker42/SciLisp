/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/* TODO write a script/elisp program to write the code for all the DEFUN
   type functions*/
#ifndef __PRIM_H_
#define __PRIM_H_
#include "common.h"
#include "cons.h"
#include "array.h"
#include "print.h"
#include <time.h>
#define DEFUN_INTERN(lname,cname)                                       \
  global_symbol cname ## _sym=(global_symbol){.name = lname,.val = {.tag=_fun,.val={.fun = &cname##call}}}; \
  global_symref cname ## _ptr=&cname##_sym;                             \
  addGlobalSymMacro(cname##_ptr)
#define INTERN_ALIAS(lname,cname,gensym)                                \
  global_symbol cname ## _sym ## gensym=                                \
    (global_symbol){.name = lname,.val =                                \
                    {.tag=_fun,.val={.fun = &cname##call}}};            \
  global_symref cname ## _ptr ## gensym =&cname##_sym##gensym;          \
  addGlobalSymMacro(cname##_ptr##gensym)
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
#define DEFUN(cname,numargs)                    \
  sexp cname DEFUN_ARGS_##numargs ;              \
  extern fxn_proto cname ## call
extern sexp lisp_pi;
extern sexp lisp_euler;
extern sexp lisp_max_long;
extern sexp lisp_mach_eps;
//create prototypes for functions in prim.c
//so primitives can be used in the c source
void hello_world();
DEFUN(lisp_consp,2);
DEFUN(lisp_numberp,1);
DEFUN(lisp_arrayp,1);
DEFUN(lisp_nilp,1);
DEFUN(lisp_add,2);
DEFUN(lisp_sub,2);
DEFUN(lisp_mul,2);
DEFUN(lisp_div,2);
DEFUN(lisp_xor,2);
DEFUN(lisp_logand,2);
DEFUN(lisp_logor,2);
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
DEFUN(last,1);
DEFUN(Cons,2);
DEFUN(mapcar,2);
DEFUN(set_car,2);
DEFUN(set_cdr,2);
DEFUN(lisp_typeName,1);
DEFUN(lisp_print,1);
DEFUN(lisp_println,1);
DEFUN(reduce,2);
DEFUN(lisp_lt,2);
DEFUN(lisp_gt,2);
DEFUN(lisp_gte,2);
DEFUN(lisp_lte,2);
DEFUN(lisp_ne,2);
DEFUN(lisp_equals,2);
DEFUN(lisp_pow,2);
DEFUN(lisp_sqrt,1);
DEFUN(lisp_cos,1);
DEFUN(lisp_sin,1);
DEFUN(lisp_tan,1);
DEFUN(lisp_exp,1);
DEFUN(lisp_log,1);
DEFUN(lisp_abs,1);
DEFUN(ash,2);
DEFUN(lisp_mod,2);
DEFUN(lisp_randfloat,1);
DEFUN(lisp_randint,0);
DEFUN(lisp_iota,4);
DEFUN(aref,2);
DEFUN(array_to_list,1);
DEFUN(lisp_eval,1);//welp this is going to fail horribly
DEFUN(lisp_length,1);
DEFUN(lisp_round,2);
DEFUN(pop_cons,1);
DEFUN(push_cons,2);
DEFUN(qsort_cons,2);
DEFUN(lisp_inc,1);
DEFUN(lisp_dec,1);
DEFUN(lisp_fputs,2);
DEFUN(lisp_open,2);
DEFUN(lisp_close,1);
/*
  (defun SciLisp-mkIntern ()
  (interactive)
  (let ((start (point)))
  (save-excursion
  (replace-regexp-lisp ",[0-9],[0-9]);" ");\\\\")
  (goto-char start)
  (replace-regexp-lisp "DEFUN(" "DEFUN_INTERN("))))*/
//insert code to initialize the global symbol table
//with all the primitive functions
#define initPrims()                                                     \
  if(initPrimsFlag){                                                    \
  initPrimsFlag=0;                                                      \
  globalSymbolTable=(global_env){.enclosing=NULL,.head=NULL};           \
  topLevelEnv=(env){.tag = _global,.enclosing=NULL,.head={.global = globalSymbolTable.head}}; \
  DEFUN_INTERN("+",lisp_add);                                           \
  DEFUN_INTERN("-",lisp_sub);                                           \
  DEFUN_INTERN("*",lisp_mul);                                           \
  DEFUN_INTERN("/",lisp_div);                                           \
  DEFUN_INTERN("logxor",lisp_xor);                                      \
  DEFUN_INTERN("logand",lisp_logand);                                   \
  DEFUN_INTERN("logor",lisp_logor);                                     \
  DEFUN_INTERN("car",car);                                              \
  DEFUN_INTERN("cdr",cdr);                                              \
  DEFUN_INTERN("<",lisp_lt);                                            \
  DEFUN_INTERN(">",lisp_gt);                                            \
  DEFUN_INTERN(">=",lisp_gte);                                          \
  DEFUN_INTERN("<=",lisp_lte);                                          \
  DEFUN_INTERN("!=",lisp_ne);                                           \
  DEFUN_INTERN("=",lisp_equals);                                        \
  DEFUN_INTERN("expt",lisp_pow);                                        \
  DEFUN_INTERN("sqrt",lisp_sqrt);                                       \
  DEFUN_INTERN("cos",lisp_cos);                                         \
  DEFUN_INTERN("sin",lisp_sin);                                         \
  DEFUN_INTERN("tan",lisp_tan);                                         \
  DEFUN_INTERN("exp",lisp_exp);                                         \
  DEFUN_INTERN("log",lisp_log);                                         \
  DEFUN_INTERN("caar",caar);                                            \
  DEFUN_INTERN("cadr",cadr);                                            \
  DEFUN_INTERN("cddr",cddr);                                            \
  DEFUN_INTERN("cdar",cdar);                                            \
  DEFUN_INTERN("caaar",caaar);                                          \
  DEFUN_INTERN("caadr",caadr);                                          \
  DEFUN_INTERN("caddr",caddr);                                          \
  DEFUN_INTERN("cdddr",cdddr);                                          \
  DEFUN_INTERN("cddar",cddar);                                          \
  DEFUN_INTERN("cdaar",cdaar);                                          \
  DEFUN_INTERN("cadar",cadar);                                          \
  DEFUN_INTERN("cdadr",cdadr);                                          \
  DEFUN_INTERN("caaadr",caaadr);                                        \
  DEFUN_INTERN("caadar",caadar);                                        \
  DEFUN_INTERN("caaddr",caaddr);                                        \
  DEFUN_INTERN("cadaar",cadaar);                                        \
  DEFUN_INTERN("cadadr",cadadr);                                        \
  DEFUN_INTERN("caddar",caddar);                                        \
  DEFUN_INTERN("cadddr",cadddr);                                        \
  DEFUN_INTERN("cdaaar",cdaaar);                                        \
  DEFUN_INTERN("cdaadr",cdaadr);                                        \
  DEFUN_INTERN("cdadar",cdadar);                                        \
  DEFUN_INTERN("cdaddr",cdaddr);                                        \
  DEFUN_INTERN("cddaar",cddaar);                                        \
  DEFUN_INTERN("cddadr",cddadr);                                        \
  DEFUN_INTERN("cdddar",cdddar);                                        \
  DEFUN_INTERN("cddddr",cddddr);                                        \
  DEFUN_INTERN("last",last);                                            \
  DEFUN_INTERN("cons",Cons);                                            \
  DEFUN_INTERN("typeName",lisp_typeName);                               \
  DEFUN_INTERN("print",lisp_print);                                     \
  DEFUN_INTERN("println",lisp_println);                                 \
  DEFUN_INTERN("reduce",reduce);                                        \
  DEFUN_INTERN("abs",lisp_abs);                                         \
  DEFUN_INTERN("ash",ash);                                              \
  DEFUN_INTERN("mod",lisp_mod);                                         \
  DEFUN_INTERN("drand",lisp_randfloat);                                 \
  DEFUN_INTERN("lrand",lisp_randint);                                   \
  DEFUN_INTERN("iota",lisp_iota);                                       \
  DEFUN_INTERN("aref",aref);                                            \
  DEFUN_INTERN("array->list",array_to_list);                            \
  DEFUN_INTERN("eval",lisp_eval);                                       \
  DEFUN_INTERN("length",lisp_length);                                   \
  DEFUN_INTERN("arrayp",lisp_arrayp);                                   \
  DEFUN_INTERN("consp",lisp_consp);                                     \
  DEFUN_INTERN("mapcar",mapcar);                                        \
  DEFUN_INTERN("set-car!",set_car);                                     \
  DEFUN_INTERN("set-cdr!",set_cdr);                                     \
  DEFUN_INTERN("round",lisp_round);                                     \
  DEFUN_INTERN("numberp",lisp_numberp);                                 \
  DEFUN_INTERN("nilp",lisp_nilp);                                       \
  DEFUN_INTERN("pop!",pop_cons);                                        \
  DEFUN_INTERN("push!",push_cons);                                      \
  DEFUN_INTERN("qsort!",qsort_cons);                                    \
  DEFUN_INTERN("++",lisp_inc);                                          \
  DEFUN_INTERN("--",lisp_dec);                                          \
  DEFUN_INTERN("fopen",lisp_open);                                      \
  DEFUN_INTERN("fclose",lisp_close);                                    \
  DEFUN_INTERN("fputs",lisp_fputs);                                     \
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
  srand48(time(NULL));}
#undef DEFUN
#endif
