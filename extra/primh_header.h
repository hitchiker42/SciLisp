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