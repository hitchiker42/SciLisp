/* This file is automatically generated, do not edit*/
/* Declarations of predefined symbols/macros to generate predefined symbols
   
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
#ifndef _PRIM_H_
#define _PRIM_H_
#define DECLARE_GLOBAL(name)                    \
  extern symbol *name;                          \
  extern sexp name##_sexp;

#define DEFSUBR(cname,numargs)                    \
  sexp cname DEFUN_ARGS_##numargs ;               \
  extern subr cname ## _fun
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
#define DEFUN_ARGS_MANY (uint64_t,sexp*)
#define DEFUN_ARGS_UNEVALED (sexp)
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
extern symref lisp_ans_ptr;
extern sexp type_of_tag(sexp_tag tag);
extern sexp type_of(sexp obj);
void initPrims();
#ifdef INSIDE_PRIMS
#define MAKE_SELF_QUOTING_SYMBOL(cname,lname,sym_len,sym_hashv,proplist) \
  symbol_name cname##_name={.hashv=sym_hashv,.is_const=1,               \
                                   .name_len=sym_len,.name=lname};      \
  symbol cname##_val={.name=&cname##_name,                              \
                      .plist=proplist,.next=NULL};                      \
  cname##_val.val=symref_sexp(&cname##_val);                            \
  symbol *cname=&cname##_val;                                           \
  sexp cname##_sexp=const_symref_sexp(&cname##_val)
#define MAKE_SYMBOL(cname,lname,sym_len,sym_hashv,sym_val,proplist,const_sym) \
  symbol_name cname##_name={.hashv=sym_hashv,.is_const=const_sym,       \
                            .name_len=sym_len,.name=lname};             \
  symbol cname##_val={.val=val,.name=&cname##_name,                     \
                      .plist=proplist,.next=NULL};                      \
  symbol *cname=&cname##_val;                                           \
  sexp cname##_sexp=const_symref_sexp(&cname##_val)
#define MAKE_TYPE(cname,lname,sym_len,sym_hashv,proplist,type_tag)      \
  symbol_name cname##_name={.hashv=sym_hashv,.is_const=1,               \
                            .name_len=sym_len,.name=#lname};            \
  symbol cname##_val={.name=&cname##_name,.plist=proplist,.next=NULL};  \
  cname##_val.val={.tag=sexp_type,.val={.type=type_tag}};               \
  symbol *cname = &cname##_val;                                         \
  sexp cname##_sexp = const_symref_sexp(&cname##_val)
#define PRIM_DEFSUBR(l_name,c_name,reqargs,optargs,keyargs,             \
                restarg,max_args,fieldname,arglist,type)                \
  subr c_name##_subr=                                                   \
    {.req_args=reqargs,.opt_args=optargs,.keyword_args=keyargs,         \
     .rest_arg=restarg,.maxargs=max_args,                               \
     .lname=l_name,.cname=#c_name,                                      \
     .comp = {.fieldname=c_name}, .signature=arglist,                   \
     .subr_type = type};
#define PRIM_DEFMACRO(l_name,c_name,reqargs,optargs,keyargs,            \
                 restarg,max_args,arglist)                              \
  DEFSUBR(l_name,c_name,reqargs,optargs,keyargs,                        \
          restarg,max_args,funevaled,arglist,subr_compiler_macro)
#endif
