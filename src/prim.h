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
void init_prims();
#ifdef INSIDE_PRIMS
#define MAKE_SELF_QUOTING_SYMBOL(cname,lname,sym_len,sym_hashv,proplist) \
  symbol_name cname##_name={.hashv=sym_hashv,                           \
                            .name_len=sym_len,.name=lname};             \
  symbol cname##_val={.name=&cname##_name,.constant=1,                  \
                      .plist=proplist,.next=NULL,                       \
                      .val=const_symref_sexp(&cname##_val)};            \
  symbol *cname=&cname##_val;                                           \
  sexp cname##_sexp=const_symref_sexp(&cname##_val)
#define MAKE_SYMBOL(cname,lname,sym_len,sym_hashv,sym_val,proplist,const_sym) \
  symbol_name cname##_name={.hashv=sym_hashv,                           \
                            .name_len=sym_len,.name=lname};             \
  symbol cname##_val={.val=sym_val,.name=&cname##_name,                 \
                      .constant=const_sym,.plist=proplist,.next=NULL};  \
  symbol *cname=&cname##_val;                                           \
  sexp cname##_sexp=const_symref_sexp(&cname##_val)
#define MAKE_TYPE(cname,lname,sym_len,sym_hashv,proplist,type_tag)      \
  symbol_name cname##_name={.hashv=sym_hashv,                           \
                            .name_len=sym_len,.name=#lname};            \
  symbol cname##_val={.name=&cname##_name,.constant=1,                  \
                      .plist=proplist,.next=NULL,                       \
                      .val={.tag=sexp_type,.val={.type=type_tag}}};     \
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
//need to add later
#if 0
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
#endif
/*Declaration of default Symbols, and their corrsponding sexps*/
/*Error Symbols*/
extern symbol *Etype;
extern sexp Etype_sexp;
extern symbol *Ebounds;
extern sexp Ebounds_sexp;
extern symbol *Efile;
extern sexp Efile_sexp;
extern symbol *Eread;
extern sexp Eread_sexp;
extern symbol *Eargs;
extern sexp Eargs_sexp;
extern symbol *Ekey;
extern sexp Ekey_sexp;
extern symbol *Efatal;
extern sexp Efatal_sexp;
extern symbol *Eundefined;
extern sexp Eundefined_sexp;
extern symbol *Eunbound;
extern sexp Eunbound_sexp;
extern symbol *Emath;
extern sexp Emath_sexp;
extern symbol *Eeof;
extern sexp Eeof_sexp;
extern symbol *Eio;
extern sexp Eio_sexp;
extern symbol *Eoverflow;
extern sexp Eoverflow_sexp;
extern symbol *Erange;
extern sexp Erange_sexp;
extern symbol *Econst;
extern sexp Econst_sexp;
extern symbol *Esystem;
extern sexp Esystem_sexp;
extern symbol *Eprint;
extern sexp Eprint_sexp;
extern symbol *Evisibility;
extern sexp Evisibility_sexp;
extern symbol *Eilseq;
extern sexp Eilseq_sexp;
extern symbol *Einternal;
extern sexp Einternal_sexp;
/*Type Symbols*/
extern symbol *Tint8;
extern sexp Tint8_sexp;
extern symbol *Tint16;
extern sexp Tint16_sexp;
extern symbol *Tint32;
extern sexp Tint32_sexp;
extern symbol *Tint64;
extern sexp Tint64_sexp;
extern symbol *Tuint8;
extern sexp Tuint8_sexp;
extern symbol *Tuint16;
extern sexp Tuint16_sexp;
extern symbol *Tuint32;
extern sexp Tuint32_sexp;
extern symbol *Tuint64;
extern sexp Tuint64_sexp;
extern symbol *Terror;
extern sexp Terror_sexp;
extern symbol *Treal32;
extern sexp Treal32_sexp;
extern symbol *Treal64;
extern sexp Treal64_sexp;
extern symbol *Tbigint;
extern sexp Tbigint_sexp;
extern symbol *Tbigfloat;
extern sexp Tbigfloat_sexp;
extern symbol *Tchar;
extern sexp Tchar_sexp;
extern symbol *Tc_char;
extern sexp Tc_char_sexp;
extern symbol *Tstring;
extern sexp Tstring_sexp;
extern symbol *Tarray;
extern sexp Tarray_sexp;
extern symbol *Tstream;
extern sexp Tstream_sexp;
extern symbol *Tsubr;
extern sexp Tsubr_sexp;
extern symbol *Tsymbol;
extern sexp Tsymbol_sexp;
extern symbol *Ttype;
extern sexp Ttype_sexp;
extern symbol *Thashtable;
extern sexp Thashtable_sexp;
extern symbol *Tregex;
extern sexp Tregex_sexp;
extern symbol *Tnil;
extern sexp Tnil_sexp;
extern symbol *Tcons;
extern sexp Tcons_sexp;
extern symbol *Tenv;
extern sexp Tenv_sexp;
extern symbol *Tobarray;
extern sexp Tobarray_sexp;
extern symbol *Ttrue;
extern sexp Ttrue_sexp;
extern symbol *Tfalse;
extern sexp Tfalse_sexp;
extern symbol *Tuninterned;
extern sexp Tuninterned_sexp;
/*Special Form Symbols*/
extern symbol *Qlambda;
extern sexp Qlambda_sexp;
extern symbol *Qclosure;
extern sexp Qclosure_sexp;
extern symbol *Qnil;
extern sexp Qnil_sexp;
extern symbol *Qlet;
extern sexp Qlet_sexp;
extern symbol *Qlet_star;
extern sexp Qlet_star_sexp;
extern symbol *Qwhile;
extern sexp Qwhile_sexp;
extern symbol *Qtagbody;
extern sexp Qtagbody_sexp;
extern symbol *Qgo;
extern sexp Qgo_sexp;
extern symbol *Qthrow;
extern sexp Qthrow_sexp;
extern symbol *Qcatch;
extern sexp Qcatch_sexp;
extern symbol *Qsetq;
extern sexp Qsetq_sexp;
extern symbol *Qunwind_protect;
extern sexp Qunwind_protect_sexp;
extern symbol *Qif;
extern sexp Qif_sexp;
extern symbol *Qprogv;
extern sexp Qprogv_sexp;
extern symbol *Qprogn;
extern sexp Qprogn_sexp;
extern symbol *Qreturn_from;
extern sexp Qreturn_from_sexp;
extern symbol *Qblock;
extern sexp Qblock_sexp;
extern symbol *Qquote;
extern sexp Qquote_sexp;
extern symbol *Qcomma;
extern sexp Qcomma_sexp;
extern symbol *Qquasiquote;
extern sexp Qquasiquote_sexp;
/*Keyword Symbols*/
extern symbol *Kend;
extern sexp Kend_sexp;
extern symbol *Kstart1;
extern sexp Kstart1_sexp;
extern symbol *Kcount;
extern sexp Kcount_sexp;
extern symbol *Kdocumentation;
extern sexp Kdocumentation_sexp;
extern symbol *Kend1;
extern sexp Kend1_sexp;
extern symbol *Kend2;
extern sexp Kend2_sexp;
extern symbol *Kexport;
extern sexp Kexport_sexp;
extern symbol *Kimport;
extern sexp Kimport_sexp;
extern symbol *Ktest;
extern sexp Ktest_sexp;
extern symbol *Kkey;
extern sexp Kkey_sexp;
extern symbol *Ksize;
extern sexp Ksize_sexp;
extern symbol *Kstart;
extern sexp Kstart_sexp;
extern symbol *Kstart2;
extern sexp Kstart2_sexp;
extern symbol *Kuse;
extern sexp Kuse_sexp;
/*Subroutine Symbols*/
extern symbol *Sarrayp;
extern sexp Sarrayp_sexp;
extern symbol *Sconsp;
extern sexp Sconsp_sexp;
extern symbol *Snumberp;
extern sexp Snumberp_sexp;
extern symbol *Sintegerp;
extern sexp Sintegerp_sexp;
extern symbol *Sfunctionp;
extern sexp Sfunctionp_sexp;
extern symbol *Sstringp;
extern sexp Sstringp_sexp;
extern symbol *Sstreamp;
extern sexp Sstreamp_sexp;
extern symbol *Ssequencep;
extern sexp Ssequencep_sexp;
extern symbol *Srealp;
extern sexp Srealp_sexp;
extern symbol *Sbignump;
extern sexp Sbignump_sexp;
extern symbol *Sbigintp;
extern sexp Sbigintp_sexp;
extern symbol *Sbigfloatp;
extern sexp Sbigfloatp_sexp;
extern symbol *Shashtablep;
extern sexp Shashtablep_sexp;
extern symbol *Smacrop;
extern sexp Smacrop_sexp;
extern symbol *Sspecial_formp;
extern sexp Sspecial_formp_sexp;
extern symbol *Seq;
extern sexp Seq_sexp;
extern symbol *Seql;
extern sexp Seql_sexp;
extern symbol *Sequal;
extern sexp Sequal_sexp;
extern symbol *Sevenp;
extern sexp Sevenp_sexp;
extern symbol *Soddp;
extern sexp Soddp_sexp;
extern symbol *Szerop;
extern sexp Szerop_sexp;
extern symbol *Sne;
extern sexp Sne_sexp;
extern symbol *Smul;
extern sexp Smul_sexp;
extern symbol *Sadd;
extern sexp Sadd_sexp;
extern symbol *Sinc;
extern sexp Sinc_sexp;
extern symbol *Ssub;
extern sexp Ssub_sexp;
extern symbol *Sdec;
extern sexp Sdec_sexp;
extern symbol *Sdiv;
extern sexp Sdiv_sexp;
extern symbol *Slt;
extern sexp Slt_sexp;
extern symbol *Sle;
extern sexp Sle_sexp;
extern symbol *Seq;
extern sexp Seq_sexp;
extern symbol *Sgt;
extern sexp Sgt_sexp;
extern symbol *Sge;
extern sexp Sge_sexp;
extern symbol *Sabs;
extern sexp Sabs_sexp;
extern symbol *Scos;
extern sexp Scos_sexp;
extern symbol *Sexp;
extern sexp Sexp_sexp;
extern symbol *Sexpt;
extern sexp Sexpt_sexp;
extern symbol *Slog;
extern sexp Slog_sexp;
extern symbol *Smod;
extern sexp Smod_sexp;
extern symbol *Spow;
extern sexp Spow_sexp;
extern symbol *Ssin;
extern sexp Ssin_sexp;
extern symbol *Stan;
extern sexp Stan_sexp;
extern symbol *Sassoc;
extern sexp Sassoc_sexp;
extern symbol *Sassq;
extern sexp Sassq_sexp;
extern symbol *Scons;
extern sexp Scons_sexp;
extern symbol *Scopy_tree;
extern sexp Scopy_tree_sexp;
extern symbol *Sdrop;
extern sexp Sdrop_sexp;
extern symbol *Slast;
extern sexp Slast_sexp;
extern symbol *Srand_list;
extern sexp Srand_list_sexp;
extern symbol *Srassoc;
extern sexp Srassoc_sexp;
extern symbol *Srassq;
extern sexp Srassq_sexp;
extern symbol *Spush;
extern sexp Spush_sexp;
extern symbol *Sreduce;
extern sexp Sreduce_sexp;
extern symbol *Snreverse;
extern sexp Snreverse_sexp;
extern symbol *Sreverse;
extern sexp Sreverse_sexp;
extern symbol *Sset_car;
extern sexp Sset_car_sexp;
extern symbol *Sset_cdr;
extern sexp Sset_cdr_sexp;
extern symbol *Ssplit;
extern sexp Ssplit_sexp;
extern symbol *Stake;
extern sexp Stake_sexp;
#endif
