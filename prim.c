/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/* This file is autogenerated do not edit */
/*"standard library" of SciLisp as it were.*/
#include "common.h"
#include "cons.h"
#include "array.h"
#include "print.h"
#include "prim.h"
#include "hash_fn.h"
#include "hash.h"
#include "regex.h"
#include "sequence.h"
#include "lisp_math.h"
//NOTE: Most of these macros are non hygenic and rely on the presense
//of an obarray named ob, used outside of this file at your own risk
#define DEFUN(l_name,c_name,reqargs,optargs,keyargs,restarg,maxargs)    \
  function_args c_name##_args=                                          \
    { .num_req_args=reqargs,.num_opt_args=optargs,.num_keyword_args=keyargs, \
      .has_rest_arg=restarg,.args=0,.max_args=maxargs };                \
  function c_name##_call=                                               \
    { .args=&c_name##_args,.lname=l_name,.cname=#c_name,                \
      .comp = {.f##maxargs=c_name},                                     \
      .type = _compiled_fun };
#define DEFMACRO(l_name,c_name,reqargs,optargs,keyargs,restarg,maxargs) \
  function_args c_name##_args=                                          \
    { .num_req_args=reqargs,.num_opt_args=optargs,.num_keyword_args=keyargs, \
      .has_rest_arg=restarg,.args=0,.max_args=maxargs };                \
  macro c_name##_expander=                                              \
    {.args=&c_name##_args,.lname=l_name,                                \
     .comp = {.f##maxargs=c_name}}
#define MAKE_SYMBOL(l_name,c_name,hash_v)                               \
  symbol c_name ## _sym = {.name=l_name,.val={.tag=_fun,.val={.fun=0}}}; \
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
void hello_world(){
  printf("hello, world!\n");
  return;
}
sexp lisp_inc(sexp num){
  if(!NUMBERP(num)){
    if(SYMBOLP(num)){
      sexp retval=lisp_inc(num.val.var->val);
      if(!ERRORP(retval)){
        num.val.var->val=retval;
        return retval;
      }
    }
    return format_error_sexp("cannot increment a(n) %s",tag_name(num.tag));
    //    return error_sexp("cannot increment something that is not a number");
  } else switch(num.tag){
      case _long:
        return (sexp){.tag=num.tag,.len=num.len,.meta=num.meta,
            .val={.int64=(++num.val.int64)}};
      case _double:
        return (sexp){.tag=num.tag,.len=num.len,.meta=num.meta,
            .val={.real64=(++num.val.real64)}};
    }
}
sexp lisp_inc_ref(sexp sym){
  if(!SYMBOLP(sym)){
    return format_type_error("++!","symbol",sym.tag);
  }
  sexp temp=lisp_inc(sym.val.var->val);
  if(ERRORP(temp)){
    return temp;
  } else {
    sym.val.var->val=temp;
    return temp;
  }
}

sexp lisp_dec(sexp num){
  if(!NUMBERP(num)){
    if(SYMBOLP(num)){
      sexp retval=lisp_inc(num.val.var->val);
      if(!ERRORP(retval)){
        num.val.var->val=retval;
        return retval;
      }
    }
    return format_error_sexp("cannot decrement a(n) %s",tag_name(num.tag));
  } else switch(num.tag){
      case _long:
        num.val.int64-=1;
        return num;
      case _double:
        num.val.real64-=1;
        return num;
    }
}
sexp lisp_dec_ref(sexp sym){
  if(!SYMBOLP(sym)){
    return format_type_error("++!","symbol",sym.tag);
  }
  sexp temp=lisp_dec(sym.val.var->val);
  if(ERRORP(temp)){
    return temp;
  } else {
    sym.val.var->val=temp;
    return temp;
  }
}
sexp lisp_sum(sexp required,sexp values){
  if(!CONSP(values) && !NILP(values)){
    return error_sexp("this shouldn't happen, "
                      "rest arg to sum is not a list or nil");
  } else {
    switch (required.tag){
      case _double:{
        sexp result= required;
        while(CONSP(values)){
          result.val.real64 += getDoubleVal(XCAR(values));
          values = XCDR(values);
        }
        return result;
      }
      case _long:{
        sexp result = required;
        while(CONSP(values)){
          //unsafe, but assume if the first arg is a long they all are
          result.val.int64 += XCAR(values).val.int64;
          values=XCDR(values);
        }
        return result;
      }
      default:
        return error_sexp("args to sum must be numbers");
    }
  }
}
sexp lisp_error(sexp error_message){
  if(!STRINGP(error_message) && !ERRORP(error_message)){
    return format_type_error_opt2("raise-error","string","error",
                                  error_message.tag);
  }
  return error_sexp(error_message.val.cord);
}
sexp lisp_assert(sexp expr){
  if(isTrue(expr)){
    return NIL;
  } else {
    return error_sexp("Assertation faliure");
  }
}
#define make_lisp_assert_eq(name,fun,error_string)                      \
  sexp name(sexp obj1,sexp obj2){                                       \
    if(isTrue(fun(obj1,obj2))){                                         \
      return NIL;                                                       \
    } else {                                                            \
      return format_error_sexp(error_string,print(obj1),print(obj2));   \
    }                                                                   \
  }
make_lisp_assert_eq(lisp_assert_eq,lisp_eq,
                    "Assertation error, %r is not eq to %r")
make_lisp_assert_eq(lisp_assert_equal,lisp_equal,
                    "Assertation error, %r is not equal to %r")
make_lisp_assert_eq(lisp_assert_eql,lisp_eq,
                    "Assertation error, %r is not eql to %r")
make_lisp_assert_eq(lisp_assert_not_eq,lisp_not_eq,
                    "Assertation error, %r is eq to %r")
make_lisp_assert_eq(lisp_assert_not_equal,lisp_not_equal,
                    "Assertation error, %r is equal to %r")
make_lisp_assert_eq(lisp_assert_not_eql,lisp_not_eq,
                    "Assertation error, %r is eql to %r")
#define set_global_vars()                               \
  lisp_stderr_sym.val.val.stream=stderr;                                \
  lisp_stdout_sym.val.val.stream=stdout;                                \
  lisp_stdin_sym.val.val.stream=stdin;                                  \
  mpz_t *mpz_const_1=xmalloc(sizeof(mpz_t));                            \
  mpz_t *mpz_const_0=xmalloc(sizeof(mpz_t));                            \
  mpfr_t *mpfr_const_1=xmalloc(sizeof(mpfr_t));                         \
  mpfr_t *mpfr_const_0=xmalloc(sizeof(mpfr_t));                         \
  mpfr_t *mpfr_const_e=xmalloc(sizeof(mpfr_t));                         \
  mpfr_t *mpfr_const_pi_var=xmalloc(sizeof(mpfr_t));                    \
  mpz_init((*mpz_const_0));                                             \
  mpfr_init((*mpfr_const_0));                                           \
  mpz_init_set_ui((*mpz_const_1),1);                                    \
  mpfr_init_set_ui((*mpfr_const_1),1,MPFR_RNDN);                        \
  mpfr_init((*mpfr_const_e));                                           \
  mpfr_init((*mpfr_const_pi_var));                                      \
  mpfr_exp(*mpfr_const_e,*mpfr_const_1,MPFR_RNDN);                      \
  mpfr_const_pi(*mpfr_const_pi_var,MPFR_RNDN);                          \
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
#define lisp_ans {.tag=-1,.val={.meta=-1}}
DEFUN("+",lisp_add,2,0,0,0,2);
DEFUN("-",lisp_sub,2,0,0,0,2);
DEFUN("*",lisp_mul,2,0,0,0,2);
DEFUN("/",lisp_div,2,0,0,0,2);
DEFUN("<",lisp_lt,2,0,0,0,2);
DEFUN(">",lisp_gt,2,0,0,0,2);
DEFUN(">=",lisp_gte,2,0,0,0,2);
DEFUN("<=",lisp_lte,2,0,0,0,2);
DEFUN("!=",lisp_ne,2,0,0,0,2);
DEFUN("=",lisp_numeq,2,0,0,0,2);
DEFUN("++",lisp_inc,1,0,0,0,1);
DEFUN("--",lisp_dec,1,0,0,0,1);
DEFUN("cons",Cons,2,0,0,0,2);
DEFUN("set-car!",set_car,2,0,0,0,2);
DEFUN("set-cdr!",set_cdr,2,0,0,0,2);
DEFUN("last",lisp_last,1,0,0,0,1);
DEFUN("push!",push_cons,2,0,0,0,2);
DEFUN("pop!",pop_cons,1,0,0,0,1);
DEFUN("mapcar",mapcar,2,0,0,0,2);
DEFUN("reduce",cons_reduce,2,0,0,0,2);
DEFUN("list-qsort",cons_qsort,2,0,0,0,2);
DEFUN("list-mergesort",merge_sort,2,0,0,0,2);
DEFUN("qsort",sequence_qsort,2,0,0,0,2);
DEFUN("length",lisp_length,1,0,0,0,1);
DEFUN("aref",aref,2,0,0,0,2);
DEFUN("array->list",array_to_list,1,0,0,0,1);
DEFUN("expt",lisp_pow,2,0,0,0,2);
DEFUN("sqrt",lisp_sqrt,1,0,0,0,1);
DEFUN("cos",lisp_cos,1,0,0,0,1);
DEFUN("sin",lisp_sin,1,0,0,0,1);
DEFUN("tan",lisp_tan,1,0,0,0,1);
DEFUN("exp",lisp_exp,1,0,0,0,1);
DEFUN("log",lisp_log,1,0,0,0,1);
DEFUN("min",lisp_min,2,0,0,0,2);
DEFUN("max",lisp_max,2,0,0,0,2);
DEFUN("mod",lisp_mod,2,0,0,0,2);
DEFUN("abs",lisp_abs,1,0,0,0,1);
DEFUN("eq",lisp_eq,2,0,0,0,2);
DEFUN("eql",lisp_eql,2,0,0,0,2);
DEFUN("equal",lisp_equal,2,0,0,0,2);
DEFUN("even?",lisp_evenp,1,0,0,0,1);
DEFUN("odd?",lisp_oddp,1,0,0,0,1);
DEFUN("zero?",lisp_zerop,1,0,0,0,1);
DEFUN("nth",lisp_nth,2,0,0,0,2);
DEFUN("assert-equal",lisp_assert_equal,2,0,0,0,2);
DEFUN("list->array",array_from_list,1,0,0,0,1);
DEFUN("raise-error",lisp_error,1,0,0,0,1);
DEFUN("not",lisp_not,1,0,0,0,1);
DEFUN("assert",lisp_assert,1,0,0,0,1);
DEFUN("assert-eq",lisp_assert_eq,2,0,0,0,2);
DEFUN("gensym",lisp_gensym,0,0,0,0,0);
DEFUN("not-eq",lisp_not_eq,2,0,0,0,2);
DEFUN("not-equal",lisp_not_equal,2,0,0,0,2);
DEFUN("assert-not-eq",lisp_assert_not_eq,2,0,0,0,2);
DEFUN("assert-not-equal",lisp_assert_not_equal,2,0,0,0,2);
DEFUN("reverse!",cons_nreverse,1,0,0,0,1);
DEFUN("drop",cons_drop,2,0,0,0,2);
DEFUN("take",cons_take,2,0,0,0,2);
DEFUN("reverse",cons_reverse,1,0,0,0,1);
DEFUN("load",lisp_load,1,0,0,0,1);
DEFUN("array-reverse",array_reverse,1,0,0,0,1);
DEFUN("array-reverse!",array_nreverse,1,0,0,0,1);
DEFUN("get-type",getKeywordType,1,0,0,0,1);
DEFUN("sort",lisp_sort,2,0,0,0,2);
DEFUN("gt",lisp_cmp_gt,2,0,0,0,2);
DEFUN("eq",lisp_cmp_eq,2,0,0,0,2);
DEFUN("lt",lisp_cmp_lt,2,0,0,0,2);
DEFUN("ge",lisp_cmp_ge,2,0,0,0,2);
DEFUN("le",lisp_cmp_le,2,0,0,0,2);
DEFUN("ne",lisp_cmp_ne,2,0,0,0,2);
DEFUN("read",lisp_read,1,0,0,0,1);
DEFUN("read-string",lisp_read_string,1,0,0,0,1);
DEFUN("pprint",lisp_pprint,1,0,0,0,1);
DEFUN("sxhash",lisp_hash_sexp,1,0,0,0,1);
DEFUN("print-to-string",lisp_print_to_string,1,0,0,0,1);
DEFUN("make-string-input-stream",make_string_input_stream,1,0,0,0,1);
DEFUN("iota",lisp_iota,1,4,0,0,5);
DEFUN("array-iota",array_iota,1,3,0,0,4);
DEFUN("array-qsort",array_qsort,2,1,0,0,3);
DEFUN("array-map",array_map,2,0,0,0,2);
DEFUN("array-map!",array_nmap,2,0,0,0,2);
DEFUN("array-reduce",array_reduce,2,1,0,0,3);
DEFUN("make-tree",make_tree,1,1,0,1,3);
DEFUN("rand-array",rand_array,1,1,0,0,2);
DEFUN("rand-list",rand_list,1,1,0,0,2);
DEFUN("typeName",lisp_typeName,1,0,0,0,1);
DEFUN("type-of",typeOf,1,0,0,0,1);
DEFUN("print",lisp_print,1,0,0,0,1);
DEFUN("println",lisp_println,1,0,0,0,1);
DEFUN("eval",lisp_eval,1,1,0,0,2);
DEFUN("fopen",lisp_open,1,1,0,0,2);
DEFUN("fclose",lisp_close,1,0,0,0,1);
DEFUN("fputs",lisp_fputs,2,0,0,0,2);
DEFUN("fprint",lisp_fprint,2,0,0,0,2);
DEFUN("fprintln",lisp_fprintln,2,0,0,0,2);
DEFUN("cat",lisp_cat,1,0,0,1,2);
DEFUN("pwd",lisp_getcwd,0,0,0,0,0);
DEFUN("system",lisp_system,1,0,0,1,2);
DEFUN("ccall",ccall,5,1,0,0,6);
DEFUN("logxor",lisp_xor,2,0,0,0,2);
DEFUN("logand",lisp_logand,2,0,0,0,2);
DEFUN("logor",lisp_logor,2,0,0,0,2);
DEFUN("ash",ash,2,0,0,0,2);
DEFUN("round",lisp_round,1,1,0,0,2);
DEFUN("drand",lisp_randfloat,0,1,0,0,1);
DEFUN("lrand",lisp_randint,0,1,0,0,1);
DEFUN("bigint",lisp_bigint,1,0,0,0,1);
DEFUN("bigfloat",lisp_bigfloat,1,2,0,0,3);
DEFUN("apply",lisp_apply,2,1,0,0,3);
DEFUN("re-compile",lisp_re_compile,1,1,0,0,2);
DEFUN("re-match",lisp_re_match,2,3,0,0,5);
DEFUN("re-subexpr",lisp_get_re_backref,2,0,0,0,2);
DEFUN("list",lisp_list,0,0,0,1,1);
DEFUN("split",cons_split,1,1,0,0,2);
DEFUN("time",lisp_time,0,1,0,0,2);
DEFUN("make-c-ptr",make_c_ptr,1,1,0,0,2);
DEFUN("c-ptr-val",lisp_dereference_c_ptr,1,0,0,0,1);
DEFUN("bigfloat-add",lisp_bigfloat_add,2,0,0,0,2);
DEFUN("bigfloat-sub",lisp_bigfloat_sub,2,0,0,0,2);
DEFUN("bigfloat-mul",lisp_bigfloat_mul,2,0,0,0,2);
DEFUN("bigfloat-div",lisp_bigfloat_div,2,0,0,0,2);
DEFUN("bigfloat-pow",lisp_bigfloat_pow,2,0,0,0,2);
DEFUN("bigfloat-gt",lisp_bigfloat_gt,2,0,0,0,2);
DEFUN("bigfloat-eq",lisp_bigfloat_eq,2,0,0,0,2);
DEFUN("bigfloat-lt",lisp_bigfloat_lt,2,0,0,0,2);
DEFUN("bigfloat-ge",lisp_bigfloat_ge,2,0,0,0,2);
DEFUN("bigfloat-le",lisp_bigfloat_le,2,0,0,0,2);
DEFUN("bigfloat-ne",lisp_bigfloat_ne,2,0,0,0,2);
DEFUN("arrayp",lisp_arrayp,1,0,0,0,1);
DEFUN("consp",lisp_consp,1,0,0,0,1);
DEFUN("numberp",lisp_numberp,1,0,0,0,1);
DEFUN("nilp",lisp_nilp,1,0,0,0,1);
DEFUN("symbolp",lisp_symbolp,1,0,0,0,1);
DEFUN("bigintp",lisp_bigintp,1,0,0,0,1);
DEFUN("bigfloatp",lisp_bigfloatp,1,0,0,0,1);
DEFUN("stringp",lisp_stringp,1,0,0,0,1);
DEFUN("bignump",lisp_bignump,1,0,0,0,1);
DEFUN("errorp",lisp_errorp,1,0,0,0,1);
DEFUN("functionp",lisp_functionp,1,0,0,0,1);
DEFUN("streamp",lisp_streamp,1,0,0,0,1);
DEFUN("add",lisp_add_driver,1,0,0,1,2);
DEFUN("sub",lisp_sub_driver,1,0,0,1,2);
DEFUN("mul",lisp_mul_driver,1,0,0,1,2);
DEFUN("div",lisp_div_driver,1,0,0,1,2);
DEFUN("pow",lisp_pow_driver,1,0,0,1,2);
DEFUN("min",lisp_min_driver,1,0,0,1,2);
DEFUN("max",lisp_max_driver,1,0,0,1,2);
DEFUN("cdr",cdr,1,0,0,0,1);
DEFUN("cddr",cddr,1,0,0,0,1);
DEFUN("cdddr",cdddr,1,0,0,0,1);
DEFUN("cddddr",cddddr,1,0,0,0,1);
DEFUN("cdddar",cdddar,1,0,0,0,1);
DEFUN("cddar",cddar,1,0,0,0,1);
DEFUN("cddadr",cddadr,1,0,0,0,1);
DEFUN("cddaar",cddaar,1,0,0,0,1);
DEFUN("cdar",cdar,1,0,0,0,1);
DEFUN("cdadr",cdadr,1,0,0,0,1);
DEFUN("cdaddr",cdaddr,1,0,0,0,1);
DEFUN("cdadar",cdadar,1,0,0,0,1);
DEFUN("cdaar",cdaar,1,0,0,0,1);
DEFUN("cdaadr",cdaadr,1,0,0,0,1);
DEFUN("cdaaar",cdaaar,1,0,0,0,1);
DEFUN("car",car,1,0,0,0,1);
DEFUN("cadr",cadr,1,0,0,0,1);
DEFUN("caddr",caddr,1,0,0,0,1);
DEFUN("cadddr",cadddr,1,0,0,0,1);
DEFUN("caddar",caddar,1,0,0,0,1);
DEFUN("cadar",cadar,1,0,0,0,1);
DEFUN("cadadr",cadadr,1,0,0,0,1);
DEFUN("cadaar",cadaar,1,0,0,0,1);
DEFUN("caar",caar,1,0,0,0,1);
DEFUN("caadr",caadr,1,0,0,0,1);
DEFUN("caaddr",caaddr,1,0,0,0,1);
DEFUN("caadar",caadar,1,0,0,0,1);
DEFUN("caaar",caaar,1,0,0,0,1);
DEFUN("caaadr",caaadr,1,0,0,0,1);
DEFUN("caaaar",caaaar,1,0,0,0,1);
#undef DEFUN
#define mkTypeCase(type,tag) case tag: return type
sexp typeOfTag(_tag tag){
  switch(tag){
    mkTypeCase(Qint8,_int8);
    mkTypeCase(Qint16,_int16);
    mkTypeCase(Qint32,_int32);
    mkTypeCase(Qint64,_int64);
    mkTypeCase(Quint8,_uint8);
    mkTypeCase(Quint16,_uint16);
    mkTypeCase(Quint32,_uint32);
    mkTypeCase(Quint64,_uint64);
    mkTypeCase(Qerror,_error);
    mkTypeCase(Qreal32,_real32);
    mkTypeCase(Qreal64,_real64);
    mkTypeCase(Qbigint,_bigint);
    mkTypeCase(Qbigfloat,_bigfloat);
    mkTypeCase(Qchar,_char);
    mkTypeCase(Qstring,_string);
    mkTypeCase(Qarray,_array);
    mkTypeCase(Qstream,_stream);
    mkTypeCase(Qlist,_list);
    mkTypeCase(Qfun,_fun);
    mkTypeCase(Qsymbol,_symbol);
    mkTypeCase(Qmacro,_macro);
    mkTypeCase(Qtype,_type);
    mkTypeCase(Qkeyword,_keyword);
    mkTypeCase(Qhashtable,_hashtable);
    mkTypeCase(Qspec,_spec);
    mkTypeCase(Qregex,_regex);
    mkTypeCase(Qnil,_nil);
    mkTypeCase(Qdpair,_dpair);
    mkTypeCase(Qlenv,_lenv);
    mkTypeCase(Qenv,_env);
    mkTypeCase(Qobarray,_obarray);
    mkTypeCase(Qfunargs,_funargs);
    mkTypeCase(Qtrue,_true);
    mkTypeCase(Qfalse,_false);
    mkTypeCase(Quninterned,_uninterned);
    mkTypeCase(Qcons,_cons);
  }
}
sexp typeOf(sexp obj){
  return typeOfTag(obj.tag);
}
MAKE_SYMBOL("+",lisp_add,0xfe1176257ade3f65 );
MAKE_SYMBOL("-",lisp_sub,0x62c6c42523166e74 );
MAKE_SYMBOL("*",lisp_mul,0x21df3c258f79be90 );
MAKE_SYMBOL("/",lisp_div,0xd2eec725627cb9a1 );
MAKE_SYMBOL("<",lisp_lt,0x3b9b0bf59c649ce );
MAKE_SYMBOL(">",lisp_gt,0x39aa2bf59ab8175 );
MAKE_SYMBOL(">=",lisp_gte,0xcb439b255e6c4e30 );
MAKE_SYMBOL("<=",lisp_lte,0x1ad100258bef2d91 );
MAKE_SYMBOL("!=",lisp_ne,0x3b29dbf59c00ad7 );
MAKE_SYMBOL("=",lisp_numeq,0x857a51d9ec2a1750 );
MAKE_SYMBOL("++",lisp_inc,0x45d5e725a43814ee );
MAKE_SYMBOL("--",lisp_dec,0xd2c5b225625990de );
MAKE_SYMBOL("cons",Cons,0x3658069d1acdf568 );
MAKE_SYMBOL("set-car!",set_car,0x1901921ef1b17f2a );
MAKE_SYMBOL("set-cdr!",set_cdr,0x18f77c1ef1a90543 );
MAKE_SYMBOL("last",lisp_last,0x229273cc92c0e97a );
MAKE_SYMBOL("push!",push_cons,0xdeb98901efbd89cb );
MAKE_SYMBOL("pop!",pop_cons,0x69c73c4f15772b00 );
MAKE_SYMBOL("mapcar",mapcar,0xc0ee7f3d3740c6c5 );
MAKE_SYMBOL("reduce",cons_reduce,0x563cfbb23bdcf7d7 );
MAKE_SYMBOL("list-qsort",cons_qsort,0x96dc82ab61416d18 );
MAKE_SYMBOL("list-mergesort",merge_sort,0x8279d9565ce14314 );
MAKE_SYMBOL("qsort",sequence_qsort,0x21160656076b01f8 );
MAKE_SYMBOL("length",lisp_length,0x8f69728654de2182 );
MAKE_SYMBOL("aref",aref,0x89502d843ec2b711 );
MAKE_SYMBOL("array->list",array_to_list,0x3dd759e226ade7d3 );
MAKE_SYMBOL("expt",lisp_pow,0x6d8fca2529cc0cb0 );
MAKE_SYMBOL("sqrt",lisp_sqrt,0x11e3ff1ab3a56bb4 );
MAKE_SYMBOL("cos",lisp_cos,0xef262d257319d109 );
MAKE_SYMBOL("sin",lisp_sin,0x62ef9825233928c4 );
MAKE_SYMBOL("tan",lisp_tan,0x49151f25149e0591 );
MAKE_SYMBOL("exp",lisp_exp,0xdd47de2568d36d09 );
MAKE_SYMBOL("log",lisp_log,0x1a7c0e258ba7055c );
MAKE_SYMBOL("min",lisp_min,0x21b63e258f56bce2 );
MAKE_SYMBOL("max",lisp_max,0x219b28258f3fcfc8 );
MAKE_SYMBOL("mod",lisp_mod,0x21cb28258f68f38a );
MAKE_SYMBOL("abs",lisp_abs,0xfe2687257af0b852 );
MAKE_SYMBOL("eq",lisp_eq,0x3a1a9bf59b1ac08 );
MAKE_SYMBOL("eql",lisp_eql,0xdd5fd42568e7edec );
MAKE_SYMBOL("equal",lisp_equal,0x1f9488d3ca8575ba );
MAKE_SYMBOL("even?",lisp_evenp,0x21a5dd09fa96af36 );
MAKE_SYMBOL("odd?",lisp_oddp,0x47fc16c1d7638f61 );
MAKE_SYMBOL("zero?",lisp_zerop,0x37aa5afe019ffb4c );
MAKE_SYMBOL("nth",lisp_nth,0x8bfc525817d91f4 );
MAKE_SYMBOL("assert-equal",lisp_assert_equal,0x30eeafd7af01ad83 );
MAKE_SYMBOL("list->array",array_from_list,0x140d90bc585ec560 );
MAKE_SYMBOL("raise-error",lisp_error,0xbdaeb1eb8692fd60 );
MAKE_SYMBOL("not",lisp_not,0x878ab2581416323 );
MAKE_SYMBOL("assert",lisp_assert,0x305cd7213a55a78c );
MAKE_SYMBOL("assert-eq",lisp_assert_eq,0x9a99e8fcb03e0ccf );
MAKE_SYMBOL("gensym",lisp_gensym,0xbeed9f84963fc95b );
MAKE_SYMBOL("not-eq",lisp_not_eq,0x5f5180daec9a8756 );
MAKE_SYMBOL("not-equal",lisp_not_equal,0xc6473989d8c9aeac );
MAKE_SYMBOL("assert-not-eq",lisp_assert_not_eq,0xc2b543a9529529c9 );
MAKE_SYMBOL("assert-not-equal",lisp_assert_not_equal,0xb2309949c09d3e59 );
MAKE_SYMBOL("reverse!",cons_nreverse,0xae2278ac6ff9a29 );
MAKE_SYMBOL("drop",cons_drop,0x12b998f81b244bdc );
MAKE_SYMBOL("take",cons_take,0xe6b14274e2e3a606 );
MAKE_SYMBOL("reverse",cons_reverse,0xd880e17f7678aaf5 );
MAKE_SYMBOL("load",lisp_load,0xa7ca35cc4cc7ab16 );
MAKE_SYMBOL("array-reverse",array_reverse,0xe177997970ed3def );
MAKE_SYMBOL("array-reverse!",array_nreverse,0x5d586493f1b1091b );
MAKE_SYMBOL("get-type",getKeywordType,0xa5b9663566ef1c2a );
MAKE_SYMBOL("sort",lisp_sort,0x23b1451abddd4d36 );
MAKE_SYMBOL("gt",lisp_cmp_gt,0xedc90e1a87aa2abc );
MAKE_SYMBOL("eq",lisp_cmp_eq,0xedd0171a87b058b5 );
MAKE_SYMBOL("lt",lisp_cmp_lt,0xedee101a87c94a5f );
MAKE_SYMBOL("ge",lisp_cmp_ge,0xedc8ff1a87aa113f );
MAKE_SYMBOL("le",lisp_cmp_le,0xedee1f1a87c963dc );
MAKE_SYMBOL("ne",lisp_cmp_ne,0xede7131a87c330ca );
MAKE_SYMBOL("read",lisp_read,0x16333b13dcac994e );
MAKE_SYMBOL("read-string",lisp_read_string,0xb5a93464a384d2e6 );
MAKE_SYMBOL("pprint",lisp_pprint,0x87d98d8e66d04385 );
MAKE_SYMBOL("sxhash",lisp_hash_sexp,0x65409b56d1224893 );
MAKE_SYMBOL("print-to-string",lisp_print_to_string,0xc340695be04fd959 );
MAKE_SYMBOL("make-string-input-stream",make_string_input_stream,0xca7ce21f3cd53c3 );
MAKE_SYMBOL("iota",lisp_iota,0xdae23af6073c56d5 );
MAKE_SYMBOL("array-iota",array_iota,0x9da75f7c30743354 );
MAKE_SYMBOL("array-qsort",array_qsort,0xe2ee49217cebc08e );
MAKE_SYMBOL("array-map",array_map,0x66ac752abcbf850b );
MAKE_SYMBOL("array-map!",array_nmap,0xc7f37293aa02428f );
MAKE_SYMBOL("array-reduce",array_reduce,0xf473832a6e5c985d );
MAKE_SYMBOL("make-tree",make_tree,0x6d0643fd5aaef378 );
MAKE_SYMBOL("rand-array",rand_array,0x5200b05e390d5540 );
MAKE_SYMBOL("rand-list",rand_list,0xaad28d384f8f3173 );
MAKE_SYMBOL("typeName",lisp_typeName,0x3fd978c7b7c570e5 );
MAKE_SYMBOL("type-of",typeOf,0x6971003f4c928aa0 );
MAKE_SYMBOL("print",lisp_print,0x4a38f2ac6b3b6ed1 );
MAKE_SYMBOL("println",lisp_println,0x41fd0729bd308d0b );
MAKE_SYMBOL("eval",lisp_eval,0x4be31a91624eed18 );
MAKE_SYMBOL("fopen",lisp_open,0x9ad6eac17558fa36 );
MAKE_SYMBOL("fclose",lisp_close,0x5fd43b5b8cc8b872 );
MAKE_SYMBOL("fputs",lisp_fputs,0x6ea4ff6635a99296 );
MAKE_SYMBOL("fprint",lisp_fprint,0xde93ad9559a71957 );
MAKE_SYMBOL("fprintln",lisp_fprintln,0xd37b5eb8b9983cdd );
MAKE_SYMBOL("cat",lisp_cat,0xef5542257341657a );
MAKE_SYMBOL("pwd",lisp_getcwd,0x3c07edcc2db7ada8 );
MAKE_SYMBOL("system",lisp_system,0x261ca996c6e4a97 );
MAKE_SYMBOL("ccall",ccall,0x9fdbba28c51a5416 );
MAKE_SYMBOL("logxor",lisp_xor,0xb1cc27255025b0d7 );
MAKE_SYMBOL("logand",lisp_logand,0xbe33926eae30db1d );
MAKE_SYMBOL("logor",lisp_logor,0xf6602a2681c26321 );
MAKE_SYMBOL("ash",ash,0xe759a1190572cddf );
MAKE_SYMBOL("round",lisp_round,0xe7863c8b43d1dc8e );
MAKE_SYMBOL("drand",lisp_randfloat,0xd7a3e2a6c9ca3a5d );
MAKE_SYMBOL("lrand",lisp_randint,0xcffb580fd3c7c16 );
MAKE_SYMBOL("bigint",lisp_bigint,0x6a031ae6deb3b2f9 );
MAKE_SYMBOL("bigfloat",lisp_bigfloat,0xbf028d8fe03cb0c2 );
MAKE_SYMBOL("apply",lisp_apply,0x35de5e4bd0bba8ae );
MAKE_SYMBOL("re-compile",lisp_re_compile,0xa2764a4bb059a9d9 );
MAKE_SYMBOL("re-match",lisp_re_match,0x80d577d4c3b37b05 );
MAKE_SYMBOL("re-subexpr",lisp_get_re_backref,0x8a116ddc4ad390f1 );
MAKE_SYMBOL("list",lisp_list,0xddb41bcc6bde9f82 );
MAKE_SYMBOL("split",cons_split,0xd2efdcf6197c2073 );
MAKE_SYMBOL("time",lisp_time,0x11f528022f8d3a4f );
MAKE_SYMBOL("make-c-ptr",make_c_ptr,0xc453eec8f4f7885c );
MAKE_SYMBOL("c-ptr-val",lisp_dereference_c_ptr,0xffd9c9f57a495d99 );
MAKE_SYMBOL("bigfloat-add",lisp_bigfloat_add,0x5ce208f741cde6d2 );
MAKE_SYMBOL("bigfloat-sub",lisp_bigfloat_sub,0xd42552f784c9600b );
MAKE_SYMBOL("bigfloat-mul",lisp_bigfloat_mul,0x7d86d2f753b9f1e7 );
MAKE_SYMBOL("bigfloat-div",lisp_bigfloat_div,0x402c9bf730c57e72 );
MAKE_SYMBOL("bigfloat-pow",lisp_bigfloat_pow,0xed99e0f7930df3bb );
MAKE_SYMBOL("bigfloat-gt",lisp_bigfloat_gt,0xc379321cd0ffa27c );
MAKE_SYMBOL("bigfloat-eq",lisp_bigfloat_eq,0xc3803b1cd105d075 );
MAKE_SYMBOL("bigfloat-lt",lisp_bigfloat_lt,0xc39f341cd120751f );
MAKE_SYMBOL("bigfloat-ge",lisp_bigfloat_ge,0xc379231cd0ff88ff );
MAKE_SYMBOL("bigfloat-le",lisp_bigfloat_le,0xc39f431cd1208e9c );
MAKE_SYMBOL("bigfloat-ne",lisp_bigfloat_ne,0xc398371cd11a5b8a );
MAKE_SYMBOL("arrayp",lisp_arrayp,0x7d808d48023e869d );
MAKE_SYMBOL("consp",lisp_consp,0x13ff0f42de4882d1 );
MAKE_SYMBOL("numberp",lisp_numberp,0x6066daab1874f889 );
MAKE_SYMBOL("nilp",lisp_nilp,0x70c241ba833a3987 );
MAKE_SYMBOL("symbolp",lisp_symbolp,0x82929922881de33a );
MAKE_SYMBOL("bigintp",lisp_bigintp,0xd6f93f4c6b585ecb );
MAKE_SYMBOL("bigfloatp",lisp_bigfloatp,0xce073d7a07203e76 );
MAKE_SYMBOL("stringp",lisp_stringp,0x4820d35f103b77f5 );
MAKE_SYMBOL("bignump",lisp_bignump,0x5675ae7650a07788 );
MAKE_SYMBOL("errorp",lisp_errorp,0xe2d56335abc40230 );
MAKE_SYMBOL("functionp",lisp_functionp,0xcfa48bf2151e142e );
MAKE_SYMBOL("streamp",lisp_streamp,0x83fb04c416f51082 );
MAKE_SYMBOL("add",lisp_add_driver,0xe5ea91f91fee8a30 );
MAKE_SYMBOL("sub",lisp_sub_driver,0x8af1354347afa423 );
MAKE_SYMBOL("mul",lisp_mul_driver,0x944bcddd9417d667 );
MAKE_SYMBOL("div",lisp_div_driver,0xaed57600bbae1764 );
MAKE_SYMBOL("pow",lisp_pow_driver,0x4b85ff031bae29c7 );
MAKE_SYMBOL("min",lisp_min_driver,0xfcdada16e882e66d );
MAKE_SYMBOL("max",lisp_max_driver,0x2afc6e3c3a7272cf );
MAKE_SYMBOL("cdr",cdr,0xf5ecf3190cecd5b0 );
MAKE_SYMBOL("cddr",cddr,0xce54d590f6525240 );
MAKE_SYMBOL("cdddr",cdddr,0xecc417528e219670 );
MAKE_SYMBOL("cddddr",cddddr,0x72a7a34782f4ff00 );
MAKE_SYMBOL("cdddar",cdddar,0x729db54782ecc911 );
MAKE_SYMBOL("cddar",cddar,0xecba29528e196081 );
MAKE_SYMBOL("cddadr",cddadr,0x5985324774f6a6bf );
MAKE_SYMBOL("cddaar",cddaar,0x5974384774e83dbe );
MAKE_SYMBOL("cdar",cdar,0xce4ae790f64a1c51 );
MAKE_SYMBOL("cdadr",cdadr,0xd3a0a65280218b2f );
MAKE_SYMBOL("cdaddr",cdaddr,0xbb48f32fb8cc1351 );
MAKE_SYMBOL("cdadar",cdadar,0xbb52e12fb8d44940 );
MAKE_SYMBOL("cdaar",cdaar,0xd38fac528013222e );
MAKE_SYMBOL("cdaadr",cdaadr,0x90141c2fa05a2362 );
MAKE_SYMBOL("cdaaar",cdaaar,0x900a262fa051dfdb );
MAKE_SYMBOL("car",car,0xf5e305190ce49fc1 );
MAKE_SYMBOL("cadr",cadr,0xb5316490e85246ff );
MAKE_SYMBOL("caddr",caddr,0x3564673ac3f6f7c1 );
MAKE_SYMBOL("cadddr",cadddr,0xb06323dafc7dceff );
MAKE_SYMBOL("caddar",caddar,0xb05229dafc6f65fe );
MAKE_SYMBOL("cadar",cadar,0x356e553ac3ff2db0 );
MAKE_SYMBOL("cadadr",cadadr,0xc98694db0a7dda40 );
MAKE_SYMBOL("cadaar",cadaar,0xc97ca6db0a75a451 );
MAKE_SYMBOL("caar",caar,0xb5206a90e843ddfe );
MAKE_SYMBOL("caadr",caadr,0xa30903aab86bad2 );
MAKE_SYMBOL("caaddr",caaddr,0xd761b3b1760c0446 );
MAKE_SYMBOL("caadar",caadar,0xd772cdb1761aa3a7 );
MAKE_SYMBOL("caaar",caaar,0xa259a3aab7cc44b );
MAKE_SYMBOL("caaadr",caaadr,0xbaf452b1654165ed );
MAKE_SYMBOL("caaaar",caaaar,0xbae350b16532ef54 );
MAKE_GLOBAL("Meps",lisp_mach_eps,1);
MAKE_GLOBAL("pi",lisp_pi,1);
MAKE_GLOBAL("e",lisp_euler,1);
MAKE_GLOBAL("max-int64",lisp_max_long,1);
MAKE_GLOBAL("nil",lisp_NIL,1);
MAKE_GLOBAL("#t",lisp_LISP_TRUE,1);
MAKE_GLOBAL("#f",lisp_LISP_FALSE,1);
MAKE_GLOBAL("stdin",lisp_stdin,0);
MAKE_GLOBAL("stdout",lisp_stdout,0);
MAKE_GLOBAL("stderr",lisp_stderr,0);
MAKE_GLOBAL("double-0",lisp_double_0,1);
MAKE_GLOBAL("double-1",lisp_double_1,1);
MAKE_GLOBAL("long-0",lisp_long_0,1);
MAKE_GLOBAL("long-1",lisp_long_1,1);
MAKE_GLOBAL("ans",lisp_ans,0);
MAKE_GLOBAL("bigfloat-0",lisp_bigfloat_0,1);
MAKE_GLOBAL("bigfloat-1",lisp_bigfloat_1,1);
MAKE_GLOBAL("bigfloat-e",lisp_bigfloat_e,1);
MAKE_GLOBAL("bigfloat-pi",lisp_bigfloat_pi,1);
MAKE_GLOBAL("bigint-0",lisp_bigint_0,1);
MAKE_GLOBAL("bigint-1",lisp_bigint_1,1);
MAKE_TYPE(int8,_int8);
MAKE_TYPE(int16,_int16);
MAKE_TYPE(int32,_int32);
MAKE_TYPE(int64,_int64);
MAKE_TYPE(uint8,_uint8);
MAKE_TYPE(uint16,_uint16);
MAKE_TYPE(uint32,_uint32);
MAKE_TYPE(uint64,_uint64);
MAKE_TYPE(error,_error);
MAKE_TYPE(real32,_real32);
MAKE_TYPE(real64,_real64);
MAKE_TYPE(bigint,_bigint);
MAKE_TYPE(bigfloat,_bigfloat);
MAKE_TYPE(char,_char);
MAKE_TYPE(string,_string);
MAKE_TYPE(array,_array);
MAKE_TYPE(stream,_stream);
MAKE_TYPE(list,_list);
MAKE_TYPE(fun,_fun);
MAKE_TYPE(symbol,_symbol);
MAKE_TYPE(macro,_macro);
MAKE_TYPE(type,_type);
MAKE_TYPE(keyword,_keyword);
MAKE_TYPE(hashtable,_hashtable);
MAKE_TYPE(spec,_spec);
MAKE_TYPE(regex,_regex);
MAKE_TYPE(nil,_nil);
MAKE_TYPE(dpair,_dpair);
MAKE_TYPE(lenv,_lenv);
MAKE_TYPE(env,_env);
MAKE_TYPE(obarray,_obarray);
MAKE_TYPE(funargs,_funargs);
MAKE_TYPE(true,_true);
MAKE_TYPE(false,_false);
MAKE_TYPE(uninterned,_uninterned);
MAKE_TYPE(cons,_cons);
mpz_t *lisp_mpz_1,*lisp_mpz_0;
mpfr_t *lisp_mpfr_1,*lisp_mpfr_0;
static void initPrimsObarray(obarray *ob,env* ob_env);
void initPrims(){
if(initPrimsFlag){
initPrimsFlag=0;
} else {
return;
}
globalObarray=xmalloc(sizeof(obarray));
obarray_entry** global_buckets=xmalloc(128*sizeof(obarray_entry*));
*globalObarray=(obarray)
{.buckets=global_buckets,.size=128,.used=0,.entries=0,.capacity=0.0,
                .capacity_inc=(1.0/(128*10)),.gthresh=0.75,.gfactor=2,
                .is_weak_hash=0,.hash_fn=fnv_hash};
keywordObarray=xmalloc(sizeof(obarray));
obarray_entry** keyword_buckets=xmalloc(128*sizeof(obarray_entry*));
*keywordObarray=(obarray)
{.buckets=keyword_buckets,.size=128,.used=0,.entries=0,.capacity=0.0,
                .capacity_inc=(1.0/(128*10)),.gthresh=0.75,.gfactor=2,
                .is_weak_hash=0,.hash_fn=fnv_hash};
globalObarrayEnv=xmalloc(sizeof(obarray_env));
keywordObarrayEnv=xmalloc(sizeof(obarray_env));
topLevelEnv=xmalloc(sizeof(env));
globalObarrayEnv->enclosing=keywordObarrayEnv->enclosing=0;
globalObarrayEnv->head=globalObarray;
keywordObarrayEnv->head=keywordObarray;
initPrimsObarray(globalObarray,(env*)globalObarrayEnv);
*topLevelEnv=(env){.enclosing=globalObarrayEnv->enclosing,
.head={.ob=globalObarrayEnv->head},.tag=_obEnv};
mpfr_set_default_prec(256);
mp_set_memory_functions(GC_MALLOC_1,GC_REALLOC_3,GC_FREE_2);
set_global_vars();
srand48(time(NULL));
INIT_SYNONYM(lisp_consp,"cons?",1);
}
static void initPrimsObarray(obarray *ob,env* ob_env){

INIT_SYMBOL(lisp_add);
INIT_SYMBOL(lisp_sub);
INIT_SYMBOL(lisp_mul);
INIT_SYMBOL(lisp_div);
INIT_SYMBOL(lisp_lt);
INIT_SYMBOL(lisp_gt);
INIT_SYMBOL(lisp_gte);
INIT_SYMBOL(lisp_lte);
INIT_SYMBOL(lisp_ne);
INIT_SYMBOL(lisp_numeq);
INIT_SYMBOL(lisp_inc);
INIT_SYMBOL(lisp_dec);
INIT_SYMBOL(Cons);
INIT_SYMBOL(set_car);
INIT_SYMBOL(set_cdr);
INIT_SYMBOL(lisp_last);
INIT_SYMBOL(push_cons);
INIT_SYMBOL(pop_cons);
INIT_SYMBOL(mapcar);
INIT_SYMBOL(cons_reduce);
INIT_SYMBOL(cons_qsort);
INIT_SYMBOL(merge_sort);
INIT_SYMBOL(sequence_qsort);
INIT_SYMBOL(lisp_length);
INIT_SYMBOL(aref);
INIT_SYMBOL(array_to_list);
INIT_SYMBOL(lisp_pow);
INIT_SYMBOL(lisp_sqrt);
INIT_SYMBOL(lisp_cos);
INIT_SYMBOL(lisp_sin);
INIT_SYMBOL(lisp_tan);
INIT_SYMBOL(lisp_exp);
INIT_SYMBOL(lisp_log);
INIT_SYMBOL(lisp_min);
INIT_SYMBOL(lisp_max);
INIT_SYMBOL(lisp_mod);
INIT_SYMBOL(lisp_abs);
INIT_SYMBOL(lisp_eq);
INIT_SYMBOL(lisp_eql);
INIT_SYMBOL(lisp_equal);
INIT_SYMBOL(lisp_evenp);
INIT_SYMBOL(lisp_oddp);
INIT_SYMBOL(lisp_zerop);
INIT_SYMBOL(lisp_nth);
INIT_SYMBOL(lisp_assert_equal);
INIT_SYMBOL(array_from_list);
INIT_SYMBOL(lisp_error);
INIT_SYMBOL(lisp_not);
INIT_SYMBOL(lisp_assert);
INIT_SYMBOL(lisp_assert_eq);
INIT_SYMBOL(lisp_gensym);
INIT_SYMBOL(lisp_not_eq);
INIT_SYMBOL(lisp_not_equal);
INIT_SYMBOL(lisp_assert_not_eq);
INIT_SYMBOL(lisp_assert_not_equal);
INIT_SYMBOL(cons_nreverse);
INIT_SYMBOL(cons_drop);
INIT_SYMBOL(cons_take);
INIT_SYMBOL(cons_reverse);
INIT_SYMBOL(lisp_load);
INIT_SYMBOL(array_reverse);
INIT_SYMBOL(array_nreverse);
INIT_SYMBOL(getKeywordType);
INIT_SYMBOL(lisp_sort);
INIT_SYMBOL(lisp_cmp_gt);
INIT_SYMBOL(lisp_cmp_eq);
INIT_SYMBOL(lisp_cmp_lt);
INIT_SYMBOL(lisp_cmp_ge);
INIT_SYMBOL(lisp_cmp_le);
INIT_SYMBOL(lisp_cmp_ne);
INIT_SYMBOL(lisp_read);
INIT_SYMBOL(lisp_read_string);
INIT_SYMBOL(lisp_pprint);
INIT_SYMBOL(lisp_hash_sexp);
INIT_SYMBOL(lisp_print_to_string);
INIT_SYMBOL(make_string_input_stream);
INIT_SYMBOL(lisp_iota);
INIT_SYMBOL(array_iota);
INIT_SYMBOL(array_qsort);
INIT_SYMBOL(array_map);
INIT_SYMBOL(array_nmap);
INIT_SYMBOL(array_reduce);
INIT_SYMBOL(make_tree);
INIT_SYMBOL(rand_array);
INIT_SYMBOL(rand_list);
INIT_SYMBOL(lisp_typeName);
INIT_SYMBOL(typeOf);
INIT_SYMBOL(lisp_print);
INIT_SYMBOL(lisp_println);
INIT_SYMBOL(lisp_eval);
INIT_SYMBOL(lisp_open);
INIT_SYMBOL(lisp_close);
INIT_SYMBOL(lisp_fputs);
INIT_SYMBOL(lisp_fprint);
INIT_SYMBOL(lisp_fprintln);
INIT_SYMBOL(lisp_cat);
INIT_SYMBOL(lisp_getcwd);
INIT_SYMBOL(lisp_system);
INIT_SYMBOL(ccall);
INIT_SYMBOL(lisp_xor);
INIT_SYMBOL(lisp_logand);
INIT_SYMBOL(lisp_logor);
INIT_SYMBOL(ash);
INIT_SYMBOL(lisp_round);
INIT_SYMBOL(lisp_randfloat);
INIT_SYMBOL(lisp_randint);
INIT_SYMBOL(lisp_bigint);
INIT_SYMBOL(lisp_bigfloat);
INIT_SYMBOL(lisp_apply);
INIT_SYMBOL(lisp_re_compile);
INIT_SYMBOL(lisp_re_match);
INIT_SYMBOL(lisp_get_re_backref);
INIT_SYMBOL(lisp_list);
INIT_SYMBOL(cons_split);
INIT_SYMBOL(lisp_time);
INIT_SYMBOL(make_c_ptr);
INIT_SYMBOL(lisp_dereference_c_ptr);
INIT_SYMBOL(lisp_bigfloat_add);
INIT_SYMBOL(lisp_bigfloat_sub);
INIT_SYMBOL(lisp_bigfloat_mul);
INIT_SYMBOL(lisp_bigfloat_div);
INIT_SYMBOL(lisp_bigfloat_pow);
INIT_SYMBOL(lisp_bigfloat_gt);
INIT_SYMBOL(lisp_bigfloat_eq);
INIT_SYMBOL(lisp_bigfloat_lt);
INIT_SYMBOL(lisp_bigfloat_ge);
INIT_SYMBOL(lisp_bigfloat_le);
INIT_SYMBOL(lisp_bigfloat_ne);
INIT_SYMBOL(lisp_arrayp);
INIT_SYMBOL(lisp_consp);
INIT_SYMBOL(lisp_numberp);
INIT_SYMBOL(lisp_nilp);
INIT_SYMBOL(lisp_symbolp);
INIT_SYMBOL(lisp_bigintp);
INIT_SYMBOL(lisp_bigfloatp);
INIT_SYMBOL(lisp_stringp);
INIT_SYMBOL(lisp_bignump);
INIT_SYMBOL(lisp_errorp);
INIT_SYMBOL(lisp_functionp);
INIT_SYMBOL(lisp_streamp);
INIT_SYMBOL(lisp_add_driver);
INIT_SYMBOL(lisp_sub_driver);
INIT_SYMBOL(lisp_mul_driver);
INIT_SYMBOL(lisp_div_driver);
INIT_SYMBOL(lisp_pow_driver);
INIT_SYMBOL(lisp_min_driver);
INIT_SYMBOL(lisp_max_driver);
INIT_SYMBOL(cdr);
INIT_SYMBOL(cddr);
INIT_SYMBOL(cdddr);
INIT_SYMBOL(cddddr);
INIT_SYMBOL(cdddar);
INIT_SYMBOL(cddar);
INIT_SYMBOL(cddadr);
INIT_SYMBOL(cddaar);
INIT_SYMBOL(cdar);
INIT_SYMBOL(cdadr);
INIT_SYMBOL(cdaddr);
INIT_SYMBOL(cdadar);
INIT_SYMBOL(cdaar);
INIT_SYMBOL(cdaadr);
INIT_SYMBOL(cdaaar);
INIT_SYMBOL(car);
INIT_SYMBOL(cadr);
INIT_SYMBOL(caddr);
INIT_SYMBOL(cadddr);
INIT_SYMBOL(caddar);
INIT_SYMBOL(cadar);
INIT_SYMBOL(cadadr);
INIT_SYMBOL(cadaar);
INIT_SYMBOL(caar);
INIT_SYMBOL(caadr);
INIT_SYMBOL(caaddr);
INIT_SYMBOL(caadar);
INIT_SYMBOL(caaar);
INIT_SYMBOL(caaadr);
INIT_SYMBOL(caaaar);
INIT_GLOBAL(lisp_mach_eps);
INIT_GLOBAL(lisp_pi);
INIT_GLOBAL(lisp_euler);
INIT_GLOBAL(lisp_max_long);
INIT_GLOBAL(lisp_NIL);
INIT_GLOBAL(lisp_LISP_TRUE);
INIT_GLOBAL(lisp_LISP_FALSE);
INIT_GLOBAL(lisp_stdin);
INIT_GLOBAL(lisp_stdout);
INIT_GLOBAL(lisp_stderr);
INIT_GLOBAL(lisp_double_0);
INIT_GLOBAL(lisp_double_1);
INIT_GLOBAL(lisp_long_0);
INIT_GLOBAL(lisp_long_1);
INIT_GLOBAL(lisp_ans);
INIT_GLOBAL(lisp_bigfloat_0);
INIT_GLOBAL(lisp_bigfloat_1);
INIT_GLOBAL(lisp_bigfloat_e);
INIT_GLOBAL(lisp_bigfloat_pi);
INIT_GLOBAL(lisp_bigint_0);
INIT_GLOBAL(lisp_bigint_1);
INIT_GLOBAL(int8);
INIT_GLOBAL(int16);
INIT_GLOBAL(int32);
INIT_GLOBAL(int64);
INIT_GLOBAL(uint8);
INIT_GLOBAL(uint16);
INIT_GLOBAL(uint32);
INIT_GLOBAL(uint64);
INIT_GLOBAL(error);
INIT_GLOBAL(real32);
INIT_GLOBAL(real64);
INIT_GLOBAL(bigint);
INIT_GLOBAL(bigfloat);
INIT_GLOBAL(char);
INIT_GLOBAL(string);
INIT_GLOBAL(array);
INIT_GLOBAL(stream);
INIT_GLOBAL(list);
INIT_GLOBAL(fun);
INIT_GLOBAL(symbol);
INIT_GLOBAL(macro);
INIT_GLOBAL(type);
INIT_GLOBAL(keyword);
INIT_GLOBAL(hashtable);
INIT_GLOBAL(spec);
INIT_GLOBAL(regex);
INIT_GLOBAL(nil);
INIT_GLOBAL(dpair);
INIT_GLOBAL(lenv);
INIT_GLOBAL(env);
INIT_GLOBAL(obarray);
INIT_GLOBAL(funargs);
INIT_GLOBAL(true);
INIT_GLOBAL(false);
INIT_GLOBAL(uninterned);
INIT_GLOBAL(cons);
}
