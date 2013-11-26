/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/* This file is autogenerated do not edit */
/*"standard library" of SciLisp as it were.
 * used to all be in prim.h, split into prim.h as declarations and
 * the initPrims macro (which actually reads all these funcitons into
 * the global symbol table)
 * this file mostly defines basic binary operations, for more list functions
 * see cons.c/h and for array functions see array.c/h*/
#include "common.h"
#include "cons.h"
#include "array.h"
#include "print.h"
#include "prim.h"
#include "hash_fn.h"
#include "regex.h"
#define binop_to_fun(op,fun_name)                                       \
  sexp fun_name(sexp x,sexp y){                                         \
    if((x.tag==y.tag)==_long){                                          \
      return                                                            \
        (sexp){.tag=_long,.val={.int64 = (x.val.int64 op y.val.int64)}}; \
    } else if(NUMBERP(x)&&NUMBERP(y)){                                  \
      register double xx=getDoubleVal(x);                               \
      register double yy=getDoubleVal(y);                               \
      return (sexp){.tag=_double,.val={.real64=(xx op yy)}};            \
    } else {                                                            \
      return format_type_error2(#fun_name,"number",x.tag,"number",y.tag);\
    }                                                                   \
  }
#define mkLisp_cmp(op,cname)                                    \
  sexp cname(sexp x,sexp y){                                    \
    if((x.tag == y.tag)==_long){                                \
      return (x.val.int64 op y.val.int64 ? LISP_TRUE : LISP_FALSE);    \
    } else if(NUMBERP(x)&&NUMBERP(y)){                          \
      register double xx=getDoubleVal(x);                       \
      register double yy=getDoubleVal(y);                       \
      return (xx op yy ? LISP_TRUE : LISP_FALSE);                      \
    } else {                                                    \
      return format_type_error2(#cname,"number",x.tag,"number",y.tag); \
    }                                                           \
  }
//ignore tags, allow logical operations on doubles(or anything else)
//be careful about this
#define lop_to_fun(op,fun_name)                                         \
  sexp fun_name(sexp x,sexp y){                                         \
    return (sexp){.tag=_long,.val={.int64=(x.val.int64 op y.val.int64)}}; \
  }
//NOTE: Most of these macros are non hygenic and rely on the presense
//of an obarray named ob, used outside of this file at your own risk
#define DEFUN(l_name,c_name,reqargs,optargs,keyargs,restarg,maxargs)  \
  function_args c_name##_args=                                            \
    { .num_req_args=reqargs,.num_opt_args=optargs,.num_keyword_args=keyargs, \
      .has_rest_arg=restarg,.args=0,.max_args=maxargs };      \
  function c_name##_call=                                             \
    { .args=&c_name##_args,.lname=l_name,.cname=#c_name,                   \
      .comp = {.f##maxargs=c_name},            \
      .type = _compiled_fun };
#define DEFMACRO(l_name,c_name,reqargs,optargs,keyargs,restarg,maxargs)  \
  function_args c_name##_args=                                            \
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
  obarray_entry c_name ##_ob_entry={.prev=0,.next=0,.ob_symbol=0,       \
                                    .hashv=0}
#define INIT_SYMBOL(c_name)                                    \
  c_name##_ptr=&c_name##_sym;                                  \
  c_name##_ptr->val.val.fun=&c_name##_call;                     \
  c_name##_ob_entry.ob_symbol=c_name##_ptr;                     \
  prim_obarray_add_entry(ob,c_name##_ptr,&c_name##_ob_entry)
#define INIT_MACRO_SYMBOL(c_name)                                    \
  c_name##_ptr=&c_name##_sym;                                        \
  c_name##_ptr->val.val.fun=&c_name##_expander;                      \
  c_name##_ob_entry.ob_symbol=c_name##_ptr;                          \
  prim_obarray_add_entry(ob,c_name##_ptr,&c_name##_ob_entry)
#define INIT_GLOBAL(c_name)                                      \
  c_name##_ptr=&c_name##_sym;                                    \
  c_name##_ob_entry.ob_symbol=c_name##_ptr;                      \
  prim_obarray_add_entry(ob,c_name##_ptr,&c_name##_ob_entry)
#define INIT_SYNONYM(c_name,l_name,gensym_counter)                      \
  symref c_name##_ptr_syn ##gensym_counter= xmalloc(sizeof(symbol));    \
  * c_name##_ptr_syn ## gensym_counter = *c_name ##_ptr;                \
  c_name##_ptr_syn ## gensym_counter -> name = l_name;                  \
  obarray_entry *c_name##_ob_entry##gensym_counter=                     \
    xmalloc(sizeof(obarray_entry));                                     \
  c_name##_ob_entry##gensym_counter->ob_symbol=c_name##_ptr_syn##gensym_counter;        \
  prim_obarray_add_entry(globalObarray,c_name##_ptr_syn ## gensym_counter,         \
                         c_name##_ob_entry##gensym_counter)
  
#define MK_PREDICATE(lname,test)                \
  sexp lisp_##lname (sexp obj){                 \
    if(obj.tag == test){                        \
      return LISP_TRUE;                         \
    } else {                                    \
      return LISP_FALSE;                        \
    }                                           \
  }
#define MK_PREDICATE2(lname,test,test2)         \
  sexp lisp_##lname (sexp obj){                 \
    if(obj.tag == test || obj.tag == test2){    \
      return LISP_TRUE;                         \
    } else {                                    \
      return LISP_FALSE;                        \
    }                                           \
  }
#define MK_PREDICATE3(lname,test,test2,test3)                           \
  sexp lisp_##lname (sexp obj){                                         \
    if(obj.tag == test || obj.tag == test2 || obj.tag == test3){        \
      return LISP_TRUE;                                                 \
    } else {                                                            \
      return LISP_FALSE;                                                \
    }                                                                   \
  }
#define MK_PREDICATE4(lname,test,test2,test3,test4)                     \
  sexp lisp_##lname (sexp obj){                                         \
    if(obj.tag == test || obj.tag == test2 || obj.tag == test3 ||       \
       obj.tag == test4){                                               \
      return LISP_TRUE;                                                 \
    } else {                                                            \
      return LISP_FALSE;                                                \
    }                                                                   \
  }

#define mkMathFun1(cname,lispname)                                      \
  sexp lispname (sexp obj){                                             \
    if(!NUMBERP(obj)){return error_sexp("type error in "#lispname);}    \
    return (sexp){.tag=_double,.val={.real64 = cname(getDoubleVal(obj))}}; \
  }
#define mkMathFun2(cname,lispname)                              \
  sexp lispname(sexp x,sexp y){                                 \
    if(!NUMBERP(x)||!NUMBERP(y))                                \
      {return error_sexp("type error in "#lispname);}           \
    register double xx=getDoubleVal(x);                         \
    register double yy=getDoubleVal(y);                         \
    return (sexp){.tag=_double,.val={.real64 = cname(xx,yy)}};  \
  }
//create c functions for primitives
//arithmatic primitives
binop_to_fun(+,lisp_add);
binop_to_fun(-,lisp_sub);
binop_to_fun(*,lisp_mul);
binop_to_fun(/,lisp_div);
//bitwise primitives(need to add !)
lop_to_fun(^,lisp_xor);
lop_to_fun(>>,lisp_rshift);
lop_to_fun(<<,lisp_lshift);
lop_to_fun(&,lisp_logand);
lop_to_fun(|,lisp_logor);
//compairson primitives
mkLisp_cmp(>,lisp_gt);
mkLisp_cmp(<,lisp_lt);
mkLisp_cmp(>=,lisp_gte);
mkLisp_cmp(<=,lisp_lte);
mkLisp_cmp(!=,lisp_ne);
mkLisp_cmp(==,lisp_numeq);
//math primitives
mkMathFun2(pow,lisp_pow);
mkMathFun1(sqrt,lisp_sqrt);
mkMathFun1(cos,lisp_cos);
mkMathFun1(sin,lisp_sin);
mkMathFun1(tan,lisp_tan);
mkMathFun1(exp,lisp_exp);
mkMathFun1(log,lisp_log);
/*sexp lisp_abs(sexp x){
  if(x.tag==_long){return
      (sexp){.tag=_long,.val={.int64 = (labs(x.val.int64))}};
  } else if(x.tag == _double){
    return (sexp){.tag=_double,.val={.real64=fabs(x.val.real64)}};
  } else {
    return error_sexp("Argument to Abs must be a number");
  }
  }*/
sexp lisp_mod(sexp x,sexp y){
  if((x.tag==y.tag)==_long){
    return (sexp){.tag=_long,.val={.int64 = (x.val.int64 % y.val.int64)}};
  } else if(NUMBERP(x) && NUMBERP(y)){
    register double xx=getDoubleVal(x);
    register double yy=getDoubleVal(y);
    return (sexp){.tag=_double,.val={.real64=fmod(xx,yy)}};
  } else {
    return error_sexp("Arguments to mod must be numbers");
  }
}
sexp ash(sexp x,sexp y){
  if(y.tag != _long || x.tag != _long){
    return error_sexp("arguments to ash must be integers");
  } else if(y.val.int64>=0){
    return lisp_rshift(x,y);
  } else{
    return lisp_lshift(x,(sexp){.tag=_long,.val={.int64 = (labs(y.val.int64))}});
  }
}
sexp lisp_randint(){
  return (sexp){.tag=_long,.val={.int64=mrand48()}};
}
sexp lisp_randfloat(sexp scale){
  double retval;
  if(scale.tag != _nil){
    retval=drand48()*getDoubleVal(scale);
  } else {
    retval = drand48();
  }
  return (sexp){.tag=_double,.val={.real64=retval}};
}
sexp lisp_eval(sexp obj,sexp env){
  return eval(obj,topLevelEnv);
}
sexp lisp_length(sexp obj){
  if(obj.len > 0){
    return (sexp){.tag=_long,.val={.int64 = obj.len}};
  } else if (CONSP(obj)){
    //    HERE();
    return cons_length(obj);
  } else {
    return error_sexp("object does not have a meaningful length field");
  }
}
sexp lisp_round(sexp float_num,sexp mode){
  double double_val=getDoubleVal(float_num);
  if(double_val == NAN){
    return error_sexp("round argument is not a number");
  } else if(NILP(mode)){
    return long_sexp(lround(double_val));
  } else if(!(INTP(mode))){
        return error_sexp("rounding mode type error");
  } else {
    switch (mode.val.int64){
    //ceil,floor & trunc return doubles, there is a function
    //lrint which rounds to integers based on the current rounding mode
    //but because rounding modes are tricky we use a bit of a hack by
    //using lround to get an integer from the specified rounding function
      case -1:
        return long_sexp(lround(floor(double_val)));
      case 0:
        return long_sexp(lround(double_val));
      case 1:
        return long_sexp(lround(ceil(double_val)));
      case 2:
        return long_sexp(lround(trunc(double_val)));
      default:
        return error_sexp("round error,undefined rounding mode");
    }
  }
}
sexp lisp_evenp(sexp obj){
  if(!BIGNUMP(obj)){
    return error_sexp("even? type error, expected a number");
  } else {
    switch(obj.tag){
      case _int64:
        return (obj.val.int64 &1 ? LISP_FALSE : LISP_TRUE);
      case _real64:
        return (obj.val.real64 == (lisp_round(obj,NIL)).val.int64 ?
                ((lisp_round(obj,NIL)).val.int64 &1 ? LISP_FALSE :LISP_TRUE): 
                LISP_FALSE);
      case _bigint:
        return (mpz_tstbit(*obj.val.bigint,0) ? LISP_FALSE :LISP_TRUE);
      case _bigfloat:
        return LISP_FALSE;//too lazy to do this now
      default:
        return error_sexp("even? unimplemented numeric type");
    }
  }
}
sexp lisp_oddp(sexp obj){
  if(!BIGNUMP(obj)){
    return error_sexp("even? type error, expected a number");
  } else {
    switch(obj.tag){
      case _int64:
        return (obj.val.int64 &1 ? LISP_TRUE : LISP_FALSE);
      case _real64:
        return (obj.val.real64 == (lisp_round(obj,NIL)).val.int64 ? 
                ((lisp_round(obj,NIL)).val.int64 &1 ? LISP_TRUE : LISP_FALSE) :
                LISP_FALSE);
      case _bigint:
        return (mpz_tstbit(*obj.val.bigint,0) ? LISP_TRUE : LISP_FALSE);
      case _bigfloat:
        return LISP_FALSE;//too lazy to do this now
      default:
        return error_sexp("even? unimplemented numeric type");
    }
  }
}
sexp lisp_iota(sexp start,sexp stop,sexp step,sexp arrayorlist,sexp rnd){
  if(NILP(arrayorlist)){
    return list_iota(start, stop, step);
  } else {
    return array_iota(start,stop,step,rnd);
  }
}
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
sexp lisp_min(sexp a,sexp b){
  if (!NUMBERP(a) || !(NUMBERP(b))){
    return error_sexp("arguments to min must be numbers");
  } if (a.tag == b.tag && a.tag==_long){
    return (a.val.int64 > b.val.int64?a:b);
  } else {
    return (getDoubleVal(a) > getDoubleVal(b)?a:b);
  }
}
sexp lisp_max(sexp a,sexp b){
  if (!NUMBERP(a) || !(NUMBERP(b))){
    return error_sexp("arguments to max must be numbers");
  } if (a.tag == b.tag && a.tag==_long){
    return (a.val.int64 < b.val.int64?a:b);
  } else {
    return (getDoubleVal(a) < getDoubleVal(b)?a:b);
  }
}
sexp lisp_zerop(sexp obj){
  if(!BIGNUMP(obj)){
    return error_sexp("error expected an number");
  }
  return lisp_numeq(obj,long_sexp(0));
}
sexp lisp_open(sexp filename,sexp mode){
  if(NILP(mode)){
    mode=string_sexp("r");
  }
  if (!STRINGP(filename) || !(STRINGP(mode))){
    return error_sexp("arguments to open must be strings");
  }
  FILE* file = fopen(CORD_as_cstring(filename.val.cord),
                     CORD_as_cstring(mode.val.cord));
  if(file){
    return (sexp){.tag=_stream,.val={.stream=file}};
  } else {
    PRINT_MSG(filename.val.cord);
    PRINT_MSG(mode.val.cord);
    return_errno("fopen");
  }
}
sexp lisp_close(sexp stream){
  if(!STREAMP(stream)){
    return error_sexp("invalid file descriptor passed to close");
  } else {
    //fclose returns 0 on success and EOF on failure
    if(fclose(stream.val.stream)){
      return_errno("fclose");
    } else {
        return NIL;
    }
  }
}
sexp lisp_fputs(sexp string,sexp stream){
  if(!STREAMP(stream)){
    return error_sexp("invalid stream passed to fputs");
  } else if (!STRINGP(string)){
    return error_sexp("invalid string passed to fputs");
  } else if (string.tag == _str){
    fputs(CORD_as_cstring(string.val.cord),stream.val.stream);
  } else {//string must be a w_char string
    fputws(string.val.ustr,stream.val.stream);
  }
  return NIL;
}
sexp arith_driver_simple(sexp required,sexp values,enum operator op){
  sexp(*f)(sexp,sexp);
  sexp retval;
  if(!(NUMBERP(required))){
    return error_sexp("arathmatic type error");
  }
  switch(op){
    case _add:
      f=lisp_add;
      retval=required;
      break;
    case _sub:
      if(NILP(values)){
        if(INTP(required)){
          return (sexp){.tag=_long,.meta=required.meta,
              .val = {.int64 = (required.val.int64<0?
                                required.val.int64:
                                -required.val.int64)}};
        } else if(FLOATP(required)){
          return (sexp){.tag=_double,.meta=required.meta,
              .val = {.real64 = (required.val.real64<0?
                                required.val.real64:
                                -required.val.real64)}};
        } else {goto TYPE_ERROR;}
      } else {
        f=lisp_sub;
        retval=required;
      }
    case _mul:
      f=lisp_mul;
      retval=required;
    case _div:
      if(NILP(values)){
        if(NUMBERP(required)){
        return (sexp){.tag=_double,.meta=required.meta,
            .val={.real64=1/getDoubleVal(required)}};
        } else {goto TYPE_ERROR;}
      } else {
        f=lisp_div;
        retval=required;
      }
    case _min:
      f=lisp_min;
      retval=required;
    case _max:
      f=lisp_max;
      retval=required;
      //do bitwise stuff
  }
  while(CONSP(values)){
    retval=f(retval,XCAR(values));
    values=XCDR(values);
    if(ERRORP(retval)){
      goto TYPE_ERROR;
    }
  }
  return retval;
 TYPE_ERROR:
  return error_sexp("Type error");
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
sexp lisp_eq(sexp obj1,sexp obj2){
  if(BIGNUMP(obj1) && BIGNUMP(obj2)){
    if(NUMBERP(obj1) && NUMBERP(obj2)){
      return lisp_numeq(obj1,obj2);
    } else {
      //define this first
      //      return lisp_bignumeq(obj1,obj2);
    }
  }
  if(obj1.tag != obj2.tag){
    return LISP_FALSE;
  }
  switch(obj1.tag){
    case _cons:
    case _list:
    case _sym:
    case _array:
    case _str:
    case _lenv:
    case _funarg:
    case _keyword:
      return (obj1.val.int64 == obj2.val.int64 ? LISP_TRUE : LISP_FALSE);
    case _type:
    case _special:
      return (obj1.val.meta == obj2.val.meta ? LISP_TRUE : LISP_FALSE);
    default:
      return LISP_FALSE;
  }
}
sexp lisp_eql(sexp obj1,sexp obj2){
  if(obj1.tag != obj2.tag){
    return LISP_FALSE;
  }
  if(STRINGP(obj1) && STRINGP(obj2)){
    return (CORD_cmp(obj1.val.cord,obj2.val.cord)==0?LISP_TRUE : LISP_FALSE);
  } else {
    return lisp_eq(obj1,obj2);
  }
} 
sexp lisp_equal(sexp obj1,sexp obj2){
  return lisp_eql(obj1,obj2);
}
sexp lisp_error(sexp error_message){
  if(!STRINGP(error_message) && !ERRORP(error_message)){
    return format_type_error_opt2("raise-error","string","error",
                                  error_message.tag);
  }
  return error_sexp(error_message.val.cord);
}
sexp lisp_not(sexp bool){
  if(isTrue(bool)){
    return LISP_FALSE;
  } else {
    return LISP_TRUE;
  }
}
sexp lisp_assert(sexp expr){
  if(isTrue(expr)){
    return NIL;
  } else {
    return error_sexp("Assertation faliure");
  }
}
sexp lisp_assert_eq(sexp obj1,sexp obj2){
  if(isTrue(lisp_eq(obj1,obj2))){
    return NIL;
  } else {
    return format_error_sexp("Assertation error, %r is not eq to %r",print(obj1),print(obj2));
  }
}
sexp lisp_gensym(){
  symref retval=xmalloc(sizeof(symbol));
  CORD_sprintf(&retval->name,"#:%ld",global_gensym_counter++);
  retval->val=UNBOUND;
  return symref_sexp(retval);
}
/*probably eaiser in lisp
  (defmacro ++! (x) `(setq ,x (++ ,x)))
  (defmacro --! (x) `(setq ,x (-- ,x)))
*/
#define lisp_stderr {.tag = _stream,.val={.stream=stderr}}
#define lisp_stdout {.tag = _stream,.val={.stream=stdout}}
#define lisp_stdin {.tag = _stream,.val={.stream=stdin}}
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
#define lisp_NIL {.tag = -1,.val={.meta = -1}}
#define lisp_LISP_TRUE {.tag = -2,.val={.meta = 11}}
#define lisp_LISP_FALSE {.tag = -3,.val={.meta = -3}}
#define lisp_ans {.tag=-1,.val={.meta=-1}}
MK_PREDICATE3(consp,_cons,_list,_dpair);
MK_PREDICATE2(numberp,_long,_double);
MK_PREDICATE(arrayp,_array);
MK_PREDICATE(nilp,_nil);
MK_PREDICATE(symbolp,_sym);
MK_PREDICATE(bigintp,_bigint);
MK_PREDICATE(bigfloatp,_bigfloat);
MK_PREDICATE2(stringp,_str,_ustr);
MK_PREDICATE4(bignump,_bigint,_bigfloat,_long,_double);
MK_PREDICATE(errorp,_error);
MK_PREDICATE(functionp,_fun);
MK_PREDICATE(streamp,_stream);
