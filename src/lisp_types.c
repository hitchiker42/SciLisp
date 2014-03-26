/* Various functions and predicates for lisp types and equality

   Copyright (C) 2013-2014 Tucker DiNapoli

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
//this is for type related functions, and it's also a catchall
//for simple stuff with no obvious location
#include "common.h"
#include "prim.h"
#define MK_PREDICATE(lname,macro)               \
  sexp lisp_##lname (sexp obj){                 \
    if(macro(obj)){                             \
      return LISP_TRUE;                         \
    } else {                                    \
      return LISP_FALSE;                        \
    }                                           \
  }
MK_PREDICATE(arrayp,ARRAYP)
MK_PREDICATE(bigintp,BIGINTP)
MK_PREDICATE(bigfloatp,BIGFLOATP)
MK_PREDICATE(bignump,BIGNUMP)
MK_PREDICATE(consp,CONSP)
MK_PREDICATE(subrp,SUBRP)
MK_PREDICATE(realp,REALP)
MK_PREDICATE(intp,INTP)
MK_PREDICATE(nilp,NILP)
MK_PREDICATE(sequencep,SEQUENCEP)
MK_PREDICATE(streamp,STREAMP)
MK_PREDICATE(stringp,STRINGP)
MK_PREDICATE(hashtablep,HASHTABLEP)
MK_PREDICATE(integerp,INT_ANYP)
MK_PREDICATE(numberp,NUMBERP)
//This differs from common lisp and elisp where eq tests for identical objects
//eql in addition checks for numerical equality(only for the same types)
//equal in addition compares conses and strings elementwise
//equalp in addition compares numbers ignoring type and arrays elementwise
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
  return (obj1.val.uint64 == obj2.val.uint64 ? LISP_TRUE : LISP_FALSE);
}
sexp lisp_identical(sexp obj1,sexp obj2){//same as common lisp eq
  if(obj1.tag != obj2.tag){
    return lisp_false;
  } else {
    return (obj1.val.uint64 == obj2.val.uint64 ? LISP_TRUE : LISP_FALSE);
sexp lisp_eql(sexp obj1,sexp obj2){
  if(EQ(obj1,obj2)){//make simple equaiity fast
    return LISP_TRUE;
  }
  //eql adds string equality
  if(STRINGP(obj1) && STRINGP(obj2)){
    return lisp_string_equal(obj1,obj2);
  }
  return LISP_FALSE;
} 
sexp lisp_equal(sexp obj1,sexp obj2){
  if(EQ(obj1,obj2)){//make simple equaiity fast
    return LISP_TRUE;
  }
  if(obj1.tag != obj2.tag){
    return LISP_FALSE;
  }
  switch(obj1.tag){
    case sexp_cons:
      if(cons_length(obj1).val.int64 != cons_length(obj2).val.int64){
        return LISP_FALSE;
      } else {
        return cons_equal(obj1,obj2);
      }
    case sexp_array:
      if(obj1.len != obj2.len){
        return LISP_FALSE;
      } else {
        int i;
        for(i=0;i<obj1.len;i++){
          if(!is_true(lisp_equal(XAREF(obj1,i),XAREF(obj2,i)))){
            return LISP_FALSE;
          }
        }
        return LISP_TRUE;
      }
    case sexp_string:
      return (CORD_cmp(obj1.val.cord,obj2.val.cord)==0 ? LISP_TRUE : LISP_FALSE);
    default:
      return LISP_FALSE;
  }
}
sexp lisp_not_eq(sexp obj1,sexp obj2){
  return lisp_not(lisp_eq(obj1,obj2));
}
sexp lisp_not_eql(sexp obj1,sexp obj2){
  return lisp_not(lisp_eql(obj1,obj2));
}
sexp lisp_not_equal(sexp obj1,sexp obj2){
  return lisp_not(lisp_equal(obj1,obj2));
}
inline sexp get_type_from_string(CORD typestring){
  CORD type_symbol_name=CORD_catn(3,"#<",typestring,">");
  symref type_sym=getGlobalSym(type_symbol_name);
  if(!type_sym){
    return format_error_sexp("unknown typename %s",typestring);
  } else if(!TYPEP(type_sym->val)){
    return format_error_sexp("%s is not a type)",typestring);
  } else {
    return type_sym->val;
  }
}
sexp internal_get_keyword_type(sexp obj){
  return get_type_from_string
    (CORD_substr(obj.val.keyword->name,1,
                 CORD_len(obj.val.keyword->name)-1));
}
sexp get_keyword_type(sexp obj){
  if(!KEYWORDP(obj)){
    return format_type_error("get-type","keyword",obj.tag);
  }
  return internal_getKeywordType(obj);
}
sexp lisp_cast_to_float(sexp obj){
  if(!NUMBERP(obj)){
    return format_type_error("float","number",obj.tag);
  }
  return real64_sexp(getDoubleValUnsafe(obj));
}

sexp lisp_strtod(sexp obj){
  if(!STRINGP(obj)){
    return format_type_error("string->real","string",obj.tag);
  }
  char *err_test;
  double retval=strtod(CORD_to_const_char_star(obj.val.cord),&err_test);
  if(err_test){
    return error_sexp("invalid string passed to string->real");
  } else {
    return real64_sexp(retval);
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
sexp lisp_identity(sexp expr){
  return expr;
}
CORD make_function_signature(function_args *args){
  CORD signature="(";
  int i,j;
  for(i=0,j=0;j<args->num_req_args;i++,j++){
    signature=CORD_catn(3,signature,args->args[i].name," ");
  }
  if(args->num_opt_args>0){
    signature=CORD_cat(signature,"&optional ");
    for(j=0;j<args->num_opt_args;j++,i++){
      signature=CORD_catn(3,signature,args->args[i].name," ");
    }
  }
  if(args->num_keyword_args){//do nothing untill I implement keyword args
  }
  if(args->has_rest_arg){
    signature=CORD_catn(4,signature,"&rest ",args->args[i].name,")");
    return CORD_balance(signature);
  } else {
    return CORD_balance
      (CORD_cat(CORD_substr(signature,0,CORD_len(signature)-1),")"));
  }
}
sexp boundp(sexp sym){
  if(!SYMBOLP(sym)){
    return format_type_error("bound?","symbol",sym.tag);
  }
  symref ref=getSymFromSexp(sym,NULL);
  return (ref?LISP_TRUE:LISP_FALSE);
}
