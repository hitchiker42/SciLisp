#include "common.h"
#include "prim.h"
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
MK_PREDICATE2(booleanp,_true,_false);
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
      //these are singleton values so if they have the same tag they must be eq
    case _nil:
    case _true:
    case _false:
      return LISP_TRUE;    
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
  if(obj1.tag != obj2.tag){
    return LISP_FALSE;
  } 
  switch(obj1.tag){
    case _cons:
    case _list:
    case _dpair:
      if(cons_length(obj1).val.int64 != cons_length(obj2).val.int64){
        return LISP_FALSE;
      } else {
        return cons_equal(obj1,obj2);
      }
    case _array:
      if(obj1.len != obj2.len){
        return LISP_FALSE;
      } else {
        int i;
        for(i=0;i<obj1.len;i++){
          if(!isTrue(lisp_equal(XAREF(obj1,i),XAREF(obj2,i)))){
            return LISP_FALSE;
          }
        }
        return LISP_TRUE;
      }
    default:
    return lisp_eql(obj1,obj2);
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
sexp _getKeywordType(sexp obj){
  CORD type_symbol_name=CORD_catn
    (3,"#<",CORD_substr(obj.val.keyword->name,1,
                        CORD_len(obj.val.keyword->name)-1),">");
  symref type_sym=getGlobalSym(type_symbol_name);
  if(!type_sym){
    return error_sexp("unknown typename passed to get-type");
  } else if(!TYPEP(type_sym->val)){
    return error_sexp("non type keyword passed to get-type");
  } else {
    return type_sym->val;
  }
}
sexp getKeywordType(sexp obj){
  if(!KEYWORDP(obj)){
    return format_type_error("get-type","keyword",obj.tag);
  }
  return _getKeywordType(obj);
}
