/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
c_string tag_name(_tag obj_tag){
  switch(obj_tag){
    case _uninterned:
      return "uninterned";
    case _nil:
      return "nil";
    case _cons:
      return "cons";
    case _double:
      return "double";
    case _long:
      return "long";
    case _char:
      return "char";
    case _str:
      return "string";
    case _fun:
      return "function";
    case _sym:
      return "symbol";
    case _special:
      return "special form";
    case _macro:
      return "macro";
    case _array:
      return "array";
    case _type:
      return "type";
    case _true:
      return "t";
  }
}
c_string typeName(sexp obj){
  return tag_name(obj.tag);
}
CORD print_num_format(sexp obj,CORD format){
  if(!NUMBERP(obj)){return 0;}
  else{
    CORD retval;
    if(FLOATP(obj)){
      if(format != 0){
        CORD_sprintf(&retval,format,(double)obj.val.real64);
      } else {
        CORD_sprintf(&retval,"%g",(double)obj.val.real64);
      }
    } else {
      if(format != 0){
        CORD_sprintf(&retval,format,(long)obj.val.int64);
      } else {
        CORD_sprintf(&retval,"%ld",(long)obj.val.int64);
      }
    }
    return retval;
  }
}
CORD print_num(sexp obj){
  return print_num_format(obj,0);
}
CORD print(sexp obj){
  //PRINT_FMT("princ for a %s object",tag_name(obj.tag));
  CORD retval,acc;
  switch (obj.tag){
    case _double:
    case _long:
      return print_num(obj);
    case _fun:
    case _sym:
      return obj.val.var->name;
      //retval=obj.val.var->name;
      //PRINT_MSG(obj.val.var->name);
      break;
    case _char:
      CORD_sprintf(&retval,"%lc",obj.val.utf8_char);
      return retval;
    case _nil:
      return "()";
    case _list:
    case _cons:
      acc="(";
      while(CONSP(obj)){
        acc=CORD_cat(acc,print(car(obj)));
        obj=cdr(obj);
        if(CONSP(obj)){acc=CORD_cat_char(acc,' ');}
      }
      if(!NILP(obj)){
        CORD_sprintf(&retval,"%r . %r)",acc,print(obj));
      } else {
        retval=CORD_cat_char(acc,')');
      }
      return CORD_balance(retval);
    case _uninterned:
      if(obj.val.meta==11){
        return "t";
      }
    case _str:
      return obj.val.cord;
    default:
      CORD_sprintf(&error_str,"print error got type %s",typeName(obj));
      return error_str;
  }
}
sexp lisp_print(sexp obj){
  CORD print_string = print(obj);
  CORD_printf("%r\n",print_string);
  return (sexp){.tag=_str,.val={.cord=print_string}};
}
sexp lisp_typeName(sexp obj){
  return (sexp){.tag = _str,.val={.cord = CORD_from_char_star(typeName(obj))}};
}
