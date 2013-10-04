#include "common.h"
#include "cons.h"
c_string tag_name(_tag obj_tag){
  switch(obj_tag){
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
  }
}
c_string toString_tag(sexp obj){
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
  }
}
