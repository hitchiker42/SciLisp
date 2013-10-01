#include "common.h"
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
c_string princ_nested(sexp obj,int nest){
  //PRINT_FMT("princ for a %s object",tag_name(obj.tag));
  char* retval=xmalloc(100*sizeof(char));
  switch (obj.tag){
    case _double: 
      snprintf(retval,100,"%g",(double)obj.val.real64);
      //PRINT_FMT("real %g",obj.val.real64);
      break;
    case _long:
      snprintf(retval,100,"%#0x",(long)obj.val.int64);
      //PRINT_FMT("long %#0x",obj.val.int64);
      break;
    case _fun:
    case _sym:     
      //asprintf(&retval,"%s",obj.val.var->name);
      snprintf(retval,100,"%s",obj.val.var->name);
      //retval=obj.val.var->name;
      //PRINT_MSG(obj.val.var->name);
      break;
    case _char:
      snprintf(retval,100,"%lc",obj.val.utf8_char);
      break;
    case _nil:
      retval="()";
      break;
    case _cons:
      if(obj.val.cons == 0){snprintf(retval,100,"");}
      else{
        c_string car_str=princ_nested(obj.val.cons->car,1);
        c_string cdr_str=princ_nested(obj.val.cons->cdr,1);
        if(!strcmp(car_str,"") && !strcmp(cdr_str,"")){
          retval="";
        } else if (!strcmp(cdr_str,"")){
          snprintf(retval,100,"%s",car_str);
        } else if (!strcmp(car_str,"")){
          snprintf(retval,100,"%s",cdr_str);
        } else{
        //PRINT_MSG(car_str);
        //PRINT_MSG(cdr_str);
          if(nest){
          snprintf(retval,100,"(%s . %s)",car_str,cdr_str);
          } else {
          snprintf(retval,100,"(%s . %s)",car_str,cdr_str);
          }
        }
      }
      break;
  }
  return retval;
}
c_string princ(sexp obj){
  return princ_nested(obj,0);
}
