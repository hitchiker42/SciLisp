/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include "print.h"
#define mk_tag_name(tag,name) case tag: return #name
c_string tag_name(_tag obj_tag){
  switch(obj_tag){
    mk_tag_name(_uninterned,uninterned);
    mk_tag_name(_list,list);
    mk_tag_name(_nil,nil);
    mk_tag_name(_cons,cons);
    mk_tag_name(_double,double);
    mk_tag_name(_long,long);
    mk_tag_name(_char,char);
    mk_tag_name(_str,string);
    mk_tag_name(_fun,function);
    mk_tag_name(_lam,lambda);
    mk_tag_name(_sym,symbol);
    mk_tag_name(_special,special form);
    mk_tag_name(_macro,macro);
    mk_tag_name(_array,array);
    mk_tag_name(_type,type);
    mk_tag_name(_true,t);
    mk_tag_name(_lenv,local environment);
  }
}
c_string typeName(sexp obj){
  return tag_name(obj.tag);
}
sexp lisp_typeName(sexp obj){
  return (sexp){.tag = _str,.val={.cord = CORD_from_char_star(typeName(obj))}};
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
inline CORD print_num(sexp obj){
  return print_num_format(obj,0);
}
CORD print(sexp obj){
  //PRINT_FMT("princ for a %s object",tag_name(obj.tag));
  CORD retval,acc;
  switch (obj.tag){
    HERE();
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
      } else {
        return "uninterned symbol";
      }
    case _str:
      return obj.val.cord;
    case _lenv:{
      local_symref cur_sym=obj.val.lenv;
      acc="(";
      while(cur_sym != 0){
        acc=CORD_cat(acc,cur_sym->name);
        cur_sym=cur_sym->next;
        if(cur_sym != 0){
          acc=CORD_cat_char(acc,' ');
        }
      }
      return CORD_balance(CORD_cat(acc,")"));
    }
    case _lam:
      acc="(lambda ";
      acc=CORD_catn(5,acc,
                    print((sexp){.tag=_lenv,.val={.lenv =obj.val.lam->env.head}})
                    ," ",print(obj.val.lam->body),")");
      return CORD_balance(acc);
    case _array:
      acc="[";
      int i;
      CORD format=0;
      data* arr=obj.val.array;
      PRINT_FMT("len = %d",obj.len);
      if(obj.meta==1){
        for(i=0;i<obj.len;i++){
          CORD_sprintf(&format,"%f",arr[i].real64);
          acc=CORD_cat(acc,format);
          if(i<obj.len-1){
            acc=CORD_cat_char(acc,' ');
          }
        }
      } else {
        for(i=0;i<obj.len;i++){
          CORD_sprintf(&format,"%d",arr[i].int64);
          acc=CORD_cat(acc,format);
          if(i<obj.len-1){
            acc=CORD_cat_char(acc,' ');
          }
        }
      }
      return CORD_balance(CORD_cat(acc,"]"));
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

