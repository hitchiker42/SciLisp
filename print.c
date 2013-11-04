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
    mk_tag_name(_unbound,unbound);
    mk_tag_name(_error,error);
    mk_tag_name(_false,#f);
    mk_tag_name(_uninterned,uninterned);
    mk_tag_name(_nil,nil);
    mk_tag_name(_cons,cons);
    mk_tag_name(_long,long);
    mk_tag_name(_double,double);
    mk_tag_name(_bigint,BigInt);
    mk_tag_name(_bigfloat,BigFloat);
    mk_tag_name(_char,char);
    mk_tag_name(_str,string);
    mk_tag_name(_array,array);
    mk_tag_name(_stream,stream);
    mk_tag_name(_list,list);
    mk_tag_name(_dpair,dotted pair);
    mk_tag_name(_fun,function);
    mk_tag_name(_sym,symbol);
    mk_tag_name(_special,special form);
    mk_tag_name(_macro,macro);
    mk_tag_name(_type,type);
    mk_tag_name(_lam,lambda);
    mk_tag_name(_lenv,local environment);
    mk_tag_name(_keyword,Keyword Symbol);
    mk_tag_name(_funargs,Function Args);
    mk_tag_name(_true,t);
    mk_tag_name(_obarray,obarray);    
    default:{
      CORD retval;
      CORD_sprintf(&retval,"don't know how to print tag number %d",obj_tag);
      return CORD_to_char_star(retval);
    }
  }
}
#undef mk_tag_name
#define spec_to_string(spec)                    \
  case _##spec: return #spec
c_string specialForm_name(sexp obj){
  special_form spec=obj.val.special;
  switch(spec){
    spec_to_string(def);
    spec_to_string(defun);
    spec_to_string(setq);
    spec_to_string(if);
    spec_to_string(let);
    spec_to_string(do);
    spec_to_string(lambda);
    spec_to_string(progn);
    spec_to_string(go);
    spec_to_string(tagbody);
    spec_to_string(struct);
    spec_to_string(union);
    spec_to_string(datatype);
    spec_to_string(enum);
    spec_to_string(eval);
    spec_to_string(defmacro);
    spec_to_string(quasi);
    spec_to_string(quote);
    spec_to_string(comma);
    spec_to_string(and);
    spec_to_string(or);
    spec_to_string(main);
    spec_to_string(while);
    spec_to_string(prog1);
    default:
      return "woops forgot to implement that special form";
  }
}
c_string typeName(sexp obj){
  return tag_name(obj.tag);
}
sexp lisp_typeName(sexp obj){
  return (sexp){.tag = _str,.val={.cord = CORD_from_char_star(typeName(obj))}};
}
CORD print_num_format(sexp obj,CORD format){
  if(!BIGNUMP(obj)){return 0;}
  else{
    CORD retval;
    if(FLOATP(obj)){
      if(format != 0){
        CORD_sprintf(&retval,format,(double)obj.val.real64);
      } else {
        CORD_sprintf(&retval,"%g",(double)obj.val.real64);
      }
    } else if (INTP(obj)){
      if(format != 0){     
        CORD_sprintf(&retval,format,(long)obj.val.int64);
      } else {
        CORD_sprintf(&retval,"%ld",(long)obj.val.int64);
      }
    } else if (BIGINTP(obj)){
      if(format != 0){
        char *temp;
        gmp_asprintf(&temp,format,(*obj.val.bigint));
        retval=temp;
      } else {
        char *temp;
        gmp_asprintf(&temp,"%Zd",(*obj.val.bigint));
        retval=temp;
      }
    } else if (BIGFLOATP(obj)){
      if(format != 0){
        char *temp;
        mpfr_asprintf(&temp,format,(*obj.val.bigfloat));
        retval=temp;
        //        retval=CORD_from_char_star(temp);
        //        mpfr_free_str(temp);
      } else {
        char *temp;
        mpfr_asprintf(&temp,"%RG",(*obj.val.bigfloat));
        retval=temp;
        //        retval=CORD_from_char_star(temp);
        //        mpfr_free_str(temp);
      }
    }
    return retval;
  }
}
inline CORD print_num(sexp obj){
  return print_num_format(obj,0);
}
CORD print(sexp obj){
  CORD retval,acc;
  switch (obj.tag){
    HERE();
    case _double:
    case _long:
    case _bigint:
    case _bigfloat:
      return print_num(obj);
    case _fun:
      return obj.val.fun->lname;
    case _sym:
      return obj.val.var->name;
    case _keyword:
      return obj.val.keyword->name;
    case _char:{
      CORD_sprintf(&retval,"%lc",(wchar_t)obj.val.uchar);
      return retval;
    }
    case _nil:
      return "()";
    case _list:
    case _cons:
      acc="(";
      while(CONSP(obj)){
        acc=CORD_cat(acc,print(car(obj)));
        obj=cdr(obj);
        if(CONSP(obj)){acc=CORD_cat_char(acc,' ');}
        //CORD_fprintf(stderr,acc);fputs("\n",stderr);
      }
      if(!NILP(obj)){
        CORD_sprintf(&retval,"%r . %r)",acc,print(obj));
      } else {
        acc=CORD_cat(acc,")");
        retval=acc;
      }
      return CORD_balance(retval);
    case _uninterned:
      switch(obj.val.meta){
        case 11:
          return "#t";
        case -0xf:
          return "unbound symbol";
        default:
          return "uninterned symbol";
      }
    case _error:
    case _str:
      CORD_sprintf(&retval,"\"%r\"",obj.val.cord);
      return retval;
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
    case _funarg:{
      acc="(";
      function_args* args=obj.val.funarg;
      int i=0,j=0;
      if(args->num_req_args){
        for(i=0;i<args->num_req_args;i++){
          acc=CORD_cat(acc,args->args[j++].name);
          if(j != args->max_args){
            acc=CORD_cat(acc," ");
          }
        }
      }
#define funarg_print_loop(argtype,optional_str)             \
      if(args->num_##argtype##_args){                       \
        acc=CORD_cat(acc,optional_str);                     \
        for(i=0;i<args->num_##argtype##_args;i++){          \
          acc=CORD_cat(acc,args->args[j++].name);                \
          if(j != args->max_args){                          \
            acc=CORD_cat(acc," ");                          \
          }                                                 \
        }                                                   \
      }
      funarg_print_loop(opt,"&optional");
      funarg_print_loop(keyword,"&key");
      if(args->has_rest_arg){
        acc=CORD_catn(4,"&rest ",args->args[j],")");
      } else {
        acc=CORD_cat(acc,")"); 
      } 
      return CORD_balance(acc);
    }
#undef funarg_print_loop
    case _lam:
      /*      acc="(lambda ";
      acc=CORD_catn(5,acc,
                    print((sexp){.tag=_lenv,.val=
                          {.lenv =(local_env*)obj.val.lam->env}})
                    ," ",print(obj.val.lam->body),")");
                    return CORD_balance(acc);*/
      return "Lambda";
    case _array:
      acc="[";
      int i;
      CORD format=0;
      data* arr=obj.val.array;
      PRINT_FMT("len = %d",obj.len);
      if(obj.meta==_double_array){
        for(i=0;i<obj.len;i++){
          CORD_sprintf(&format,"%f",arr[i].real64);
          acc=CORD_cat(acc,format);
          if(i<obj.len-1){
            acc=CORD_cat_char(acc,' ');
          }
        }
      } else if (obj.meta == _long_array){
        for(i=0;i<obj.len;i++){
          CORD_sprintf(&format,"%d",arr[i].int64);
          acc=CORD_cat(acc,format);
          if(i<obj.len-1){
            acc=CORD_cat_char(acc,' ');
          }
        }
      }
      return CORD_balance(CORD_cat(acc,"]"));
    case _special:
      return specialForm_name(obj);
    case _false:
      return "#f";
    case _type:
      return tag_name(obj.val.meta);
    case _stream:
      CORD_sprintf(&retval,"File descriptor %d",fileno(obj.val.stream));
      return retval;
    case _obarray:{
      obarray* ob=obj.val.ob;
      CORD_sprintf
        (&retval,
         "Obarray statistics:\nsize:%d\nused:%d\nentries:%d\ncapacity:%f\n",
         ob->size,ob->used,ob->entries,ob->capacity);
        return retval;
    }
    default:
      CORD_sprintf(&error_str,"print error got type %s",typeName(obj));
      return error_str;
  }
}
sexp lisp_print(sexp obj){
  CORD print_string = print(eval(obj,topLevelEnv));
  CORD_fprintf(stderr,"%r\n",print_string);
  return (sexp){.tag=_str,.val={.cord=print_string}};
}
sexp lisp_println(sexp obj){
  CORD print_string = print(eval(obj,topLevelEnv));
  print_string=CORD_cat(print_string,"\n");
  CORD_fprintf(stderr,"%r\n",print_string);
  return (sexp){.tag=_str,.val={.cord=print_string}};
}
sexp lisp_fprint(sexp obj, sexp file){
  lisp_fputs(lisp_print(obj),file);
  return NIL;
}
sexp lisp_fprintln(sexp obj, sexp file){
  lisp_fputs(lisp_println(obj),file);
  return NIL;
}
/*CORD function_name(function fun){
  if(fun.fxn_type==1){
    return "lambda";
  } else if(fun.fxn_type == 0){
    return fun.fun.prim->lispname;
  }
  }*/
#define mk_tok_name(tok) case tok: return #tok
CORD token_name(TOKEN token){
  switch(token){
    mk_tok_name(TOK_EOF);
    mk_tok_name(TOK_INT);
    mk_tok_name(TOK_REAL);
    mk_tok_name(TOK_CHAR);
    mk_tok_name(TOK_STRING);
    mk_tok_name(TOK_ID);
    mk_tok_name(TOK_LISP_TRUE);
    mk_tok_name(TOK_LISP_FALSE);
    mk_tok_name(TOK_SPECIAL);
    mk_tok_name(TOK_QUOTE);
    mk_tok_name(TOK_COMMENT_START);
    mk_tok_name(TOK_COMMENT_END);
    mk_tok_name(TOK_DOT);
    mk_tok_name(TOK_COLON);
    mk_tok_name(TOK_LAMBDA);
    mk_tok_name(TOK_TYPEDEF);
    mk_tok_name(TOK_TYPEINFO);
    mk_tok_name(TOK_LPAREN);
    mk_tok_name(TOK_RPAREN);
    mk_tok_name(TOK_LBRACE);
    mk_tok_name(TOK_RBRACE);
    mk_tok_name(TOK_LCBRACE);
    mk_tok_name(TOK_RCBRACE);
    default:
      return "forgot to implemnt that token";
  }
}
