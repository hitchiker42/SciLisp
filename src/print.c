/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include "regex.h"
#include "print.h"
//#include "tree.h"
#include "hash.h"
#define mk_tag_name(tag,name) case tag: return #name
const char *tag_name(sexp_tag obj_tag){
  switch(obj_tag){
    mk_tag_name(sexp_unbound,unbound);
    mk_tag_name(sexp_error,error);
    mk_tag_name(sexp_false,#f);
    mk_tag_name(sexp_nil,nil);
    mk_tag_name(sexp_cons,cons);
    mk_tag_name(sexp_int8,int8);
    mk_tag_name(sexp_int16,int16);
    mk_tag_name(sexp_int32,int32);
    mk_tag_name(sexp_int64,int64);
    mk_tag_name(sexp_uint8,uint8);
    mk_tag_name(sexp_uint16,uint16);
    mk_tag_name(sexp_uint32,uint32);
    mk_tag_name(sexp_uint64,uint64);
    mk_tag_name(sexp_double,double);
    mk_tag_name(sexp_bigint,bigint);
    mk_tag_name(sexp_bigfloat,bigfloat);
    mk_tag_name(sexp_char,char);
    mk_tag_name(sexp_str,string);
    mk_tag_name(sexp_array,array);
    mk_tag_name(sexp_stream,stream);
    mk_tag_name(sexp_subr,subroutine);
    mk_tag_name(sexp_sym,symbol);
    mk_tag_name(sexp_type,type);
    mk_tag_name(sexp_true,t);
    mk_tag_name(sexp_obarray,obarray);
    mk_tag_name(sexp_typed_array,typed array);
    mk_tag_name(sexp_hash_table,hash table);
    mk_tag_name(sexp_ctype,ctype);
    mk_tag_name(sexp_cdata,cdata);
    default:{
      CORD retval;
      CORD_sprintf(&retval,"\"unknown tag number %d\"",obj_tag);
      return CORD_to_char_star(retval);
    }
  }
}
//temporary (should be generated)
#define mkTypeCase(type,tag) case tag: return type_sexp(type)
sexp type_of_tag(sexp_tag tag){
  switch(tag){
    mkTypeCase(Tint8,sexp_int8);
    mkTypeCase(Tint16,sexp_int16);
    mkTypeCase(Tint32,sexp_int32);
    mkTypeCase(Tint64,sexp_int64);
    mkTypeCase(Tuint8,sexp_uint8);
    mkTypeCase(Tuint16,sexp_uint16);
    mkTypeCase(Tuint32,sexp_uint32);
    mkTypeCase(Tuint64,sexp_uint64);
    mkTypeCase(Terror,sexp_error);
    mkTypeCase(Treal32,sexp_real32);
    mkTypeCase(Treal64,sexp_real64);
    mkTypeCase(Tbigint,sexp_bigint);
    mkTypeCase(Tbigfloat,sexp_bigfloat);
    mkTypeCase(Tchar,sexp_char);
    mkTypeCase(Tstring,sexp_string);
    mkTypeCase(Tarray,sexp_array);
    mkTypeCase(Tstream,sexp_stream);
    mkTypeCase(Tfun,sexp_fun);
    mkTypeCase(Tsymbol,sexp_symbol);
    mkTypeCase(Ttype,sexp_type);
    mkTypeCase(Thashtable,sexp_hashtable);
    mkTypeCase(Tregex,sexp_regex);
    mkTypeCase(Tnil,sexp_nil);
    mkTypeCase(Tenv,sexp_env);
    mkTypeCase(Tobarray,sexp_obarray);
    mkTypeCase(Ttrue,sexp_true);
    mkTypeCase(Tfalse,sexp_false);
    mkTypeCase(Tuninterned,sexp_uninterned);
    mkTypeCase(Tcons,sexp_cons);
    //    mkTypeCase(Tpointer,sexp_opaque);
  }
}
#undef mk_tag_name
const char *typeName(sexp obj){
  return tag_name(obj.tag);
}
sexp lisp_typeName(sexp obj){
  return (sexp){.tag = _str,.val={.cord = CORD_from_char_star(typeName(obj))}};
}
//
CORD print_num_format(sexp obj,CORD format){
  if(!BIGNUMP(obj)){return 0;}
  else{
    CORD retval;
    if(FLOATP(obj)){
      if(format != 0){
        CORD_sprintf(&retval,format,(double)obj.val.real64);
      } else {
        CORD_sprintf(&retval,"%.6g",(double)obj.val.real64);
      }
    } else if (INTP(obj)){
      if(format != 0){
        CORD_sprintf(&retval,format,(long)obj.val.int64);
      } else {
        CORD_sprintf(&retval,"%ld",(long)obj.val.int64);
      }
    } else if (UINT64P(obj)){
      if(format != 0){
        CORD_sprintf(&retval,format,(uint64_t)obj.val.uint64);
      } else {
        CORD_sprintf(&retval,"0x%lx",(uint64_t)obj.val.uint64);
      }
    }
    //I use asprintf for gmp and mpfr, but I set them to use
    //gc malloc for allocation, so the memory will get cleaned up
    else if (BIGINTP(obj)){      
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
      } else {
        char *temp;
        mpfr_asprintf(&temp,"%.10RG",(*obj.val.bigfloat));
        retval=temp;
      }
    } else {
      return "print num error";
    }
    return retval;
  }
}
inline CORD print_num(sexp obj){
  return print_num_format(obj,0);
}
//assume we have a global var *print-level* which
//determines when to abbrivate things   
static inline CORD print_array(sexp *arr,int len){
  int i;
  CORD acc="[";
  for(i=0;i<len-1;i++){
    acc=CORD_catn(3,acc,print(arr[i])," ");
  }
  return CORD_catn(3,acc,print(arr[len-1]),"]");
}
static inline CORD print_typed_array(data *arr,int len,int type){
  acc
  if(type==sexp_uchar){
    
CORD print(sexp obj){
  CORD retval=CORD_EMPTY,acc=CORD_EMPTY;
  if(obj.tag < 13){//so I don't need to write a case for every numeric type
    if(obj.tag == 0){
      return "nil";
    } else if (obj.tag == 1){
      return CORD_asprintf("%lc",obj.val.uchar);
    } else {
      return print_num(obj);
    }
  }
  switch (obj.tag){
    case sexp_subr:
      switch(obj.val.subr->subr_type){
        case subr_special_form:
          return CORD_catn(3,"#<special form ",obj.val.subr->lname,">")
        case subr_compiled:
          return CORD_catn(3,"#<compiled function ",obj.val.subr->lname,">");
        case subr_compiler_macro:
          return CORD_catn(3,"#<compiler macro  ",obj.val.subr->lname,">");
        case subr_lambda:
          //this won't work for the argument list as is
          return CORD_catn(3,"(lambda ",print(obj.val.subr->lambda_arglist),
                           print(cons_sexp(obj.val.subr->lambda_body)));
        default:
          raise_simple_error(Eprint,"don't know how to print that type of subr");
      }
    case sexp_sym:
      return obj.val.sym->name->name;
    case sexp_cons:
      acc=CORD_cat(acc,"(");
      int i=0;
      do{
        acc=CORD_cat(acc,print(XCAR(obj)));
        obj=XCDR(obj);        
      } while (CONSP(obj) && (acc=CORD_cat_char(acc,' ');));
      if(!NILP(obj)){//cons-cell/improper list
        CORD_sprintf(&retval,"%r . %r)",acc,print(obj));
      } else {
        acc=CORD_cat(acc,")");
        retval=acc;
      }
      return CORD_balance(retval);
    case sexp_str:
      //need to figure out how to do esacpe sequences
      return CORD_catn(3,"\"",obj.val.cord,"\"");
    case sexp_array:{
      lisp_array *arr=obj.val.array;
      if(arr->type == 0){
        if(arr->dims==1){
          int len=arr>len;
          if(len>lisp_print_length){
            return "[...]";
          } else {
            return print_array(arr->vector,len);
          }
        } else if (arr->dims == 2){
          int rows=arr->rows,cols=arr->cols,i;
          if(rows>lisp_print_length){
            return "[...]";
          } else if (cols>lisp_print_length){
            acc="[";
            for(i=0;i<rows;i++){
              acc=CORD_cat(acc,"[...]");
            }
            return CORD_balance(CORD_cat(acc,"]"));
          } else {
            acc="[";
            for(i=0;i<rows;i++){
              acc=CORD_cat(acc,print_array((arr->array)+(i*cols),cols));
            }
            return CORD_balance(CORD_cat(acc,"]"));
          }
        } else {
          CORD_asprintf("#<%d dimensional array>",arr->dims);
        }
      }//if type== char loop printig chars
      //if type <13 loop printing numbers
      //else we would've translated to a sexp array already
      //(theres not much point of having an array of some non 
      //scalar data, the inconvience is far greater than any performance gain)
      acc="[[";
      if(arr->type==sexp_uchar){
        
      return CORD_balance(CORD_cat(acc,"]]"));
    }
    case sexp_opaque:
      return "<#opaque pointer>";
    case sexp_false:
      return "#f";
    case sexp_type:
      return tag_name(obj.val.meta);
    case sexp_stream:
      CORD_sprintf(&retval,"File descriptor %d",fileno(obj.val.stream));
      return retval;
    case sexp_obarray:{
      obarray* ob=obj.val.ob;
      CORD_sprintf
        (&retval,
         "Obarray statistics:\nsize:%d\nused:%d\nentries:%d\ncapacity:%f\n",
         ob->size,ob->used,ob->entries,ob->capacity);
        return retval;
    }
    case sexp_re_data:
      CORD_sprintf
        (&retval,
         "#<re-match-data for \"%r\">",obj.val.re_data->re_string);
      return retval;
    case sexp_regex:
      return ("#<regular-expression>");
    case sexp_cdata:{
      c_data* c_obj=obj.val.c_val;
      if(c_obj->ptr_depth){
        acc=CORD_cat(acc,"#<");
        register int depth=c_obj->ptr_depth;
        while(depth){
          acc=CORD_cat(acc,"*");
          depth--;
        }
        acc=CORD_cat(acc,print(dereference_c_ptr(c_obj)));
        acc=CORD_cat(acc,">");
        return acc;
      } else {
        return print(c_data_to_sexp(c_obj));
      }
    }
    case sexp_hashtable:{
      hash_table *hash=obj.val.hashtable;
      CORD_sprintf(&retval,"#<hash-table :test %r :entries %d>",
                   hashtable_test_fn_name(obj),hash->entries);
      return retval;
    }
    case sexp_env:{
      return "#<environment>";
      /*
      switch(obj.val.cur_env->tag){
        case sexp_local:
          return "#<local environment>";
        case sexp_global:
          return "#<global environment>";
        case sexp_funArgs:
          return "#<function arguments environment>";
        case sexp_obEnv:
          return "#<obarray environment>";
      }
      */
    }
    default:
      CORD_sprintf(&error_str,"print error got type %s",typeName(obj));
      return error_str;
  }
}
sexp lisp_print(sexp obj){
  CORD print_string = print(eval(obj,topLevelEnv));
  CORD_printf("%r",print_string);
  return obj;
}
sexp lisp_pprint(sexp obj){
  CORD print_string = print(eval(obj,topLevelEnv));
  CORD_printf("\n%r",print_string);
  return NIL;
}
sexp lisp_print_to_string(sexp obj){
  CORD print_string = print(eval(obj,topLevelEnv));
  return string_sexp(print_string);
}
sexp lisp_println(sexp obj){
  CORD print_string = print(eval(obj,topLevelEnv));
  CORD_fprintf(stderr,"%r\n",print_string);
  return obj;
}
sexp lisp_fprint(sexp obj, sexp file){
  lisp_fputs(lisp_print(obj),file);
  return obj;
}
sexp lisp_fprintln(sexp obj, sexp file){
  lisp_fputs(lisp_println(obj),file);
  return obj;
}
sexp make_string_input_stream(sexp str){
  if(!STRINGP(str)){
    return format_type_error("make-string-stream","string",str.tag);
  }
  FILE *retval=fmemopen(CORD_to_char_star(str.val.cord),
                        CORD_len(str.val.cord),"r");
  if(retval){
    return stream_sexp(retval);
  } else {
    int errno_save=errno;
    return error_sexp(CORD_cat("fmemopen_error: ",strerror(errno_save)));
  }
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
    mk_tok_name(TOK_CHAR);
    mk_tok_name(TOK_COLON);
    mk_tok_name(TOK_COMMA);
    mk_tok_name(TOK_COMMENT_END);
    mk_tok_name(TOK_COMMENT_START);
    mk_tok_name(TOK_DBL_LBRACE);
    mk_tok_name(TOK_DBL_RBRACE);
    mk_tok_name(TOK_DOT);
    mk_tok_name(TOK_EOF);
    mk_tok_name(TOK_ID);
    mk_tok_name(TOK_INT);
    mk_tok_name(TOK_LAMBDA);
    mk_tok_name(TOK_LBRACE);
    mk_tok_name(TOK_LCBRACE);
    mk_tok_name(TOK_LET);
    mk_tok_name(TOK_LISP_FALSE);
    mk_tok_name(TOK_LISP_TRUE);
    mk_tok_name(TOK_LPAREN);
    mk_tok_name(TOK_MACRO);
    mk_tok_name(TOK_QUASI);
    mk_tok_name(TOK_QUOTE);
    mk_tok_name(TOK_RBRACE);
    mk_tok_name(TOK_RCBRACE);
    mk_tok_name(TOK_REAL);
    mk_tok_name(TOK_RPAREN);
    mk_tok_name(TOK_SPECIAL);
    mk_tok_name(TOK_STRING);
    mk_tok_name(TOK_TYPEDEF);
    mk_tok_name(TOK_TYPEINFO);
    mk_tok_name(TOK_LIST_SPLICE);
    default:
      return "forgot to implemnt that token";
  }
}
sexp lisp_get_signature(sexp fun_or_macro){
  if(!FUNCTIONP(fun_or_macro) && !MACROP(fun_or_macro)){
    return format_type_error_opt2
      ("signature","function","macro",fun_or_macro.tag);
  } else {
    return cord_sexp(get_signature(fun_or_macro.val.fun));
  }
}
sexp lisp_get_docstring(sexp lisp_symbol){
  if(!SYMBOLP(lisp_symbol)){
    return format_type_error("documentation","symbol",lisp_symbol.tag);
  } else {
    return cord_sexp(get_docstring(lisp_symbol.val.var));
  }
}
CORD prin1(sexp obj);//print readably
CORD princ(sexp obj);//pretty print
//I'll need to see how this is done elsewhere first
//use the environment's data stack
#if 0
CORD print_circle(sexp obj){
  return print_circle_sub(obj,NULL,0);
}
CORD print_circle_sub(sexp obj,cons *prev,int index){
  if(!CONSP(obj)){
    return print(obj);
  } else {
    int i;
    for(i=0;i<index;i++){
      if(obj.val.cons==prev[i]){
        return CORD_asprintf("#%d#",%i);
      }
    }
    int size=8,i=0;
    sexp *buf=alloca(8*sizeof(sexp));
    while(CONSP(obj)){
      if(size<i){
        sexp *temp=alloca((size*=2)*sizeof(sexp));
        memcpy(temp,buf,size>>1);
        buf=temp;
      }
      buf[i++]=POP(obj);
    }
  }
}
#endif
