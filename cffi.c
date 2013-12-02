#include "common.h"
#include "prim.h"
//not sure the point of this
/*void fill_c_ptr(c_ptr *pointer,ctype_val ctype_data){
  ctype_val *pointers=xmalloc(sizeof(pointer->depth));
  int i,n=pointer->depth-1;
  for(i=0;i<n;i++){
    pointers[i].pointer=pointers+(i+1);
  }
  pointers[n]=ctype_data;
  pointer->c_data=pointers;
  return;
  }*/
sexp make_c_ptr(sexp c_value,sexp deg_of_indir){
  if(!INTP(deg_of_indir)){
    return format_type_error("make-cpointer","integer",deg_of_indir.tag);
  }
  //check that c_value is of some c type
  ctype_val* pointer_mem=xmalloc(sizeof(ctype_val)*deg_of_indir.val.int64+1);
  int i=0;
  //because sexp data and ctype_vals are both unions of the same size
  //I should be fine just setting the c_value to the sexp value
  pointer_mem[i]=*(ctype_val*)&c_value.val;
  for(i=1;i<deg_of_indir.val.int64+1;i++){
    pointer_mem[i].pointer=pointer_mem+(i-1);
  }
  c_data *retval=xmalloc(sizeof(c_data));
  *retval=(c_data){.val={.pointer=pointer_mem},.type=c_value.tag,
                   .ptr_depth=deg_of_indir.val.int64};
  return c_data_sexp(retval);
}
static ctype_val dereference_c_ptr_helper(ctype_val *ptr_data,int depth){
  if(depth>1){
    return dereference_c_ptr_helper((*(ptr_data->pointer)).pointer,depth-1);
  } else {
    return *(ptr_data->pointer);
  }
}
sexp dereference_c_ptr(c_data *pointer){
  ctype_val value=
    dereference_c_ptr_helper(&pointer->val,pointer->ptr_depth);
  switch(pointer->type){
    case _ctype_int8:
      return int_n_sexp(value.ctype_int8,8);
    case _ctype_int16:
      return int_n_sexp(value.ctype_int16,16);
    case _ctype_int32:
      return int_n_sexp(value.ctype_int32,32);
    case _ctype_int64:
      return long_sexp(value.ctype_int64);
    case _ctype_uint8:
      return uint_n_sexp(value.ctype_uint8,8);
    case _ctype_uint16:
      return uint_n_sexp(value.ctype_uint16,16);
    case _ctype_uint32:
      return uint_n_sexp(value.ctype_uint32,32);
    case _ctype_uint64:
      return uint_n_sexp(value.ctype_uint64,64);
    case _ctype_float:
      return float_sexp(value.ctype_float);
    case _ctype_double:
      return double_sexp(value.ctype_double);
    case _ctype_mpz:
      return bigint_sexp(value.ctype_mpz); 
    case _ctype_mpfr:
      return bigfloat_sexp(value.ctype_mpfr);
    case _ctype_FILE:
      return stream_sexp(value.ctype_file);
    case _ctype_struct:
      return opaque_sexp(value.ctype_struct);
  }
}
int pointer_typecheck(sexp pointer,int depth,enum ctype_kind type){
  if(pointer.tag == _cdata){
    c_data *ptr=pointer.val.c_val;
    if(ptr->ptr_depth == depth){
      if(ptr->type == type){
        return 1;
      }
    }
  }
  return 0;
}
//I wrote all of this, but it's probably not necessary
#include "regex.h"
sexp get_c_type(sexp ctype_keysym){
  if(!KEYWORDP(ctype_keysym)){
    return error_sexp("argument to get_c_type must be a keyword symbol");
  }
  re_set_syntax(0);
  size_t typename_len=CORD_len(ctype_keysym.val.keyword->name)-1;
  const char *typename=CORD_to_char_star
    (CORD_substr(ctype_keysym.val.keyword->name,1,typename_len));
  regex_t *c_type_regex=xmalloc(sizeof(regex_t));
  uint32_t num_registers=8;
  struct re_registers match_data;
  //1=real<x>_ptr?;2=<x>;3=_ptr?;4=u?int<x>_ptr?;5=u?;6=<x>;7=_ptr?
  const char *c_type_pattern="\\([rR]eal\\(64\\|32\\)\\(_ptr\\)?\\)\\|"
    "\\(\\([uU]\\)?[iI]nt\\(8\\|16\\|32\\|64\\)\\(_ptr\\)?\\)";
  size_t pat_len=strlen(c_type_pattern);
  const char *re_error=re_compile_pattern(c_type_pattern,pat_len,c_type_regex);
  if(re_error){
    return error_sexp(CORD_from_char_star(re_error));
  }
  size_t match_len=re_match(c_type_regex,typename,strlen(typename),0,&match_data);
  if(match_len != typename_len){    
    HERE();
    goto FAIL;
  }
  sexp retval;
  if(match_data.start[1] != -1){//matched a real
    if(match_data.start[3] != -1){
      if(match_data.end[1] != 11){goto FAIL;}        
      //return pointer
    } else {
      if((match_data.end[1]) != 6){
        printf("start=%zu\n",match_data.start[1]);
        printf("end=%zu\n",match_data.end[1]);
        HERE();goto FAIL;}
      switch(typename[match_data.start[2]]){
        case '3':
          retval=Qreal32;
          break;
        case '6':
          retval=Qreal64;
          break;
        default:
          HERE();
          goto FAIL;
      }
      return retval;
    }
  } else if(match_data.start[4] != -1){
    if(match_data.start[7] != -1){
    } else {
      if(match_data.start[5] != -1){
        switch(typename[match_data.start[6]]){
          case '8':
            if(match_data.end[4]!=5){goto FAIL;}
            retval=Quint8;
            break;
          case '1':
            if(match_data.end[4]!=6){goto FAIL;}
            retval=Quint16;
            break;
          case '3':
            if(match_data.end[4]!=6){goto FAIL;}
            retval=Quint32;
            break;
          case '6':
            if(match_data.end[4]!=6){goto FAIL;}
            retval=Quint64;
          default:
            HERE();
            goto FAIL;
        }
        return retval;
      } else {
        switch(typename[match_data.start[6]]){
          case '8':
            if(match_data.end[4]!=4){goto FAIL;}
            return Qint8;
          case '1':
            if(match_data.end[4]!=5){goto FAIL;}
            return Qint16;
          case '3':
            if(match_data.end[4]!=5){goto FAIL;}
            return Qint32;
          case '6':
            if(match_data.end[4]!=5){goto FAIL;}
            return Qint64;
          default:
            HERE();
            goto FAIL;
        }
      }
    }
  }
 FAIL:
  return error_sexp("invalid typename passed to get_c_type");
}
