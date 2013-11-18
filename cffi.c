#include "common.h"
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
    regfree(c_type_regex);
    return error_sexp(CORD_from_char_star(re_error));
  }
  //  regoff_t *re_starts=malloc(num_registers+1*sizeof(regoff_t));
  //  regoff_t *re_ends=malloc(num_registers+1*sizeof(regoff_t));
  //we do this because by default the register data gets allocated with malloc
  //and that conflicts with gc(the pattern is malloced too, but theres a function
  //to clean it up(
  //  re_set_registers(c_type_regex,match_data,num_registers,re_starts,re_ends);
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
          retval=QReal32;
          break;
        case '6':
          retval=QReal64;
          break;
        default:
          HERE();
          goto FAIL;
      }
      //      free(re_starts);
      //      free(re_ends);
      free(match_data.start);
      free(match_data.end);
      regfree(c_type_regex);
      return retval;
    }
  } else if(match_data.start[4] != -1){
    if(match_data.start[7] != -1){
    } else {
      if(match_data.start[5] != -1){
        switch(typename[match_data.start[6]]){
          case '8':
            if(match_data.end[4]!=5){goto FAIL;}
            retval=QUInt8;
            break;
          case '1':
            if(match_data.end[4]!=6){goto FAIL;}
            retval=QUInt16;
            break;
          case '3':
            if(match_data.end[4]!=6){goto FAIL;}
            retval=QUInt32;
            break;
          case '6':
            if(match_data.end[4]!=6){goto FAIL;}
            retval=QUInt64;
          default:
            HERE();
            goto FAIL;
        }
        //        free(re_starts);
        //        free(re_ends);
        free(match_data.start);
        free(match_data.end);
        regfree(c_type_regex);
        return retval;
      } else {
        switch(typename[match_data.start[6]]){
          case '8':
            if(match_data.end[4]!=4){goto FAIL;}
            return QInt8;
          case '1':
            if(match_data.end[4]!=5){goto FAIL;}
            return QInt16;
          case '3':
            if(match_data.end[4]!=5){goto FAIL;}
            return QInt32;
          case '6':
            if(match_data.end[4]!=5){goto FAIL;}
            return QInt64;
          default:
            HERE();
            goto FAIL;
        }
      }
    }
  }
 FAIL:
  //  free(re_starts);
  //  free(re_ends);
  free(match_data.start);
  free(match_data.end);
  regfree(c_type_regex);
  return error_sexp("invalid typename passed to get_c_type");
}
