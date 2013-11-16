#ifndef CFFI_H
#define CFFI_H
#include "common.h"
enum ctype_kind {
  _ctype_int8 = 1,
  _ctype_int16 = 3,
  _ctype_int32 = 5,
  _ctype_int64 = 7,
  _ctype_uint8 = 2,
  _ctype_uint16 = 4,
  _ctype_uint32 = 6,
  _ctype_uint64 = 8,
  _ctype_float = 9,_ctype_real32=9,
  _ctype_double = 10,_ctype_real64=10,
  //special cases of structs
  _ctype_FILE = 24,
  _ctype_mpz = 11,
  _ctype_mpfr = 12,
  _ctype_struct = 46,//pointer to opaque struct
};
union ctype_val{
  int8 ctype_int8;
  int16 ctype_int16;
  int32 ctype_int32;
  int64 ctype_int64;
  uint8 ctype_uint8;
  uint16 ctype_uint16;
  uint32 ctype_uint32;
  uint64 ctype_uint64;
  float ctype_float;
  float ctype_real32;
  double ctype_double;
  double ctype_real64;
  FILE *ctype_file;
  mpz_t *ctype_mpz;
  mpfr_t *ctype_mpz;
  void *ctype_struct;
  union ctype_val *pointer;
};
struct c_ptr {
  union ctype_val *c_data;//actual pointer value
  int depth;//degree of inderection(*=1,**=2,etc)
  enum ctype_kind type;//actual type,
};
//not sure the point of this
void fill_c_ptr(c_ptr *pointer,union ctype_val){
  union ctype_val *pointers=xmalloc(sizeof(pointer->depth));
  int i,n=pointer->depth-1;
  for(i=0;i<n;i++){
    pointers[i]=pointers+(i+1);
  }
  pointers[n]=ctype_val;
  c_ptr->c_data=pointers;
  return;
}
sexp dereferance_c_ptr(c_ptr *pointer){
  union ctype_val value=
    dereferance_c_ptr_helper(pointer->c_data,pointer->depth);
  switch(pointer->type){
    case _ctype_int8:
      return int_n_sexp(value.ctype_int8,8);
    case _ctype_int16;
      return int_n_sexp(value.ctype_int16,16);
    case _ctype_int32:
      return int_n_sexp(value.ctype_int32,32);
    case _ctype_int64:
      return long_sexp(value.ctype_int64);
    case _ctype_uint8:
      return uint_n_sexp(value.ctype_uint8,8);
    case _ctype_uint16;
      return uint_n_sexp(value.ctype_uint16,16);
    case _ctype_uint32:
      return uint_n_sexp(value.ctype_uint32,32);
    case _ctype_uint64:
      return uint_n_sexp(value.ctype_uint64,64);
    case _ctype_float:
      return float_sexp(value.ctype_float);
    case _ctype_double:
      return doubel_sexp(value.ctype_double);
    case _ctype_mpz:
      return bigint_sexp(value.ctype_mpz); 
    case _ctype_mpfr:
      return bigint_sexp(value.ctype_mpfr);
    case _ctype_FILE:
      return stream_sexp(value.ctype_file);
    case _ctype_struct:
      return opaque_sexp(value.ctype_struct);
  }
}
union ctype_val dereferance_c_ptr_helper(union ctype_val *c_data,int depth){
  if(depth>1){
    return dereferance_c_ptr_helper(*(c_data->pointer),depth-1,type);
  } else {
    return *(c_data->pointer);
  }
}
int pointer_typecheck(sexp pointer,int depth,enum ctype_kind type){
  if(pointer.tag == _cptr){
    c_ptr *ptr=pointer.val.c_ptr;
    if(ptr->depth == depth){
      if(ptr->type == type){
        return 1;
      }
    }
  }
  return 0;
}
#endif
