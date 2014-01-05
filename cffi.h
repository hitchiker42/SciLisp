#ifndef CFFI_H
#define CFFI_H
#include "ffi.h"
enum ctype_kind {
  _ctype_int8 = _int8,
  _ctype_int16 = _int16,
  _ctype_int32 = _int32,
  _ctype_int64 = _int64,
  _ctype_uint8 = _uint8,
  _ctype_uint16 = _uint16,
  _ctype_uint32 = _uint32,
  _ctype_uint64 = _uint64,
  _ctype_float = _float,_ctype_real32=_real32,
  _ctype_double = _double,_ctype_real64=_real64,
  //special cases of structs
  _ctype_FILE = _file,
  _ctype_mpz = _bigint,
  _ctype_mpfr = _bigfloat,
  _ctype_struct = _opaque,//pointer to opaque struct
};
union ctype_val{
  int8_t ctype_int8;
  int16_t ctype_int16;
  int32_t ctype_int32;
  int64_t ctype_int64;
  uint8_t ctype_uint8;
  uint16_t ctype_uint16;
  uint32_t ctype_uint32;
  uint64_t ctype_uint64;
  float ctype_float;
  float ctype_real32;
  double ctype_double;
  double ctype_real64;
  FILE *ctype_file;
  mpz_t *ctype_mpz;
  mpfr_t *ctype_mpfr;
  void *ctype_struct;
  ctype_val *pointer;  
};
struct c_data {
  ctype_val val;//actual pointer value
  enum ctype_kind type;//actual type
  int ptr_depth;//degree of inderection(*=1,**=2,etc)
};
struct my_closure{
  void * fun;
  ffi_closure *closure;
};
sexp dereference_c_ptr(c_data *pointer);
int pointer_typecheck(sexp pointer,int depth,enum ctype_kind type);
sexp ccall(sexp function,sexp libname,sexp rettype,sexp argtypes,sexp args,sexp thread);
sexp get_c_type(sexp ctype_keysym);
void prep_sexp_cifs();
void *make_closure(sexp lambda,sexp fun_env,int numargs);
static inline sexp c_data_to_sexp(c_data* obj){
  return (sexp){.val={.uint64=obj->val.ctype_uint64},.tag=obj->type};
}
#endif
