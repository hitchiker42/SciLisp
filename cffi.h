#ifndef CFFI_H
#define CFFI_H
typedef union ctype_val ctype_val;
typedef struct c_data c_data;
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
sexp dereference_c_ptr(c_data *pointer);
int pointer_typecheck(sexp pointer,int depth,enum ctype_kind type);
sexp ccall(sexp function,sexp libname,sexp rettype,sexp argtypes,sexp args);
sexp get_c_type(sexp ctype_keysym);
#endif
