#ifndef CFFI_H
#define CFFI_H
#include "ffi.h"
enum ctype_kind {
  ctype_int8 = sexp_int8,
  ctype_int16 = sexp_int16,
  ctype_int32 = sexp_int32,
  ctype_int64 = sexp_int64,
  ctype_uint8 = sexp_uint8,
  ctype_uint16 = sexp_uint16,
  ctype_uint32 = sexp_uint32,
  ctype_uint64 = sexp_uint64,
  ctype_real32=sexp_real32,
  ctype_real64=sexp_real64,
  //special cases of structs
  ctype_FILE = sexp_file,
  ctype_mpz = sexp_bigint,
  ctype_mpfr = sexp_bigfloat,
  ctype_struct = sexp_opaque,//pointer to opaque struct
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
void **make_closure(sexp lambda,sexp fun_env,int numargs);
static inline sexp c_data_to_sexp(c_data* obj){
  return (sexp){.val={.uint64=obj->val.ctype_uint64},.tag=obj->type};
}
#define make_function_pointer(f,fun_obj,arity)  \
  make_function_pointer_##arity(f,fun_obj)
#define make_function_pointer_1(f,fun_obj)                              \
  if(FUNP(fun_obj)){                                                    \
    f=sort_fn.val.fun->comp.f1;                                         \
  } else if (LAMBDAP(fun_obj)){                                         \
    closure=make_closure(fun_obj,env_sexp(cur_env_ptr),1);              \
  if(!closure){return error_sexp("error constructing ffi_closure");}    \
  f=(sexp(*)(sexp))closure[0];                                          \
  }
#define make_function_pointer_2(f,fun_obj)                              \
  if(FUNP(fun_obj)){                                                    \
    f=sort_fn.val.fun->comp.f2;                                         \
  } else if (LAMBDAP(fun_obj)){                                         \
    closure=make_closure(fun_obj,env_sexp(cur_env_ptr),2);              \
  if(!closure){return error_sexp("error constructing ffi_closure");}    \
  f=(sexp(*)(sexp,sexp))closure[0];                                     \
  }
#endif
