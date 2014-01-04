#include "common.h"
#include "prim.h"
#include "ffi.h"
sexp make_c_ptr(sexp c_value,sexp deg_of_indir){
  int indir=1;
  if(NILP(deg_of_indir)){
    indir=1;
  } else if(!INTP(deg_of_indir)){
    return format_type_error("make-cpointer","integer",deg_of_indir.tag);
  } else {
    indir=deg_of_indir.val.int64;
  }
  //check that c_value is of some c type
  ctype_val* pointer_mem=xmalloc(sizeof(ctype_val)*indir+1);
  int i=0;
  //because sexp data and ctype_vals are both unions of the same size
  //I should be fine just setting the c_value to the sexp value
  pointer_mem[i]=*(ctype_val*)&c_value.val;
  for(i=1;i<indir+1;i++){
    pointer_mem[i].pointer=pointer_mem+(i-1);
  }
  c_data *retval=xmalloc(sizeof(c_data));
  *retval=(c_data){.val={.pointer=pointer_mem},.type=c_value.tag,
                   .ptr_depth=indir};
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
    dereference_c_ptr_helper(pointer->val.pointer,pointer->ptr_depth);
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
sexp lisp_dereference_c_ptr(sexp c_val){
  if(!CDATAP(c_val)){
    return format_type_error("dereference-c-ptr","c-data",c_val.tag);
  } else {
    return dereference_c_ptr(c_val.val.c_val);
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
sexp get_c_type(sexp ctype_keysym){
  return getKeywordType(ctype_keysym);
}
/*an ffi closure is a struct of the form
{...,
 ffi_cif *cif,
 void (*fun)(ffi_cif*,void*,void**,void*);
 void *user_data;
}
*/
ffi_type *sexp_type_elements[3]={&ffi_type_pointer,&ffi_type_uint64,NULL};
ffi_type sexp_type={.size=0,.alignment=0,.type=FFI_TYPE_STRUCT,
                    .elements=sexp_type_elements};
ffi_cif sexp_0;
ffi_cif sexp_1;
ffi_cif sexp_2;
ffi_cif sexp_3;
ffi_cif sexp_4;
void prep_sexp_cifs(){
  ffi_type *sexp_types[4]={&sexp_type,&sexp_type,&sexp_type,&sexp_type};
  ffi_prep_cif(&sexp_0,FFI_DEFAULT_ABI,0,&sexp_type,sexp_types);
  ffi_prep_cif(&sexp_1,FFI_DEFAULT_ABI,1,&sexp_type,sexp_types);
  ffi_prep_cif(&sexp_2,FFI_DEFAULT_ABI,2,&sexp_type,sexp_types);
  ffi_prep_cif(&sexp_3,FFI_DEFAULT_ABI,3,&sexp_type,sexp_types);
  ffi_prep_cif(&sexp_4,FFI_DEFAULT_ABI,4,&sexp_type,sexp_types);
  return;
}
void sexp_closure_call(ffi_cif *CIF,void *RET,void **ARGS,void *USER_DATA){
  sexp *retval=(sexp*)RET;
  sexp **args=(sexp**)ARGS;
  int numargs=CIF->nargs;
  sexp arglist=NIL;
  if(numargs){
    retval=xmalloc(sizeof(sexp));
    int i;
    for(i=0;i<numargs;i++){
      arglist=Cons(*args[numargs-i],arglist);
    }
  }
  cons *fun_and_env=(cons*)USER_DATA;
  sexp lambda_fun=fun_and_env->car;
  sexp lambda_env=fun_and_env->cdr;
  *retval=call_lambda(Cons(lambda_fun,arglist),lambda_env.val.cur_env);
  return;
}
//returns a pointer to the ffi closure, which containes the
//desired function pointer, we can't just return a function pointer
//because the closure structure must be explicitly deallocated because
//it needs to do some tricks to allocate executable memory
ffi_closure *make_closure(sexp lambda,sexp fun_env,int numargs){
  void *code;
  ffi_closure *closure;
  closure=ffi_closure_alloc(sizeof(ffi_closure),&code);
  cons* data=xmalloc(sizeof(cons));
  data->car=lambda;
  data->cdr=fun_env;
  ffi_status status;
  switch(numargs){
    case 0:
      status=ffi_prep_closure_loc(closure,sexp_0,sexp_closure_call,
                                  (void*)data,code);
      break;
    case 1:
      status=ffi_prep_closure_loc(closure,sexp_1,sexp_closure_call,
                           (void*)data,code);
      break;
    case 2:
      status=ffi_prep_closure_loc(closure,sexp_2,sexp_closure_call,
                           (void*)data,code);
      break;
    case 3:
      status=ffi_prep_closure_loc(closure,sexp_3,sexp_closure_call,
                                  (void*)data,code);
      break;
    case 4:
      status=ffi_prep_closure_loc(closure,sexp_4,sexp_closure_call,
                                  (void*)data,code);
      break;
    default:
      ffi_closure_free(closure);
      return NULL;
  }
  if(status!=FFI_OK){
    ffi_closure_free(closure);
    return NULL;
  } else {
    return closure;
  }
}
#define get_closure_fun(closure) (closure->fun)
