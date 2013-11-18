#include "common.h"
//not sure the point of this
void fill_c_ptr(c_ptr *pointer,ctype_val ctype_data){
  ctype_val *pointers=xmalloc(sizeof(pointer->depth));
  int i,n=pointer->depth-1;
  for(i=0;i<n;i++){
    pointers[i].pointer=pointers+(i+1);
  }
  pointers[n]=ctype_data;
  pointer->c_data=pointers;
  return;
}
static ctype_val dereference_c_ptr_helper(ctype_val *c_data,int depth){
  if(depth>1){
    return dereference_c_ptr_helper((*(c_data->pointer)).pointer,depth-1);
  } else {
    return *(c_data->pointer);
  }
}
sexp dereference_c_ptr(c_ptr *pointer){
  ctype_val value=
    dereference_c_ptr_helper(pointer->c_data,pointer->depth);
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
//just call a c function, unsafe, no typechecking and not very user frendly
//argtypes should be keyword symbols
//maybe make a global list of currently loaded libraries?
sexp ccall(sexp function,sexp libname,sexp rettype,sexp argtypes,sexp args){
  if(!STRINGP(function) || !(STRINGP(libname)) || !(CONSP(argtypes))
     || !(CONSP(args))){
    return error_sexp("type error in ccall");
  }
  char* dllibname;
  void* dllib;
  void* dlfun;
  //really basic option parsing
  if(!CORD_ncmp(libname.val.cord,0,"-l",0,2)){
    dllibname=(char*)CORD_to_const_char_star
      (CORD_catn(3,"lib",CORD_substr
                 (libname.val.cord,2,CORD_len(libname.val.cord)-2),".s0"));
  }
  if(!CORD_ncmp(libname.val.cord,0,"lib",0,3)){
    dllibname=(char*)CORD_to_const_char_star(CORD_cat(libname.val.cord,".so"));
  } else {
    dllibname=(char*)CORD_to_const_char_star(libname.val.cord);
  }
  dllib=dlopen(dllibname,RTLD_LAZY);
  if(!dllib){
    return error_sexp(CORD_from_char_star(dlerror()));
  }
  dlfun=dlsym(dllib,CORD_to_const_char_star(function.val.cord));
  if(!dlfun){
    return error_sexp(CORD_from_char_star(dlerror()));
  }
  return NIL;
}
