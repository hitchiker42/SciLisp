#include "common.h"
#include "cons.h"
#include "prim.h"
#include <dlfcn.h>
#include "ffi.h"
static struct sigaction old_abort_action;
static void __attribute__((noreturn)) handle_abort(int signal){
  sigaction(SIGABRT,&old_abort_action,NULL);
  longjmp(error_buf,-1);
}
static const struct sigaction abort_action_object={.sa_handler=&handle_abort};
static const struct sigaction* restrict abort_action=&abort_action_object;
//using libffi
static inline ffi_type* get_ffi_type_sub(_tag type_tag){
  switch(type_tag){
    case _nil: return &ffi_type_void;
    case _int8: return &ffi_type_sint8;
    case _uint8: return &ffi_type_uint8;
    case _int16: return &ffi_type_sint16;
    case _uint16: return &ffi_type_uint16;
    case _int32: return &ffi_type_sint32;
    case _uint32: return &ffi_type_uint32;
    case _int64: return &ffi_type_sint64;
    case _uint64: return &ffi_type_uint64;
    case _real32: return &ffi_type_float;
    case _real64: return &ffi_type_double;
      //now all of the pointers
    case _opaque:
    case _bigint:
    case _bigfloat:
    case _cord:
    case _cons:
    case _list:
    case _array:
    case _re_data:
    case _sfmt:
      //I doubt anything else should be passed to c
      return &ffi_type_pointer;
    default:
      return NULL;
  }
}
static inline ffi_cif* prep_ffi_cif(_tag *arg_tags,_tag ret_tag, int numargs){
  ffi_cif *CIF=xmalloc(sizeof(ffi_cif));
  ffi_status status;
  ffi_type *ret_type=get_ffi_type_sub(ret_tag);
  if(numargs == 0){
    ffi_status status=ffi_prep_cif(CIF,FFI_DEFAULT_ABI,numargs,ret_type,NULL);
  } else {
    ffi_type** arg_types=xmalloc(sizeof(ffi_type*)*numargs);
    int i;
    for(i=0;i<numargs;i++){
      arg_types[i]=get_ffi_type_sub(arg_tags[i]);
    }
    ffi_status status=ffi_prep_cif(CIF,FFI_DEFAULT_ABI,numargs,ret_type,arg_types);
  }
  if(status != FFI_OK){
    return NULL;
  } else {
    return CIF;
  }
}
static inline void* get_dl_fun(CORD funname_cord,CORD libname){
  const char* funname=CORD_to_const_char_star(funname_cord);
  char *dllibname;
  void* dllib;
  void* dlfun;
  //really basic option parsing
  //-l<lib> -> lib<lib>.so
  if(!CORD_ncmp(libname,0,"-l",0,2)){
    dllibname=(char*)CORD_to_const_char_star
      (CORD_catn(3,"lib",CORD_substr
                 (libname,2,CORD_len(libname)-2),".so"));
  }
  //lib<name>-> lib<name>.so
  if(!CORD_ncmp(libname,0,"lib",0,3)){
    dllibname=(char*)CORD_to_const_char_star(CORD_cat(libname,".so"));
  } else {
    dllibname=(char*)CORD_to_const_char_star(libname);
  }
  dllib=dlopen(dllibname,RTLD_LAZY);
  if(!dllib){
    return NULL;//call dlerror from calling function if this happens
  }
  dlfun=dlsym(dllib,funname);
  if(!dlfun){
    return NULL;
  }
  return dlfun;
}
static inline void *ffi_get_c_arg(_tag type_tag,sexp val){
  void *retval=xmalloc(sizeof(void*));
  switch(type_tag){
    case _ctype_int8:
    case _ctype_int16:
    case _ctype_int32:
    case _ctype_int64:
      *(int64_t*)retval=val.val.int64;
    case _ctype_uint8:
    case _ctype_uint16:
    case _ctype_uint32:
    case _ctype_uint64:
      *(uint64_t*)retval=val.val.uint64;
    case _ctype_float:
    case _ctype_double:
      //we really shouldn't get here...why
      *(real64_t*)retval=val.val.real64;
  //special cases of structs
    case _ctype_FILE:
      *(FILE**)retval=val.val.stream;
    case _ctype_mpz:
      *(mpz_t**)retval=val.val.bigint;
    case _ctype_mpfr:
      *(mpfr_t**)retval=val.val.bigfloat;
    case _ctype_struct:
      *(void**)retval=val.val.opaque;
  }
  return retval;
}
static inline sexp make_sexp(void *val,_tag type){
  switch(type){
    case _int8: return int_n_sexp(*(int8_t*)val,8);
    case _int16: return int_n_sexp(*(int16_t*)val,16);
    case _int32: return int_n_sexp(*(int32_t*)val,32);
    case _int64: return int_n_sexp(*(int64_t*)val,64);
    case _uint8: return uint_n_sexp(*(uint8_t*)val,8);
    case _uint16: return uint_n_sexp(*(uint16_t*)val,16);
    case _uint32: return uint_n_sexp(*(uint32_t*)val,32);
    case _uint64: return uint_n_sexp(*(uint64_t*)val,64);
    case _real32: return error_sexp("real32 type unimplemented");//I need to implement this
    case _real64: return real64_sexp(*(real64_t*)val);
    case _bigint: return bigint_sexp((mpz_t*)val);
    case _bigfloat: return bigfloat_sexp((mpfr_t*)val);
    case _opaque: return opaque_sexp(val);
    default:
      return format_error_sexp("unknown tag %d",type);
  }
}
//(defun ccall (function-name lib-name returrn-type
//              argument-types arguments &optional thread)
//type: (string*string*keyword*keyword-list*list)->sexp
sexp ffi_ccall_unsafe(sexp fun_name,sexp libname,sexp rettype,
                      sexp argtypes,sexp args,sexp thread){
  //leave typechecking upto the caller
  void *fp=get_dl_fun(fun_name.val.cord,libname.val.cord);
  if(!fp){
    return error_sexp(CORD_cat("Error in libdl:\n",dlerror()));
  }
  //test
  real64_t(*f)(uint32_t)=fp;
  PRINT_FMT("fact of 4 is %f",f(4));
  //Assume the caller has verified len(argtypes)==len(args)...or not
  int numargs=argtypes.len,i;
  sexp cur_argtype;
  _tag *c_argtypes=xmalloc(sizeof(_tag)*numargs);
  _tag c_rettype;
  void **c_args=xmalloc(sizeof(void*)*numargs);
  sexp ret_argtype;
  if(NILP(rettype)){
    ret_argtype=Qnil;
  } else {
    ret_argtype=get_c_type(rettype);
    if(ERRORP(ret_argtype)){return ret_argtype;}
  }
  c_rettype=ret_argtype.val.meta;
  for(i=0;i<numargs;i++){
    if(!CONSP(args)){
      return error_sexp
        ("fewer arguments than type parameters passed to ccall");
    }
    if(!CONSP(argtypes)){
      return error_sexp
        ("fewer type parameters than arguments passed to ccall");
    }
    cur_argtype=get_c_type(XCAR(argtypes));
    if(ERRORP(cur_argtype)){return cur_argtype;}
    c_argtypes[i]=cur_argtype.val.meta;
    c_args[i]=ffi_get_c_arg(cur_argtype.val.meta,XCAR(args));
    args=XCDR(args);
    argtypes=XCDR(argtypes);
  }
  ffi_cif *CIF=prep_ffi_cif(c_argtypes,c_rettype,numargs);
  //currently we only have return types that are pointers or literal types
  //so the maximum size of the return type is 64 bits
  void *retval=xmalloc(sizeof(void*));
  //prevent an ffi call from aborting SciLisp
  //any other fatal error will still crash SciLisp atm
  sigaction(SIGABRT,abort_action,&old_abort_action);
  //ffi_call(CIF,fp,retval,&c_args_test);
  HERE();
  ffi_call(CIF,fp,retval,c_args);
  return make_sexp(retval,c_rettype);
}
sexp ffi_ccall(sexp fun_name,sexp libname,sexp rettype,
               sexp argtypes,sexp args,sexp thread){
  
  if(!STRINGP(fun_name)){
    return format_type_error_named("ccall","function-name","string",fun_name.tag);
  }
  if(!STRINGP(libname)){
    return format_type_error_named("ccall","library-name","string",libname.tag);
  }
  if(!KEYWORDP(rettype)){
    return format_type_error_named("ccall","return-type","type-keyword",rettype.tag);
  }
  if(!CONS_OR_NIL(argtypes)){
    return format_type_error_opt_named("ccall","list","argument-types",argtypes.tag);
  }
  if (!CONS_OR_NIL(args)){
    return format_type_error_opt_named("ccall","list","arguments",args.tag);   
  }
  return ffi_ccall_unsafe(fun_name,libname,rettype,argtypes,args,thread);
}
struct thread_args {
  sexp fun_name;
  sexp numargs;
  sexp rettype;
  sexp argtypes;
  sexp args;
};
static void* ffi_ccall_from_new_thread(void* ccall_args){
  struct thread_args args=*(struct thread_args*)ccall_args;
  sexp *retval=xmalloc(sizeof(sexp));
  *retval=(sexp)ffi_ccall_unsafe(args.fun_name,args.numargs,args.rettype,
                                 args.argtypes,args.args,NIL);
  return (void*)retval;
}
/* Preserved for posterity,I suppose, My hacked together horrible
   version of ccall*/
#if 0
static sexp get_c_sexp(ctype_val c_value,enum ctype_kind ctype_tag);
typedef ctype_val(*dlfun_t)();
static sexp call_c_fun(dlfun_t c_fun,uint64_t numargs,sexp rettype,
                       ctype_val c_args[3],enum ctype_kind c_argtypes[3]);
#pragma GCC diagnostic ignored "-Wenum-compare"
struct thread_args {
  dlfun_t c_fun;
  uint64_t numargs;
  sexp rettype;
  ctype_val *c_args;
  enum ctype_kind *c_argtypes;
};
static void* call_c_fun_from_new_thread(void* ccall_args);
//just call a c function, unsafe, no typechecking and not very user frendly
//argtypes should be keyword symbols
sexp ccall(sexp function,sexp libname,sexp rettype,
           sexp argtypes,sexp args,sexp thread){
  if(!STRINGP(function) || !(STRINGP(libname)) || !KEYWORDP(rettype) ||
     !(CONS_OR_NIL(argtypes)) || !(CONS_OR_NIL(args))){
    return error_sexp("type error in ccall");
  }
  int thread_action;
  if(NILP(thread)){
    thread_action=0;
  } else {
    thread_action=thread.val.int64;
    if(thread_action>2){
      return error_sexp("invalid thread/process option");
    }
  }
  ctype_val c_args[3];
  enum ctype_kind c_argtypes[3];
  char* dllibname;
  void* dllib;
  dlfun_t dlfun;
  uint64_t num_args=(NILP(argtypes)?0:cons_length(argtypes).val.int64);
  //really basic option parsing
  if(!CORD_ncmp(libname.val.cord,0,"-l",0,2)){
    dllibname=(char*)CORD_to_const_char_star
      (CORD_catn(3,"lib",CORD_substr
                 (libname.val.cord,2,CORD_len(libname.val.cord)-2),".so"));
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
  dlfun=(dlfun_t)dlsym(dllib,CORD_to_const_char_star(function.val.cord));
  if(!dlfun){
    return error_sexp(CORD_from_char_star(dlerror()));
  }
  int i;
  sexp cur_argtype;
  for(i=0;i<num_args;i++){
    if(!CONSP(args) || !CONSP(argtypes)){
      return error_sexp("fewer arguments than type parameters passed to ccall");
    }
    cur_argtype=get_c_type(XCAR(argtypes));
    if(ERRORP(cur_argtype)){return cur_argtype;}
    c_argtypes[i]=cur_argtype.val.meta;
    c_args[i].ctype_uint64=XCAR(args).val.uint64;
    args=XCDR(args);
    argtypes=XCDR(argtypes);
  }
  sigaction(SIGABRT,abort_action,&old_abort_action);
  switch(thread_action){
    case 0:
      return call_c_fun(dlfun,num_args,rettype,c_args,c_argtypes);
    case 1:{
      pthread_t new_thread;
      struct thread_args args={.c_fun=dlfun,.numargs=num_args,
                               .rettype=rettype,.c_args=c_args,
                               .c_argtypes=c_argtypes};
      pthread_create(&new_thread,NULL,call_c_fun_from_new_thread,(void*)&args);
      sexp *retval=alloca(sizeof(sexp));
      pthread_join(new_thread,(void**)&retval);
      return *retval;
    }
    case 2:
      return NIL;
      //do stuff to fork a new process
  }
}
static void* call_c_fun_from_new_thread(void* ccall_args){
  struct thread_args args=*(struct thread_args*)ccall_args;
  sexp *retval=xmalloc(sizeof(sexp));
  *retval=(sexp)call_c_fun(args.c_fun,args.numargs,args.rettype,
                           args.c_args,args.c_argtypes);
  return (void*) retval;
}
#define IS_FLOAT_TYPE(obj) (obj==_real32 || obj==_real64)
#define GET_ARG_VAL(index) ((c_argtypes[index]==_real64 || c_argtypes[index]==_real32) ? \
                            c_args[index].ctype_real64 : c_args[index].ctype_uint64)
#define REAL_ARG(index) c_args[index].ctype_real64
#define UINT_ARG(index) c_args[index].ctype_uint64
#define TYPE_SWITCH(get_sexp)                                           \
  switch(numargs){                                                      \
  case 0:                                                               \
  HERE();                                                               \
  return get_sexp(fp());                                                \
  case 1:                                                               \
  HERE();                                                               \
  if(IS_FLOAT_TYPE(c_argtypes[0])){                                     \
    return get_sexp(fp(REAL_ARG(0)));                                   \
  } else {                                                              \
    return get_sexp(fp(UINT_ARG(0)));                                   \
  }                                                                     \
  case 2:                                                               \
  HERE();                                                               \
  if(IS_FLOAT_TYPE(c_argtypes[0])){                                     \
    if(IS_FLOAT_TYPE(c_argtypes[1])){                                   \
      return get_sexp(fp(REAL_ARG(0),REAL_ARG(1)));                     \
    } else {                                                            \
      return get_sexp(fp(REAL_ARG(0),UINT_ARG(1)));                     \
    }                                                                   \
  } else {                                                              \
    if(IS_FLOAT_TYPE(c_argtypes[1])){                                   \
      return get_sexp(fp(UINT_ARG(0),REAL_ARG(1)));                     \
    } else {                                                            \
      return get_sexp(fp(UINT_ARG(0),UINT_ARG(1)));                     \
    }                                                                   \
  }                                                                     \
  case 3:                                                               \
  HERE();                                                               \
  if(IS_FLOAT_TYPE(c_argtypes[0])){                                     \
    if(IS_FLOAT_TYPE(c_argtypes[1])){                                   \
      if(IS_FLOAT_TYPE(c_argtypes[2])){                                 \
        return get_sexp(fp(REAL_ARG(0),REAL_ARG(1),REAL_ARG(2)));       \
      } else {                                                          \
        return get_sexp(fp(REAL_ARG(0),REAL_ARG(1),UINT_ARG(2)));       \
      }                                                                 \
    } else {                                                            \
      if(IS_FLOAT_TYPE(c_argtypes[2])){                                 \
        return get_sexp(fp(REAL_ARG(0),UINT_ARG(1),REAL_ARG(2)));       \
      } else {                                                          \
        return get_sexp(fp(REAL_ARG(0),UINT_ARG(1),UINT_ARG(2)));       \
      }                                                                 \
    }                                                                   \
  } else {                                                              \
    if(IS_FLOAT_TYPE(c_argtypes[1])){                                   \
      if(IS_FLOAT_TYPE(c_argtypes[2])){                                 \
        return get_sexp(fp(UINT_ARG(0),REAL_ARG(1),REAL_ARG(2)));       \
      } else {                                                          \
        return get_sexp(fp(UINT_ARG(0),REAL_ARG(1),UINT_ARG(2)));       \
      }                                                                 \
    } else {                                                            \
      if(IS_FLOAT_TYPE(c_argtypes[2])){                                 \
        return get_sexp(fp(UINT_ARG(0),UINT_ARG(1),REAL_ARG(2)));       \
      } else {                                                          \
        return get_sexp(fp(UINT_ARG(0),UINT_ARG(1),UINT_ARG(2)));       \
      }                                                                 \
    }                                                                   \
  }                                                                     \
  default:                                                              \
    return error_sexp                                                   \
      ("Sorry c functions with more than 3 arguments are not yet implemented"); \
  }
#define get_c_uint_sexp(c_uint) get_c_sexp(c_uint,c_rettype)
static inline sexp call_c_fun(dlfun_t c_fun,uint64_t numargs,sexp rettype,
                              ctype_val c_args[3],enum ctype_kind c_argtypes[3]){
  sexp rettype_sexp=get_c_type(rettype);
  if(ERRORP(rettype_sexp)){return rettype_sexp;};
  enum ctype_kind c_rettype=rettype_sexp.val.meta;
  HERE();
  PRINT_MSG(tag_name(c_rettype));
  if(IS_FLOAT_TYPE(c_rettype)){
    double(*fp)()=(double(*)())c_fun;
    TYPE_SWITCH(double_sexp);
  } else {
    ctype_val(*fp)()=c_fun;
    TYPE_SWITCH(get_c_uint_sexp);
  }
}

static sexp get_c_sexp(ctype_val c_value,enum ctype_kind ctype_tag){
  switch(ctype_tag){
    case _ctype_int8:
    case _ctype_int16:
    case _ctype_int32:
    case _ctype_int64:
      return long_sexp(c_value.ctype_int64);
    case _ctype_uint8:
    case _ctype_uint16:
    case _ctype_uint32:
    case _ctype_uint64:
      return ulong_sexp(c_value.ctype_uint64);
    case _ctype_float:
    case _ctype_double:
      //we really shouldn't get here...why
      return double_sexp(c_value.ctype_real64);
  //special cases of structs
    case _ctype_FILE:
      return stream_sexp(c_value.ctype_file);
    case _ctype_mpz:
      return bigint_sexp(c_value.ctype_mpz);
    case _ctype_mpfr:
      return bigfloat_sexp(c_value.ctype_mpfr);
    case _ctype_struct:
      return opaque_sexp(c_value.ctype_struct);
  }
}
#endif
