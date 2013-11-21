#include "common.h"
#include "cons.h"
#include <dlfcn.h>
static sexp get_c_sexp(ctype_val c_value,enum ctype_kind ctype_tag);
typedef ctype_val(*dlfun_t)();
static sexp call_c_fun(dlfun_t c_fun,uint64_t numargs,sexp rettype,
                       ctype_val c_args[3],enum ctype_kind c_argtypes[3]);
#pragma GCC diagnostic ignored "-Wenum-compare"
static struct sigaction old_abort_action;
static void __attribute__((noreturn)) handle_abort(int signal){
  sigaction(SIGABRT,&old_abort_action,NULL);
  longjmp(error_buf,-1);
}
static const struct sigaction abort_action_object={.sa_handler=&handle_abort};
static const struct sigaction* restrict abort_action=&abort_action_object;
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
      //we really shouldn't get here
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
