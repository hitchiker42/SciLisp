#include "common.h"
#define op_to_fun(op,fun_name,type)\
  static inline type fun_name(type x,type y){   \
    return x op y;                              \
  }
op_to_fun(+,lisp_fadd,double);
op_to_fun(-,lisp_fsub,double);
op_to_fun(*,lisp_fmul,double);
op_to_fun(/,lisp_fdiv,double);
op_to_fun(+,lisp_iadd,long);
op_to_fun(-,lisp_isub,long);
op_to_fun(*,lisp_imul,long);
op_to_fun(/,lisp_idiv,long);
op_to_fun(^,lisp_xor,long);
op_to_fun(>>,lisp_rshift,long);
op_to_fun(>>,lisp_lshift,long);
op_to_fun(&,lisp_logand,long);
op_to_fun(|,lisp_logor,long);
#define makeSyms(symName,fun_name,lisp_name)    \
  symref symName={lisp_name,(sexp){_fun,(data)(void*)fun_name}};        \
  symref* symName##ptr=&symName
#define initPrims()\
  makeSyms(fadd_sym,lisp_fadd,"+");\
  makeSyms(fsub_sym,lisp_fsub,"-");\
  makeSyms(fmul_sym,lisp_fmul,"*");\
  makeSyms(fdiv_sym,lisp_fdiv,"/");\
  addSym(fadd_symptr);\
  addSym(fsub_symptr);\
  addSym(fmul_symptr);\
  addSym(fdiv_symptr)
  
//This is a mess
/*#include "common.h"
/*
  va_list ap;
  sexp x;
  while((x=va_arg(ap,sexp)) != NIL){
    do something
  }
  call va_end() if you feel like it
  return
/
//void init_prims(){
#define get_double(sexp_name,var_name)                        \
  switch(sexp_name.tag){                                 \
    case _double:                                   \
      var_name=sexp_name.val.real64;                          \
      break;                                        \
    case _long:                                     \
      var_name=(double)sexp_name.val.int64;                   \
      break;                                        \
    case _sym:                                                  \
      get_double(sexp_name.val.var->val);                                \
      break;                                                    \
    default:                                                    \
      fprintf(stderr,"Error invalid type expected double or int got %s" \
              ,tagString(sexp_name.tag));                                    \
      return NIL;                                                       \
  }
#define get_long(sexp_name,var_name)                        \
  switch(sexp_name.tag){                                 \
    case _long:                                   \
      var_name=sexp_name.val.int64;                          \
      break;                                       \
  case _sym:                                       \
    get_long(sexp_name.val.var->val);                    \
    break;                                         \
  }
 static inline _tag get_tag(sexp var){
  return(var.tag == _sym ? get_tag(var.val.var->val) : var.tag);
}
#define binop_real(op,name)                     \
  static const sexp name(sexp x,sexp y){                     \
  double xval,yval,retval;                      \
  get_double(x,xval);                           \
  get_double(y,yval);                           \
  retval=xval op yval;                          \
  return (sexp){_double,retval};                \
}
#define binop_long(op,name)                     \
  static const sexp name(sexp x,sexp y){                     \
  long xval,yval,retval;                      \
  get_long(x,xval);                           \
  get_long(y,yval);                           \
  retval=xval op yval;                          \
  return (sexp){_long,retval};                \
}
binop_real(+,lisp_fadd);
binop_real(-,lisp_fsub);
binop_real(*,lisp_fmul);
binop_real(/,lisp_fdiv);
binop_long(+,lisp_iadd);
binop_long(-,lisp_isub);
binop_long(*,lisp_imul);
binop_long(/,lisp_idiv);
binop_long(^,lisp_xor);
binop_long(>>,lisp_rshift);
binop_long(>>,lisp_lshift);
binop_long(&,lisp_logand);
binop_long(|,lisp_logor);
#define poly_op(name)                                   \
  static const sexp poly_##name(sexp x,sexp y){                \
    if(get_tag(x) == _double || get_tag(y) == _double){ \
      return lisp_f##name(x,y);                         \
    } else {return lisp_i##name(x,y);}                  \
  }
poly_op(add);poly_op(sub);poly_op(mul);poly_op(div);
#define mkVars(name)                            \
  sexp_binop name##_fp=name;
  mkVars(poly_add)
    mkVars(poly_sub)
    mkVars(poly_div)
    mkVars(poly_mul)
#define mkRef(lisp_name,cname)                  \
  symref __##cname = {lisp_name,(sexp){_fun,(data)cname}}
#define internRef(lisp_name)                    \
  addSym((&lisp_name))
#define intern_prims()                        \
  internRef(__poly_add_fp);                      \
  internRef(__poly_sub_fp);                      \
  internRef(__poly_mul_fp);                      \
  internRef(__poly_div_fp);
void init_prims(){
  mkRef("+",poly_add_fp);
  mkRef("-",poly_sub_fp);
  mkRef("*",poly_mul_fp);
  mkRef("/",poly_div_fp);
  intern_prims();
}
*/
