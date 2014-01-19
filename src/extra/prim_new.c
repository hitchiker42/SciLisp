#include "scilisp.h"
#include <locale.h>
#include <langinfo.h>
#define DEFUN(l_name,c_name,reqargs,optargs,keyargs,restarg,max_args,sig) \
  function c_name##_fun=                                                \
    {.req_args=reqargs,num_opt_args=optargs,num_keyword_args=keyargs,   \
     .has_rest_arg=restarg,.maxargs=max_args,                           \
     .lname=l_name,.cname=#c_name,                                      \
     .comp = {.f##maxargs=c_name}, .signature=sig,                      \
     .type = fun_compiled};
//compilier macros take their arguments unevaluated
#define DEFMACRO(l_name,c_name,sig)                             \
  function c_name##_fun=                                        \
    {.type=fun_compiler_macro,.lname=l_name,.cname=c_name,      \
     .funcall={funevaled=c_name},.signature=sig}
//special forms, same as compiler macros except for the type
#define DEFSPECIAL(l_name,c_name,sig)                             \
  function c_name##_fun=                                        \
    {.type=fun_special_form,.lname=l_name,.cname=c_name,      \
     .funcall={funevaled=c_name},.signature=sig}
