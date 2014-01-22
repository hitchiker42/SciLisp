#ifndef _SUBR_H_
#define _SUBR_H_
static inline sexp call_many_with_2(funcall f,sexp a,sexp b){
  sexp args[2]={a,b};
  return f.fmany(2,args);
}
static inline sexp  call_many_with_1(funcall f,sexp a){
  sexp args[1]={a};
  return f.fmany(1,args);
}
static inline sexp call_many_with_2_implicit(sexp a,sexp b){
  sexp (*f)(uint64_t,sexp*)=env->data_ptr->val.fun->comp.fmany;
  sexp args[2]={a,b};
  return f(2,args);
}
static inline sexp call_many_with_1_implicit(sexp a,sexp b){
  sexp (*f)(uint64_t,sexp*)=env->data_ptr->val.fun->comp.fmany;
  sexp args[1]={a};
  return f(1,args);
}
sexp call_1_with_1_optional(sexp a){
  sexp(*f)(sexp,sexp)=env->data_ptr->val.fun->comp.f2;
  return f(a,NIL);
}
sexp call_2_with_1_optional(sexp a,sexp b){
  sexp(*f)(sexp,sexp,sexp)=env->data_ptr->val.fun->comp.f3;
  return f(a,b,NIL);
}
sexp call_1_with_2_optional(sexp a){
  sexp(*f)(sexp,sexp,sexp)=env->data_ptr->val.fun->comp.f3;
  return f(a,NIL,NIL);
}
sexp call_2_with_2_optional(sexp a,sexp b){
  sexp(*f)(sexp,sexp,sexp,sexp)=env->data_ptr->val.fun->comp.f4;
  return f(a,b,NIL,NIL);
}
sexp(*)(sexp,sexp)make_function_pointer2(subr *fun){
  switch(fun->subr_type){
    case fun_lambda:
    case fun_closure:
    case fun_compiled:
      if(fun->rest_arg){
        if(fun->req_args<=2){
          push_data(current_environment,function_sexp(fun));          
          return call_many_with_2_implicit;
        }
      } else if(fun->maxargs==fun->req_args){
        if(fun->maxargs == 2){
          return fun->comp.f2;
        } else {
          
      }
            
#endif
