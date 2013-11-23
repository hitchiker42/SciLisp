#include "common.h"
struct promise {
  pthread_t thread_no;
  sexp retval;
  int returned;
};
typedef struct promise_args promise_args;
struct promise_args{
  struct promise *promise_val;
  sexp arguments;
  sexp environment;
};
sexp make_promise(sexp function,sexp arguments,sexp environment){}
static void* promise_execute(void *pthrea_arg){
  promise_args *args=(promise_args*)pthread_arg;
  
