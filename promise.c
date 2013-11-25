#include "common.h"
static pthread_attr_t promise_attr;
static void* promise_execute(void *pthrea_arg);
static int promise_init;
struct promise {
  pthread_t thread_no;
  pthread_mutex_t promise_mutex;
  pthread_mutex_cond promise_cond;
  sexp retval;
  int returned;
};
typedef struct promise_args promise_args;
struct promise_args{
  struct promise *promise_val;
  sexp promise_fun;
  sexp arguments;
  sexp environment;
};
sexp make_promise(sexp function,sexp arguments,sexp environment,sexp pass_promise_as_arg){
  if(!promise_init){
    promise_init=1;
    pthread_attr_init(&promise_attr);
    pthread_attr_setdetachstate(promise_attr,PTHREAD_CREATE_DETACHED);
  }
  struct promise *promise_obj=xmalloc(sizeof(struct promise));
  promise_obj->promise_mutex=PTHREAD_MUTEX_INITIALIZER;
  promise_obj->promise_cond=PTHREAD_COND_INITIALIZER;
  if(isTrue(pass_promise_as_arg)){
    arguments=Cons(arguments,promise_obj);
  }
  struct promise_args *args=xmalloc(sizeof(struct promise_args));
  *args=(struct promise_args){.promise_val=promise_obj,.promise_fun=function,
                              .arguments=arguments,.environment=environment};
  pthread_create(&(promise_obj->thread_no),&promise_attr,promise_execute,(void*)args);
  return promise_sexp(promise_obj);
}
static void* promise_execute(void *pthrea_arg){
  struct promise_args *args=(promise_args*)pthread_arg;
  struct promise cur_promise=args->promise_val;
  sexp retval=
    lisp_apply(args->promise_fun,args->arguments,args->environment);
  pthread_mutex_lock(&(cur_promise.promise_mutex));
  cur_promise.retval=retval;
  cur_promise.returned=1;
  pthread_cond_broadcast(&(cur_promise.promise_cond));
  pthead_mutex_unlock(&(cur_promise.promise_mutex));
  return NULL;
}
sexp try_getPromiseVal(sexp promise){
  sexp retval;
  retval.val.cons=xmalloc(sizeof(cons));
  //since the promise could well return nil we need
  //to return multiple values(in this case just a cons cell
  //as I've yet to add multpile return values)
  XCAR(retval)=NIL;
  XCDR(retval)=LISP_FALSE;
  if(!pthread_mutex_trylock(promise.val.promise->promise_mutex)){
    //we got the lock
    if(promise.val.promise->returned){
      XCAR(retval)=promise.val.promise->retval;
      XCDR(retval)=LISP_TRUE;
    }
    pthread_mutex_unlock();
  }
  return retval;
}
sexp getPromiseVal_wait(sexp promise){
