/* Thread pool and promises

   Copyright (C) 2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
#define POP_JOB(thread) (thread->job_queue[queued_jobs--])
#define PUSH_JOB(thread,job) (thread->job_queue[queued_jobs++]=job)
#define POOL_SIZE 5
#define MAX_JOBS 10
struct promise {
  pthread_mutex_t *lock;
  pthread_mutex_cond *cond;
  sexp retval;
  int returned;
};
struct thread_job {
  union {
    sexp(*c_fun)();
    sexp lisp_expr;
  };
  promise *promise;//optional
  sexp *args;
  int32_t numargs;
  int job_type;
}
struct pooled_thread {
  pthread_t thread;
  pthread_mutex_t *lock;
  pthread_cond_t *cond;
  thread_job *job_queue;
  uint32_t queued_jobs;
};
pooled_thread* make_pooled_thread(){
  pooled_thread *retval=xmalloc(sizeof(pooled_thread));
  retval->lock=xmalloc_atomic(sizeof(pthread_mutex_t));
  retval->cond=xmalloc_atomic(sizeof(pthread_cond_t));
  pthread_cond_init(retval->cond,NULL);
  pthread_mutex_init(retval->lock,NULL);
  retval->thread_job=xmalloc(sizeof(thread_job)*MAX_JOBS);
  return retval;
}
void* __attribute__((noreturn)) pooled_thread_init(void* args){
  pooled_thread self=*(pooled_thread*)args;
  init_environment();
  pooled_thread_loop(self);
  return 0;
}
void __attribute__((noreturn)) pooled_thread_loop(struct pooled_thread thread){
  while(1){
    phtread_mutex_lock(thread->lock);
    while(!thread->queued_jobs){//if no jobs
      pthread_cond_wait(thread->cond,thread->lock);//wait for a job
    }
    thread_job job=POP_JOB;
    pthead_mutex_unlock(thread->lock);
    sexp  retval;
    switch(job->job_type){
      case 0://lisp job
        retval=eval(job->lisp_expr,current_env);//need to set up thread env at some point
        break;
      case 1://c_job
        retval=job.c_fun(job->numargs,job->args);
        break;        
        //if you need to call other types of c functions write a wrapper
    }
    if(job->promise){
      pthread_mutex_lock(job->promise->lock);
      job->promise->retval=retval;
      job->promise->returned=1;
      pthread_cond_signal(job->promise->cond);
      pthread_mutex_unlock(job->promise->lock);
    }
  }
}
struct thread_pool{
  pooled_thread threads[POOL_SIZE];
  int pool_index;
};
void add_job(thread_job *job,struct thread_pool *pool){
  pooled_thread *thread=pool->threads[pool->pool_index];//get thread to use
  if(!thread){//don't allocate new threads untill we need them
    thread=pool->threads[pool->pool_index]=make_pooled_thread();
    pthread_create(&thread->thread,NULL,pooled_thread_init,(void*)thread);
  }
  pool->pool_index+=((pool->pool_index+1)%POOL_SIZE);//rotate threads    
  pthread_mutex_lock(thread->lock);
  PUSH_JOB(thread,job);
  pthread_cond_signal(thread->cond);
  pthread_mutex_unlock(thread->lock);
}
sexp promise_try_get_val(sexp promise){
  sexp retval;
  retval.val.cons=xmalloc(sizeof(cons));
  //since the promise could return nil as a valid value we need
  //to return multiple values(in this case just a cons cell
  //as I've yet to add multpile return values)
  XCAR(retval)=NIL;
  XCDR(retval)=LISP_FALSE;
  if(!pthread_mutex_trylock(promise.val.promise->lock)){
    //we got the lock
    if(promise.val.promise->returned){
      XCAR(retval)=promise.val.promise->retval;
      XCDR(retval)=LISP_TRUE;
    }
    pthread_mutex_unlock(promise.val.promise->lock);
  }
  return retval;
}
sexp getPromiseVal_wait(sexp promise){
  promise real_promise=promise.val.promise;
  pthread_mutex_lock(real_promise->lock);
  while(!real_promise->returned){
    pthread_cond_wait(real_promise->cond);
  }
  sexp retval=real_promise->retval;
  phread_mutex_unlock(real_promise->lock);
  return retval;
}
