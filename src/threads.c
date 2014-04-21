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
#include "common.h"
#include "threads.h"
#include <semaphore.h>
/* ideas/Issues with atomic queues (using arrays, not linked lists);
   using an array is a bad idea in the first place since it has
   no priority, use a heap instead if I'm going to be locking.
   
   //this part of SciLisp isn't lisp and doesn't need to be
   //it's implementing thread primitives so it can afford to
   //be closer to C then usual
   //this allocates a lot of memory
   atomic tail queue:
   struct queue_node {
     sexp *data;
     queue_node *next;
   }
   struct queue {
   queue_node *head;//(data . next)
   queue_node *tail;//(data . nil)
   }
   sexp queue_pop(struct queue *queue){
     queue_node *old_heap;
     //exchanges pointers, which is why it can be atomic
     __atomic_exchange(&queue->heap,&queue->head->next,&old_head);
     return old_head->data;
     //thank god for gc
   }
   void queue_append_by_value(struct queue *queue, sexp val){
      sexp *val_ptr=xmalloc(sizeof(sexp));
      *val_ptr=val;
      queue_append(queue,val_ptr);
   }
   void queue_append(struct queue *queue,sexp *val){
     queue_node *tail_copy=xmalloc(sizeof(queue_node))
     queue_node *new_node=xmalloc(sizeof(queue_node))
     new_node={.data=val,.next=NULL};
     do {
       queue_node *old_tail =queue->tail;
       *tail_copy = *old_tail;     
       tail_copy->next=new_node;
     } while(!cmpxchg(queue->tail,old_tail,tail_copy));
   }
*/
#define POP_JOB(thread,job)                      \
  ({thread_job *new_job;\
    pthread_spin_lock(thread->queue_lock);      \
    new_job=thread->job_queue[--thread->queued_jobs];   \
    pthread_spin_unlock(thread->queue_lock);    \
    new_job;})
#define PUSH_JOB(thread,job) {                  \
    pthread_spin_lock(thread->queue_lock);      \
    thread->job_queue[thread->queued_jobs++]=job;       \
    pthread_spin_unlock(thread->queue_lock);    \
  }
#ifdef NUM_PROCS
#define POOL_SIZE NUM_PROCS
#else
#define POOL_SIZE 4
#endif
#define MAX_JOBS 10
static pthread_attr_t pool_thread_attr;
static pthread_once_t pool_once=PTHREAD_ONCE_INIT;
enum promise_state{
  PROMISE_UNINITIALIZED=0,
  PROMISE_UNFULFILLED,
  PROMISE_FULFILLED,
  PROMISE_CANCLED,
};
struct promise {
//the thread locks this for writing untill the promise is
//fuffiled or cancled
  pthread_rwlock_t lock;
  sexp retval;
  int status;
};
struct thread_job {
  uint64_t priority;//if I use a priority queue
  union {
    sexp(*c_fun)();
    sexp lisp_expr;
  };
  promise *promise;//optional
  sexp *args;//might be self referential
  int32_t numargs;
  int job_type;//lisp/c
  //might add to this to allow a green threads to be
  //run in the thread pool, the user would need to 
  //call yield however (of course you can still use
  //native threads if you want)
}
struct pooled_thread {
  pthread_t thread;
  sem_t semaphore;//for waiting for jobs
  pthread_spinlock_t queue_lock;
  thread_job job_queue[MAX_JOBS];
  uint32_t queued_jobs;
};
pooled_thread* make_pooled_thread(){
  pooled_thread *retval=xmalloc(sizeof(pooled_thread));
  sem_init(&retval->semaphore);
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
    sem_wait(thread->semaphore);
    thread_job job=POP_JOB;
    sexp retval;
    /* Setup an unwind protect frame here, run the job
       if we catch an error set the promise state to cancled and
       get another job
     */
    if(job->promise){
      pthread_rwlock_lock(job->promise->lock);
      job->promise->state=PROMISE_UNFULFILLED;
    }
    switch(job->job_type){
      case 0://lisp job
        //need to set up thread env at some point(presumably in thread init)
        retval=eval(job->lisp_expr,current_env);
        break;
      case 1://c_job
        retval=job.c_fun(job->numargs,job->args);
        break;        
        //if you need to call other types of c functions write a wrapper
    }
    if(job->promise){
      job->promise->retval=retval;
      job->promise->state=PROMISE_FULFILLED;
      pthread_rwlock_unlock(job->probise->lock);
    }
  }
}
struct thread_pool{
  pooled_thread threads[POOL_SIZE];
  int pool_index;
};
void add_job(thread_job *job,struct thread_pool *pool){
  pooled_thread *thread;
  //this will loop untill there is a thread without a full
  //job queue, while it should be rare nfor all the
  //threads to have full job queues at once it's not impossible 
  //so this should probably do something (i.e wait) if this is the case
  do {
    thread=pool->threads[pool->pool_index];//get thread to use
    if(!thread){//don't allocate new threads untill we need them
      thread=pool->threads[pool->pool_index]=make_pooled_thread();
      //shuold create the thread detached
      pthread_create(&thread->thread,NULL,pooled_thread_init,(void*)thread);
    }
    pool->pool_index+=((pool->pool_index+1)%POOL_SIZE);//rotate threasd
  } while (atomic_load(thread->queued_jobs) >= MAX_JOBS);
  PUSH_JOB(thread,job);
  sem_post(&thread->semaphore);
}
sexp promise_try_get_val(sexp promise){
  if(!pthread_rwlock_tryrdlock(&promise.val.promise->lock)){
    //we got the lock
    if(promise.val.promise->state == PROMISE_FULFILLED){
      sexp retval=Fcons(promise.val.promise->retval,LISP_TRUE);
      pthread_rwlock_unlock(&promise.val.promise->lock);
      return retval;
    } 
    pthread_rwlock_unlock(promise.val.promise->lock);
  }
  //since the promise could return nil as a valid value we need
  //to return multiple values(in this case just a cons cell
  //as I've yet to add multpile return values)
  return Fcons(NIL,LISP_FALSE);
}
sexp get_promise_val_wait(sexp promise_sexp){
//this has the same problem as above, I think
//I need a constant for promise canceld
  promise promise=promise.val.promise;
  sexp retval;
  while(atomic_load(&promise.state)==PROMISE_UNINITIALIZED){
    //probably shouldn't be busy waiting here
    ;
  }
  pthread_rwlock_rdlock(&promise->lock);//blocks if necessary
  if(promise.state == PROMISE_CANCLED){
    retval=NIL;
  } else {
    retval=promise->retval;
  }
  phread_rwlock_unlock(promise->lock);
  return retval;
}
