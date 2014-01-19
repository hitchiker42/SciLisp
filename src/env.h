/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __ENV_H__
#define __ENV_H__
#include "common.h"
enum symbol_interned {
  _symbol_interned = 0,
  _symbol_uninterned = 1,
  _symbol_interned_in_initial_obarray = 2,
};
enum externally_visable {
  _symbol_in_current_or_global_obarray = 0,
  _symbol_externally_visable = 1,
  _symbol_not_externally_visable = 2,
  _symbol_locally_visable = 3,
};
//should be allocated using gc_malloc_atomic(sizeof(symbol_name)*name_len)
/*structure for symbol name and simple/common properties, to avoid having 
  to access the plist to determine things like constness or typing*/
struct symbol_name {
  uint32_t name_len;
  uint64_t hashv;
  const char *name;//needs to be last(its basically a variable sized array)
};

struct symbol {
  sexp val;
  sexp plist;
  uint8_t type;
  unsigned interned :2;
  unsigned constant :2;
  unsigned visibility :2;
  unsigned special :1;//non special variables don't need to have their values saved
  struct symbol_name *name;
  //pointer to next symbol, in obarray bucket
  symbol *next;
};
struct binding {
  symbol *sym;//pointer to symbol
  sexp prev_val;
};
/* Lexical environments don't need to be special, they're just alists
*/
//some of these might need to be rewritten if they voilate the
//semantics of the ?:
#define push_generic_signal(stack,env,data)                             \
  (env->stack##_ptr>=env->stack##_stack?env->error_num=2,raise(SIGUSR1):\
   *env->stack##_ptr++=data)
#define push_generic_no_signal(stack,env,data)                          \
  (env->stack##_ptr>=env->stack##_stack?NULL:*env->stack##_ptr++=data,1)
#define push_generic_unsafe(stack,env,data)     \
  (*env->stack##ptr++=data)
#define pop_generic_signal(stack,env)                                   \
  (env->stack##_ptr<=env->stack##_top?env->error_num=2,raise(SIGUSR1):  \
   *env->stack##_ptr--)
#define pop_generic_no_signal(stack,env,data)                       \
  (env->stack##_ptr<=env->stack##_top?NULL:data=*env->stack##_ptr--,1)
#define pop_generic_unsafe(stack,env)           \
  (env->stack##_ptr--)
#define stack_size(stack,env) (env->stack##_ptr-env->stack##_stack)

#define data_size(env) (stack_size(data,env)) 
#define binding_size(env) (stack_size(binding,env)) 
#define frame_size(env) (stack_size(frame,env)) 
#define call_size(env) (stack_size(call,env)) 

#define push_binding(env,data) push_generic_signal(binding,env,data)
#define pop_binding(env) pop_generic_signal(binding,env)
#define push_frame(env,data) push_generic_signal(frame,env,data)
#define pop_frame(env) pop_generic_signal(frame,env)
#define push_call(env,data) push_generic_signal(call,env,data)
#define pop_call(env) pop_generic_signal(call,env)
#define push_data(env,data) push_generic_signal(data,env,data)
#define pop_data(env) pop_generic_signal(data,env)

#define try_push_binding(env,data) push_generic_no_signal(binding,env,data)
#define try_pop_binding(env,data) pop_generic_no_signal(binding,env)
#define try_push_frame(env,data) push_generic_no_signal(frame,env,data)
#define try_pop_frame(env,data) pop_generic_no_signal(frame,env)
#define try_push_call(env,data) push_generic_no_signal(call,env,data)
#define try_pop_call(env,data) pop_generic_no_signal(call,env)
#define try_push_data(env,data) push_generic_no_signal(data,env,data)
#define try_pop_data(env,data) pop_generic_no_signal(data,env)

#define LEX_BIND(env,val,sym) (env->lex_env=Cons(Cons(sym,val),env->lex_env),env->lex_bindings++) 
#define LEX_BIND_CURRENT(val,sym) LEX_BIND(current_environment,val,sym)
/* per thread values (no need for a lock)*/
struct environment {
  //data which shouldn't be set to 0
  package *current_package;//contains current obarray and 
  //c thread local data
  stack_t *sigstack;//alternative stack for signals  
  //stacks
  bindings *binding_stack;//lexical bindings stack
  bindings *binding_ptr;//stack pointer
  bindings *binding_top;//top of lexical bindings stack
  //holds lables(frames,jump points, whatever) for functions/errors,etc
  frame *frame_stack;//stack of jump points (returns, catches, handlers)
  frame *frame_ptr;//stack pointer
  frame *frame_top;
  //records function calls
  sexp *call_stack;//call stack
  sexp *call_ptr;//stack pointer
  sexp *call_top;
  //holds function arguments mostly
  sexp *data_stack;//data/function argument stack
  sexp *data_ptr;//stack ptr
  sexp *data_top;
  //return values? for now I'll just use the stack
  //everything below here can be set to zero on error
  sexp lex_env;//lexical environment, alist or NIL
  uint32_t lex_bindings;//number of currently active lexical bindings
  uint32_t eval_depth;//current eval depth
  //used for dealng with c signals, if error_num is set to a non-zero
  //value that means the current signal was sent due to a lisp error
  uint32_t error_num;//maybe define to sig_atomic_t?
  uint32_t frame_size;
  uint32_t data_size;
  uint32_t binding_size;
  uint32_t call_size;
};
struct frame {
  union{
    jmp_buf dest;//a jmp_buf is pretty big, 200 bytes
    ucontext_t *context;//...but ucontext_t is 936 bytes
  }
  uint64_t tag;//a pointer to a symbol or an integer
  //need a value that specifically catches anything to allow for cleanup
  //something like 0xffffffff
  //nevermind, I can use the type to decide that
  sexp value;//for throw or return
  enum {
    function_frame,
    block_frame,
    catch_frame,
    tagbody_frame,
    condition_frame,//uses ucontext
    unwind_protect_frame,//catches anythings
  } frame_type;
};
/*
  typedef struct frame frame[1]
  #define make_frame(tag,type){.tag=tag,.frame_type=type}
  various non local exits:
  blocks:
  (block name &rest body)
    functions are implicitly enclosed in blocks named nil, to
    allow returning, (actually probably not, because
    I should be able to do that via a local jump and untill I
    figure out how to best do that it's not worth the overhead
    of longjmp for every function)
  defun block(name,body){
    frame new_frame;
    *new_frame=make_frame(name,block);
    if(setjmp(new_frame->dest)){
      return new_frame->value;
    }
    push_frame(current_environment,new_frame);//signals on everflow
    return eval(body);
  }
  tagbody:
    tagbody (tag|form)*
    evaluate forms, tags are symbols or unsigned integers and are targets for go
    should be able to optimize to local jumps for some cases
    defun tagbody(args){
    frame *tag;
    sexp start=args;
    while(CONSP(args)){
    if(SYMBOLP(XCAR(args)) || INTP(XCAR(args)){
    *tag=xmalloc(sizeof(struct frame));
    *tag->tag=XCAR(args).val.uint64_t;
    //not sure if this will work or not
    if(setjmp(*tag->dest)){
      eval(*tag->value);
    } else {
    *tag->value=XCDR(args);
      //strip out the tag so we don't set it twice
      XCAR(args)=XCADR(args);
      XCDR(args)=XCDDR(args);
      push_frame(current_environment,*tag);
    }
    } else {
      args=XCDR(args);
     }
     return eval(start);
}
  go:
    (go tag)
    goto the first tagbody frame on the frame stack who's value is eq to tag
  catch:
    (catch tag forms*)
    evaluate and return value of forms unless interrupted by throw,
  throw:
    (throw tag form)
    return the value of form from the nearest enclosing catch with a
    tag eq to tag
  return;
    (return form)
    return the value of form from the nearest enclosing block with a tag of nil
  return-from:
    (return-from tag form)
    return the value of form from the nearest enclosing block with
    a tag eq to tag
  unwind-protect:
    (unwind-protect protected cleanup*)
    establish a handler to catch any nonlocal exit from protected
    then evaluate protected followed by cleanup, regardless of how
    protected is exited, return value of protected
    
 */
//should it be an error if num_bindings > env->lex_bindings?
static void unwind_lex_env(environment *env,uint32_t num_bindings){
  if(num_bindings==env->lex_bindings){
    env->lex_env=NIL;
    return;
  } else {
    int i;
    for(i=0;i<num_bindings;i++){
      env->lex_env=XCDR(env->lex_env);
      env->lex_bindings--;
    }
  }
}
struct package {
  lisp_string name;
  obarray *symbol_table;
};
obarray *global_obarray;
//current dynamic environment
static thread_local struct obarray *current_obarray;
static thread_local struct environment *current_environment;
extern uint64_t bindings_stack_size;
extern uint64_t handler_stack_size;
symbol *copy_symbol(symbol_new *sym,int copy_props);
sexp getKeywordType(sexp obj);
struct obarray {
  symbol **buckets;
  uint32_t size;//number of buckets
  uint32_t used;//buckets used
  uint32_t entries;//number of symbols in the table
  float capacity;//entries/size(for convience)
  float capacity_inc;//1/(size*10) (10 is soft cap on entries/bucket)
  float gthreshold;//value of capacity at which to enlarge the table
  float gfactor;//ammount to multiply size by when growing the table
#ifdef MULTI_THREADED
  pthread_rwlock_t *restrict lock;
#endif
  //32 bits of padding
};
symbol* lookup_symbol(struct obarray_new *ob,const char* name);
symbol *lookup_symbol_global(char *restrict name);
obarray *make_obarray_new(uint32_t size,float gthreshold,float gfactor);
symbol *c_intern(const char* name,uint32_t len,struct obarray_new *ob);
symbol *obarray_lookup_sym(symbol_name *sym_name,obarray_new *ob);
sexp lisp_intern(sexp sym_or_name,sexp ob);
void c_intern_unsafe(obarray *ob,symbol* new);
#endif
