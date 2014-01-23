/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __ENV_H__
#define __ENV_H__
//#include "common.h"
enum symbol_interned {
  symbol_interned = 0,
  symbol_uninterned = 1,
  symbol_interned_in_initial_obarray = 2,
};
enum externally_visable {
  symbol_in_current_or_global_obarray = 0,
  symbol_externally_visable = 1,
  symbol_not_externally_visable = 2,
  symbol_locally_visable = 3,
};
//should be allocated using gc_malloc_atomic(sizeof(symbol_name)*name_len)
/*structure for symbol name and simple/common properties, to avoid having
  to access the plist to determine things like constness or typing*/
struct symbol_name {
  uint64_t hashv;
  uint32_t name_len;
  uint8_t multibyte;//really just needs a bit, but we can spare the space
  const char *name;//needs to be last(its basically a variable sized array)
};
struct symbol {
  sexp val;
  sexp plist;
  uint8_t type;
  unsigned interned :2;//enum symbol_interned
  unsigned constant :2;//0 no, 1 yes, 2 warn before changing
  unsigned visibility :2;//enum externally_visable
  unsigned special :1;//non special variables don't need to have their values saved
  struct symbol_name *name;
  //pointer to next symbol, in obarray bucket
  symbol *next;
};
//alternate dynamic bindings are thread local, to get a dynamic binding
//we need to search through the bindings stack, this is likely less
//efficent for single threaded programs than rebinding the symbol value
//in the obarray, but it lets me do the same thing regardless of threads
typedef struct binding binding;
typedef struct subr_call subr_call;
struct binding {
  symbol *sym;//pointer to symbol
  sexp val;
};
/* Lexical environments don't need to be special, they're just alists
*/
//the signaling push/pop macros might need to use a statement expression
//rather than a ?: operator
#define push_generic_signal(stack,env,data)                             \
  ({if(env->stack##_ptr>=env->stack##_top){                             \
      env->error_num=2;                                                 \
    raise(SIGUSR1);                                                     \
    }                                                                   \
    env->stack##_index++;                                               \
    *env->stack##_ptr++=data;})
#define push_generic_no_signal(stack,env,data)                          \
  (env->stack##_ptr>=env->stack##_top?NULL:                             \
   env->stack##_index++,*env->stack##_ptr++=data,1)
#define push_generic_unsafe(stack,env,data)                             \
  (env->stack##_index++,*env->stack##ptr++=data)
#define pop_generic_signal(stack,env)                                   \
  ({if(env->stack##_ptr<=env->stack##_stack){                           \
    env->error_num=2;                                                   \
    raise(SIGUSR1):                                                     \
    }                                                                   \
    env->stack##_index--;                                               \
    *env->stack##_ptr--})
#define pop_generic_no_signal(stack,env,data)                           \
  (env->stack##_ptr<=env->stack##_stack?NULL:                           \
   env->stack##_size--,data=*env->stack##_ptr--,1)
#define pop_generic_unsafe(stack,env)           \
  (env->stack##_index--,env->stack##_ptr--)
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
#define LEX_BIND_CURRENT(val,sym) LEX_BIND(current_env,val,sym)

//two megs seems a normal default, but that seems a bit much for now
//so I'll use 2^15 bytes, whatever that is
static uint32_t frame_stack_size=2<<15;//max size of an signed short
static uint32_t data_stack_size=2<<15;
static uint32_t bindings_stack_size=2<<15;
static uint32_t call_stack_size=2<<15;
void init_environment(void);
void *init_environment_pthread(void*);
int lisp_pthread_create(pthread_t *thread,const pthread_attr_t *attr,
                       void*(*start_routine)(void*),void *arg);
/* per thread values (no need for a lock)*/
struct environment {
  //data which shouldn't be set to 0
  package *current_package;//contains current obarray and
  //c thread local data
  stack_t *sigstack;//alternative stack for signals
  //stacks
  binding *bindings_stack;//lexical bindings stack
  binding *bindings_ptr;//stack pointer
  binding *bindings_top;//top of lexical bindings stack
  //holds lables(frames,jump points, whatever) for functions/errors,etc
  frame *frame_stack;//stack of jump points (returns, catches, handlers)
  frame *frame_ptr;//stack pointer
  frame *frame_top;
  //records function calls
  //also holds the lexical environment
  subr_call *call_stack;//call stack
  subr_call *call_ptr;//stack pointer
  subr_call *call_top;
  //holds function arguments mostly
  sexp *data_stack;//data/function argument stack
  sexp *data_ptr;//stack ptr
  sexp *data_top;
  //return values? for now I'll just use the stack
  //everything below here can be set to zero on error
  sexp lex_env;//current lexical environment, call stack is used to unwind it
  //used for dealng with c signals, if error_num is set to a non-zero
  //value that means the current signal was sent due to a lisp error
  uint32_t error_num;//maybe define to sig_atomic_t?
  uint32_t frame_index;
  uint32_t data_index;
  uint32_t bindings_index;
  uint32_t call_index;
};
struct subr_call {
  sexp lex_env;
  sexp lisp_subr;
  uint32_t bindings_index;
};
/* for now dealt with by the call stack
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
  }*/
struct package {
  lisp_string name;
  obarray *symbol_table;
};
obarray *global_obarray;
//current dynamic environment
static thread_local struct obarray *current_obarray;
static thread_local struct environment *current_env;
static thread_local frame_addr top_level_frame;
//extern uint64_t bindings_stack_size;
//extern uint64_t handler_stack_size;
symbol *copy_symbol(symbol *sym,int copy_props);
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
symbol* lookup_symbol(struct obarray *ob,const char* name);
symbol *lookup_symbol_global(char *restrict name);
obarray *make_obarray_new(uint32_t size,float gthreshold,float gfactor);
symbol *c_intern(const char* name,uint32_t len,struct obarray *ob);
symbol *obarray_lookup_sym(symbol_name *sym_name,obarray *ob);
sexp lisp_intern(sexp sym_or_name,sexp ob);
void c_intern_unsafe(obarray *ob,symbol* new);
#endif
