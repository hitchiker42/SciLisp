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
  sexp val;
};
/* Lexical environments don't need to be special, they're just alists
*/
#define push_generic_safe(stack,env,data)            \
  (env->stack##_ptr>=env->stack##_stack?raise(SIGUSR1):*env->stack##_ptr++=data)
#define push_generic_unsafe(stack,env,data)     \
  (*env->stack##ptr++=data)
#define pop_generic_safe(stack,env)             \
  (env->stack##_ptr<=env->stack##_top?raise(SIGUSR1):*env->stack##_ptr--)
#define pop_generic_unsafe(stack,env)           \
  (env->stack##_ptr--)
#define push_binding(env,data) push_generic_safe(binding,env,data)
#define pop_binding(env) pop_generic_safe(binding,env)
#define push_frame(env,data) push_generic_safe(frame,env,data)
#define pop_frame(env) pop_generic_safe(frame,env)
#define push_call(env,data) push_generic_safe(call,env,data)
#define pop_call(env) pop_generic_safe(call,env)
#define push_data(env,data) push_generic_safe(data,env,data)
#define pop_data(env) pop_generic_safe(data,env)

/* per thread values (no need for a lock)*/
struct environment {
  //stacks
  package *current_package;//contains current obarray and 
  bindings *binding_stack;//lexical bindings stack
  bindings *binding_ptr;//stack pointer
  bindings *binding_top;//top of lexical bindings stack
  //holds lables(frames,jump points, whatever) for functions/errors,etc
  handler *frame_stack;//stack of jump points (returns, catches, handlers)
  handler *frame_ptr;//stack pointer
  handler *frame_top;
  //records function calls
  sexp *call_stack;//call stack
  sexp *call_ptr;//stack pointer
  sexp *call_top;
  //holds function arguments mostly
  sexp *data_stack;//data/function argument stack
  sexp *data_ptr;//stack ptr
  sexp *data_top;
  uint32_t eval_depth;//current eval depth
  uint32_t error_num;
  uint32_t frame_size;
  uint32_t data_size;
  uint32_t binding_size;
  uint32_t call_size;
  //c thread local data
  stack_t *sigstack;//alternative stack for signals
  
};
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
#endif
