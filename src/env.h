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
  //pointer to next symbol, in obarray bucket for global symbols, or in local
  //environment for local symbols
  symbol *next;
};
struct binding {
  symbol *sym;//pointer to symbol
  sexp prev_val;
};
struct lexical_bindings {
  struct environment *enclosing;
  binding *bindings;//stack of bindings
  uint32_t num_bindings;
};
struct lexical_env {
  sexp env_alist;
  uint32_t size;
};
/* per thread values (no need for a lock)*/
struct environment {
  //stacks
  package *current_package;//contains current obarray and 
  bindings **lexical_bindings;//stack for lexical bindings
  bindings *current_lexical_env;//stack pointer
  handler **frame_stack;//stack of jump points (returns, catches, handlers)
  handler *innermost_frame;//stack pointer
  sexp **call_stack;//call stack
  sexp *current_function;//stack pointer
  sexp **stack;//data/function argument stack
  sexp *stack_ptr;//stack ptr
  uint32_t eval_depth;//current eval depth
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
