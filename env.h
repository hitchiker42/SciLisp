/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __ENV_H__
#define __ENV_H__
#include "common.h"
struct symbol_props {
  int is_const :1;
  int setfable :1;
  int typed :1;
  int global :1;
  _tag type;
};
struct symbol {
  CORD name;
  sexp val;
  symbol_props props;
};
struct local_symbol{
  CORD name;
  sexp val;
  symbol_props props;
  local_symref next;
};
struct local_env{
  env* enclosing;
  local_symref head;
};
struct function_env{
  env* enclosing;
  function_args* head;//for consistancy in naming
};
union symbol_ref{
  local_symref local;
  function_args *function;
  obarray *ob;
};
union symbol_val{
  local_symbol local;
};
struct env{
  env* enclosing;
  symbol_ref head;  
  enum {
    _local=0,
    _global=1,
    _funArgs=2,
    _obEnv=3,
  } tag;
};
#define to_env (cur_env,type)\
  (env){.enclosing=cur_env->enclosing,.head=cur_env->head,.tag=type}
env *topLevelEnv;
obarray *globalObarray;
obarray_env *globalObarrayEnv;
obarray *keywordObarray;
obarray_env *keywordObarrayEnv;
local_symref getLocalSym(local_env *cur_env,CORD name);
symref getFunctionSym(function_env* cur_env,CORD name);
symref getGlobalSym(CORD name);
sexp getKeySymSexp(CORD name);
symref getSym(env *cur_env,CORD name);
symref addSym(env *cur_env,symref Var);
symref addGlobalSym(symref Var);
symref addLocalSym(env *cur_env,symref Var);
//functions to look for a  symbol in a specific environment only
symref getSymLocalOnly(local_env *cur_env,CORD name);
symref getSymFunctionOnly(function_env* cur_env,CORD name);
symref getSymObarrayOnly(obarray_env* ob_env,CORD name);
symref getSymNotGlobal(env *cur_env,CORD name);
//check if name refers to a function argument, return NULL if not
long isFunctionArg(function_env *cur_env,CORD name);
//type punning macros
#define toSymbol(sym) (*(symbol*)&sym)
#define toSymref(ref) (*(symref*)&(ref))

//not sure if this should be a parameter
#define OBARRAY_BKT_CAPACITY 10
struct obarray {  
  obarray_entry **buckets;//points to first bucket
  int size;//memory allocated for the obarray
  int used;//buckets used
  int entries;//number of obarray_entries
  float capacity;//sum of entries per buckets for all buckets/num_buckets
  float capacity_inc;//1/(size*10)
  float gthresh;//growth threshold
  float gfactor;//growth factor
  int is_weak_hash;//only actually needs a single bit
  uint64_t (*hash_fn)(const void*,int);
};
struct obarray_env {
  env* enclosing;
  obarray* head;
};
struct obarray_entry {
  obarray_entry *prev;
  obarray_entry *next;
  symref ob_symbol;
  uint64_t hashv;
};
enum add_option{//conflict resolution for an existing symbol
  _update=2,//change value of existing symbol
  _ignore=3,//keep current symbol and add new symbol
  //this is likely to cause some errors, but it exists, because why not
  _overwrite=4,//explictly overwrite current entry
  _use_current=5,//keep current entry and ignore update
};
obarray* obarray_init_custom(float gthresh,uint64_t(*hash_fn)(const void*,int),
                      uint64_t size,int32_t is_weak_hash);
obarray* obarray_init_default(uint64_t size);
obarray* obarray_init(uint64_t size,float gthresh);
obarray* init_prim_obarray();
obarray_entry*  obarray_add_entry_generic
(obarray *ob,symref new_entry,enum add_option conflict_opt,int append);
obarray_entry*  obarray_add_entry(obarray *ob,symref new_entry);
int obarray_rehash(obarray *ob);
obarray_entry* obarray_get_entry(obarray *cur_obarray,CORD symname,uint64_t hashv);
obarray_entry* obarray_remove_entry(obarray *cur_obarray,CORD symname);
symref getObarraySym(obarray_env *ob_env,CORD name);
symref addObarraySym(obarray_env *ob_env,symref Var);
int bucketLength(obarray_entry* bucket);
uint64_t obarray_delete_entry(obarray *ob,symref entry);
obarray_entry* prim_obarray_add_entry(obarray *ob,symref new_entry,
                                      obarray_entry *entry);
static inline size_t symbolSize(env *cur_env){
  switch(cur_env->tag){
    case _local:
      return sizeof(local_symbol);
    case _obEnv:
      return sizeof(obarray);
    case _funArgs:
      return sizeof(function_args);
    default:
      return 0;
  }
}
#endif
