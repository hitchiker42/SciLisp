/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef __ENV_H__
#define __ENV_H__
#include "common.h"
enum symbol_interned{
    _symbol_interned = 0,
    _symbol_uninterned = 1,
    _symbol_interned_in_initial_obarray = 2
};
struct symbol_props {
  CORD doc;
  unsigned int is_const :1;
  unsigned int global :1;
  unsigned int setfable :1;//move this to functions? (yes)
  unsigned int typed :1;
  unsigned int interned : 2;
  _tag type;
};
struct symbol {
  CORD name;
  sexp val;
  symbol_props props;//need to change to plist
};
//add at some point
/* It's my language, plists are alists deal with it
  so:
  struct symbol {
    CORD name;
    sexp val;
    symbol_props props;
  }
  struct symbol_props {
    cons *plist;
    ... //w/o the CORD
  }
  sexp get_symbol_prop(sexp symbol_sexp,sexp prop){
    symref symbol_ref=get_symbol(cur_env,symbol_sexp.var.var->name);
    if(!symref){return error;}
    cons proplist=*symbol_ref->plist;
    uint64_t prop_key=prop.val.uint64;//assume prop is a keyword
    while(proplist.cdr.tag != _nil){
    if(XCAR(proplist.car).val.uint64 == prop_key){
    return XCDR(proplist.car);
    }
    proplist=*(proplist.cdr.val.cons)
    }
    return error
  }
 */
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
symref getSymFromSexp(sexp var,env *cur_env);
symref addSymFromSexp(sexp var,sexp val,env *cur_env);
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
sexp getKeywordType(sexp obj);
//check if name refers to a function argument, return NULL if not
long isFunctionArg(function_env *cur_env,CORD name);
obarray* obarray_init_custom(float gthresh,uint64_t(*hash_fn)(const void*,int),
                      uint64_t size,int32_t is_weak_hash);
obarray* obarray_init_default(uint64_t size);
obarray* obarray_init(uint64_t size,float gthresh);
obarray* init_prim_obarray();
obarray_entry*  obarray_add_entry_generic
(obarray *ob,symref new_entry,add_option conflict_opt,int append);
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
//type punning macros
#define toSymbol(sym) (*(symbol*)&sym)
#define toSymref(ref) (*(symref*)&(ref))
#define KEYWORD_COMPARE(name,var)               \
  (var.val.int64 == (getKeySymSexp(name)).val.int64)

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
static inline CORD get_docstring(symref lisp_var){
  return lisp_var->props.doc;
}
#endif
