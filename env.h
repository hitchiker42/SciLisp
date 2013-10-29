#ifndef __ENV_H__
#define __ENV_H__
#include<uthash.h>
struct symbol{
  CORD name;
  sexp val;
  env* symbol_env;
};
struct global_symbol{
  CORD name;
  sexp val;
  env* symbol_env;
  UT_hash_handle hh;
};
struct local_symbol{
  CORD name;
  sexp val;
  env* symbol_env;
  local_symref next;
};
struct local_env{
  env* enclosing;
  local_symref head;
};
struct global_env{
  env* enclosing;
  global_symref head;
};
struct function_env{
  env* enclosing;
  function_args* head;//for consistancy in naming
};
union symbol_ref{
  global_symref global;
  local_symref local;
  function_args* function;
};
union symbol_val{
  local_symbol local;
  global_symbol global;
};
struct env{
  env* enclosing;
  symbol_ref head;  
  enum {
    _local=0,
    _global=1,
    _funArgs=2,
  } tag;
};
#define to_env (cur_env,type)\
  (env){.enclosing=cur_env->enclosing,.head=cur_env->head,.tag=type}
global_env globalSymbolTable;
env topLevelEnv;
global_env keywordSymbols;
//basically env.h(move to seperate file?)
local_symref getLocalSym(local_env *cur_env,CORD name);
global_symref getGlobalSym(CORD name);
sexp getKeySymSexp(CORD name);
symref getSym(env *cur_env,CORD name);
symref addSym(env *cur_env,symref Var);
symref addGlobalSym(symref Var);
symref addLocalSym(env *cur_env,symref Var);
//type punning macros
#define toSymbol(sym) (*(symbol*)&sym)
#define toSymref(ref) (*(symref*)&(ref))
static inline size_t symbolSize(env *cur_env){
  switch(cur_env->tag){
    case _global:
      return sizeof(global_symbol);
    case _local:
      return sizeof(local_symbol);
 }
}
#define getGlobalSymMacro(name,Var)                             \
  HASH_FIND_STR(globalSymbolTable.head,CORD_to_const_char_star(name),Var)
#define addGlobalSymMacro(Var)                                          \
  HASH_ADD_KEYPTR(hh, globalSymbolTable.head, Var->name, CORD_len(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
#define getKeySymMacro(name,Var)                                  \
  HASH_FIND_STR(keywordSymbols.head,(const char *)name,Var)
#define addKeySymMacro(Var)                                          \
  HASH_ADD_KEYPTR(hh, keywordSymbols.head, Var->name, strlen(Var->name), Var)
//not sure if this should be a parameter
#define OBARRAY_BKT_CAPACITY 10
struct obarray {  
  obarray_entry **buckets;//points to first bucket
  int size;//memory allocated for the obarray
  int used;//buckets used, should usually be equal to num_buckets
  int entries;//number of obarray_entries 
  float capacity;//sum of entries per buckets for all buckets/num_buckets
  float capicity_inc;//1/(size*10)
  float gthresh;//growth threshold
  float gfactor;//growth factor
  int is_weak_hash;//only actually needs a single bit
  unsigned long (*hash_fn)(void*,int);
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
obarray* obarray_init(float gthresh,uint64_t(*hash_fn)(void*,long),
                      int64_t num_buckets,int32_t is_weak_hash);
obarray* obarray_init_default(int64_t num_buckets);
obarray_entry*  obarray_add_entry_generic
(obarray *ob,symref new_entry,enum add_option conflict_opt,int append);
obarray_entry*  obarray_add_entry(obarray *ob,symref new_entry{
int obarray_rehash(obarray *ob);
obarray_entry* obarray_get_entry(obarray *cur_obarray,CORD symname,uint64_t hashv);
obarray_entry* obarray_remove_entry(obarray *cur_obarray,CORD symname);
#endif
