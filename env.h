#ifndef __ENV_H__
#define __ENV_H__
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
#define getGlobalSymMacro(name,Var)                                  \
  HASH_FIND_STR(globalSymbolTable.head,(const char *)name,Var)
#define addGlobalSymMacro(Var)                                          \
  HASH_ADD_KEYPTR(hh, globalSymbolTable.head, Var->name, strlen(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
#define getKeySymMacro(name,Var)                                  \
  HASH_FIND_STR(keywordSymbols.head,(const char *)name,Var)
#define addKeySymMacro(Var)                                          \
  HASH_ADD_KEYPTR(hh, keywordSymbols.head, Var->name, strlen(Var->name), Var)
#endif
