#include "common.h"
//#include "env.h"
local_symref getLocalSym(local_env cur_env,CORD name);
global_symref getGlobalSym(CORD name){
  global_symref tempsym;
  getGlobalSymMacro(name,tempsym);
  return tempsym;
}
symref addGlobalSym(symref Var){
  global_symref GlobalVar=xmalloc(sizeof(global_symbol));
  GlobalVar->name=Var->name;
  GlobalVar->val=Var->val;
  addGlobalSymMacro(GlobalVar);
  PRINT_MSG(GlobalVar->name);
  return toSymref(GlobalVar);
}
symref getSym(env cur_env,CORD name){
  switch(cur_env.tag){
    case _global:{
      global_symref tempsym;
      getGlobalSymMacro(name,tempsym);
      return *(symref*)&tempsym;
      //return (sexp){.tag=_sym,.val={.var=tempsym}};
    }
    case _local:
      return (symref)getLocalSym((*(local_env*)&cur_env),name);
  }
}
local_symref getLocalSym(local_env cur_env,CORD name){
  local_symref cur_sym=cur_env.head;
  while(cur_sym != NULL){
    if(!CORD_cmp(cur_sym->name,name)){
      return cur_sym;
    }
    cur_sym=cur_sym->next;
  }
  return (local_symref)getSym(*cur_env.enclosing,name);
}
symref addLocalSym(local_env cur_env,symref Var){
  local_symref LocalVar=xmalloc(sizeof(local_symbol));
  LocalVar->next=cur_env.head;
  LocalVar->name=Var->name;
  LocalVar->val=Var->val;
  cur_env.head=LocalVar;
  return toSymref(LocalVar);
}
symref addSym(env cur_env,symref Var){
  switch (cur_env.tag){
    case _global:
      return addGlobalSym(Var);
    case _local:
      return addLocalSym((*(local_env*)&cur_env),Var);
  }
}
