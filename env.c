/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
//#include "env.h"
local_symref getLocalSym(local_env *cur_env,CORD name);
symref getFunctionSym(function_env *cur_env,CORD name);
symref getSym(env *cur_env,CORD name){
  switch(cur_env->tag){
    case _global:{
      HERE();
      global_symref tempsym;
      getGlobalSymMacro(name,tempsym);
      HERE();
      return *(symref*)&tempsym;
      //return (sexp){.tag=_sym,.val={.var=tempsym}};
    }
    case _local:
      return (symref)getLocalSym((local_env*)cur_env,name);
    case _funArgs:
      HERE();
      return (symref)getFunctionSym((function_env*)cur_env,name);
    default:
      fprintf(stderr,"shouldn't get here");
      exit(1);
  }
}
global_symref getGlobalSym(CORD name){
  global_symref tempsym;
  getGlobalSymMacro(name,tempsym);
  return tempsym;
}
symref addGlobalSym(symref Var){
  global_symref GlobalVar=xmalloc(sizeof(global_symbol));
  GlobalVar->name=Var->name;
  GlobalVar->val=Var->val;
  GlobalVar->symbol_env=&topLevelEnv;
  addGlobalSymMacro(GlobalVar);
  PRINT_MSG(GlobalVar->name);
  return toSymref(GlobalVar);
}
local_symref getLocalSym(local_env *cur_env,CORD name){
  local_symref cur_sym=cur_env->head;
  while(cur_sym != NULL){
    if(!CORD_cmp(cur_sym->name,name)){
      return cur_sym;
    }
    cur_sym=cur_sym->next;
  }
  return (local_symref)getSym(cur_env->enclosing,name);
}
//perhaps I should check to see if variables exist
//and redifine them
symref addLocalSym(env *cur_env,symref Var){
  local_env *cur_lenv=(local_env*)cur_env;
  local_symref LocalVar=xmalloc(sizeof(local_symbol));
  LocalVar->next=cur_lenv->head;
  LocalVar->name=Var->name;
  LocalVar->val=Var->val;
  LocalVar->symbol_env=cur_env;
  cur_lenv->head=LocalVar;
  return toSymref(LocalVar);
}
symref addSym(env *cur_env,symref Var){
  switch (cur_env->tag){
    case _global:
      return addGlobalSym(Var);
    case _local:
      return addLocalSym(cur_env,Var);
    case _funArgs:
      return NULL;
  }
}
symref getFunctionSym(function_env* cur_env,CORD name){
  function_args* args=cur_env->head;
  int i;
  HERE();
  for(i=0;i<args->max_args;i++){
    if(!CORD_cmp(name,args->args[i].name)){
      return args->args+i;
    }
  }
  return getSym(cur_env->enclosing,name);
}
sexp getKeySymSexp(CORD name){
  keyword_symref keysym;
  getKeySymMacro(name,keysym);
  if(keysym){PRINT_FMT("%#0d",keysym);return (sexp){.tag=_keyword,.val={.keyword=keysym}};}
  keysym=xmalloc(sizeof(keyword_symbol));
  keysym->name=name;
  addKeySymMacro(keysym);
  PRINT_FMT("%#0d",keysym);
  return (sexp){.tag=_keyword,.val={.keyword=keysym}};
}
//needs to move to somewhere else.

    
/*array_symref getArraySym(array_env args,CORD name){
  int i,len=args.head[0].index;
  array_symref arr=args.head;//array_symref=array_symbol*=array of array symbols
  for(i=0;i<len;i++){
    if(!CORD_cmp(arr[i].name,name)){
      return arr[i];
    }
  }
  return (array_symref)getSym(args.enclosing,name);
  }*/
