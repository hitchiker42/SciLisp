/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
//#include "env.h"
//looking stuff up, don't need a pointer
local_symref getLocalSym(local_env *cur_env,CORD name);
global_symref getGlobalSym(CORD name){
  global_symref tempsym;
  getGlobalSymMacro(name,tempsym);
  return tempsym;
}
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
symref addGlobalSym(symref Var){
  global_symref GlobalVar=xmalloc(sizeof(global_symbol));
  GlobalVar->name=Var->name;
  GlobalVar->val=Var->val;
  addGlobalSymMacro(GlobalVar);
  PRINT_MSG(GlobalVar->name);
  return toSymref(GlobalVar);
}
symref getSym(env *cur_env,CORD name){
  switch(cur_env->tag){
    case _global:{
      global_symref tempsym;
      getGlobalSymMacro(name,tempsym);
      return *(symref*)&tempsym;
      //return (sexp){.tag=_sym,.val={.var=tempsym}};
    }
    case _local:
      return (symref)getLocalSym((local_env*)cur_env,name);
      //    case _array:
      //      return (symref)GetArraySym((*(array_env*)&cur_env),name);
  }
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
symref addLocalSym(local_env *cur_env,symref Var){
  local_symref LocalVar=xmalloc(sizeof(local_symbol));
  LocalVar->next=cur_env->head;
  LocalVar->name=Var->name;
  LocalVar->val=Var->val;
  cur_env->head=LocalVar;
  return toSymref(LocalVar);
}
symref addSym(env *cur_env,symref Var){
  switch (cur_env->tag){
    case _global:
      return addGlobalSym(Var);
    case _local:
      return addLocalSym((local_env*)cur_env,Var);
  }
}
sexp getFunctionArgs(sexp args,env* cur_env){
  if(args.tag != _funargs){
    handle_error();
  }
  int i,j=0;
  for(i=0;i<args.val.funargs->num_req_args;i++){  
    if(!(CONSP(args))){
      handle_error_str("not enough args");
      handle_error();
    } else {
      args.val.funargs->args[j++]=eval(XCAR(args));
      args=XCDR(args);
    }
  }
  for(i=0;i<args.val.funargs->num_opt_args;i++){
    
  ARGS_END:
