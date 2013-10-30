/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "env.h"
#include "hash_fn.h"
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
/*obarray implementation (as usual with lisp, a name for historical reasons)*/
const struct timespec one_ms={.tv_nsec=1e6};
struct timespec rmtp;
obarray* obarray_init_custom(float gthresh,uint64_t(*hash_fn)(const void*,int),
                             uint64_t size,int is_weak_hash){
  obarray *new_obarray=xmalloc(sizeof(obarray));
  if(gthresh <=0 || gthresh > 1){
    gthresh = 0.75;
  }
  if(!hash_fn){
    hash_fn=fnv_hash;
  }
  if(!size){
    size=16;
    //insure size is a power of 2
  } else if(size & (size - 1)){//size == 2^n <=> size&(size-1) == 0
    while(size & (size -1)){size=size&(size-1);}
    size<<=1;
  }
  obarray_entry **buckets=xmalloc(sizeof(obarray_entry*)*size);
  *new_obarray=(obarray)
    {.buckets=buckets,.size=size,.used=0,.entries=0,.capacity=0.0,
     .capacity_inc=(1.0/(size*10)),.gthresh=gthresh,.gfactor=2,
     .is_weak_hash=is_weak_hash,.hash_fn=hash_fn};
  return new_obarray;
}
obarray* obarray_init_default(uint64_t size){
  if(size==0){size=16;}
  //insure size is a power of two
  else if(size & (size - 1)){//size == 2^n <=> size&(size-1) == 0
    while(size & (size -1)){size=size&(size-1);}
    size<<=1;
  }
  obarray* ob=xmalloc(sizeof(obarray));
  ob->buckets=xmalloc(size*sizeof(obarray_entry*));
  *ob=(obarray){.buckets=ob->buckets,.size=size,.used=0,.entries=0,.capacity=0.0,
                .capacity_inc=(1.0/(size*10)),.gthresh=0.75,.gfactor=2,
                .is_weak_hash=0,.hash_fn=fnv_hash};
  return ob;
}
obarray* obarray_init(uint64_t size,float gthresh){
  if(size==0){size=16;}
  //insure size is a power of two
  else if(size & (size - 1)){//size == 2^n <=> size&(size-1) == 0
    while(size & (size -1)){size=size&(size-1);}
    size<<=1;
  }
  if(gthresh > 1 || gthresh < 0){
    gthresh = 0.75;
  }
  obarray* ob=xmalloc(sizeof(obarray));
  ob->buckets=xmalloc(size*sizeof(obarray_entry*));
  *ob=(obarray){.buckets=ob->buckets,.size=size,.used=0,.entries=0,.capacity=0.0,
                .capacity_inc=(1.0/(size*10)),.gthresh=gthresh,.gfactor=2,
                .is_weak_hash=0,.hash_fn=fnv_hash};
  return ob;
}
//assume a hash value of 0 is impossible(is it?)
obarray_entry* obarray_get_entry(obarray *cur_obarray,CORD symname,uint64_t hashv){
  if(!hashv){
    hashv=cur_obarray->hash_fn(symname,CORD_len(symname));
  }
  hashv=hashv%cur_obarray->size;
  obarray_entry *bucket_head=cur_obarray->buckets[hashv];
  if(!bucket_head){
    return NULL;
  }
  if(!bucket_head->next){
    return bucket_head;
  } else {
    while(bucket_head && bucket_head != bucket_head->next){
      if(!CORD_cmp(symname,bucket_head->ob_symbol->name)){
        return bucket_head;
      }
      bucket_head=bucket_head->next;
    }
  }
  return 0;
}
obarray_entry* obarray_add_entry_generic
(obarray *ob,symref new_entry,enum add_option conflict_opt,int append){
  if (ob->capacity>=ob->gthresh){
    HERE();
    obarray_rehash(ob);
    HERE();
  }
  uint64_t hashv=ob->hash_fn
    (CORD_as_cstring(new_entry->name),CORD_len(new_entry->name));
  uint64_t index=hashv%ob->size;
  obarray_entry* test=ob->buckets[index];
  if(!ob->buckets[index]){
    ob->buckets[index]=xmalloc(sizeof(obarray_entry));
    *ob->buckets[index]=(obarray_entry)
      {.prev=0,.next=0,.ob_symbol=new_entry,.hashv=hashv};
    ob->used++;
    ob->entries++;
    ob->capacity+=ob->capacity_inc;
    return ob->buckets[index];
  }
  obarray_entry* existing_entry;
  if(!(existing_entry=obarray_get_entry(ob,new_entry->name,hashv))||
     conflict_opt == _ignore){
    //int retval=(conflict_op==_ignore)?_ignore:1;
    if(append){
      obarray_entry *cur_tail=ob->buckets[index];
      while(cur_tail->next){cur_tail=cur_tail->next;}
      cur_tail->next=xmalloc(sizeof(obarray_entry));
      cur_tail->next->prev=cur_tail;
      cur_tail->next->ob_symbol=new_entry;
      cur_tail->next->hashv=hashv;
      ob->entries++;
      ob->capacity+=ob->capacity_inc;
      return cur_tail->next;
    } else {
      obarray_entry* cur_head=ob->buckets[index];
      obarray_entry* new_link=xmalloc(sizeof(obarray_entry));//make new entry
      new_link->ob_symbol=new_entry;
      cur_head->hashv=hashv;
      new_link->next=cur_head;//link new entry to current list
      cur_head->prev=new_link;//llink current list to new entry
      ob->buckets[index]=new_link;//update bucket
      ob->entries++;
      ob->capacity+=ob->capacity_inc;
      return new_link;
    }
  }
  if(conflict_opt< 2 || conflict_opt > 5){
    conflict_opt=0;
  }
  switch(conflict_opt){
    case _update:
      existing_entry->ob_symbol=new_entry;
      return existing_entry;
    case _ignore://unimplemented
    case _overwrite://unimplemented
    case _use_current:
      return existing_entry;
  }
  return 0;
}
obarray_entry* obarray_add_entry(obarray* ob,symref new_entry){
  return obarray_add_entry_generic(ob,new_entry,_ignore,0);
}
int obarray_rehash(obarray *ob){
  HERE();
  uint64_t old_len=ob->size;
  //  ob->size*=ob->gfactor;
  ob->size*=2;
  ob->capacity/=2;
  ob->capacity_inc/=2;
  ob->buckets=xrealloc(ob->buckets,(sizeof(obarray_entry*)*ob->size));
  //suprisingly important, new memory needs to be zeroed
  memset((void*)(ob->buckets+old_len),'\0',old_len);
  int i,j;
  obarray_entry *bucket,*temp,*old_bucket;
  for(i=0;i<ob->size;i++){
    bucket=ob->buckets[i];
    while(bucket && bucket != bucket->next){
      //hashv%ob->size is 0 or oldsize, if it's 0 we need to move it
      if(bucket->hashv%ob->size){
        bucket=bucket->next;
      } else {
        old_bucket=bucket;
        if(bucket->prev){
          bucket->prev->next=bucket->next;
        }
        if(bucket->next){
          bucket->next->prev=bucket->prev;
        }
        if(!ob->buckets[i+old_len]){
          ob->buckets[i+old_len]=bucket;
          bucket=bucket->next;
          old_bucket->prev=NULL;
          old_bucket->next=NULL;
          ob->used++;
        } else {
          temp=ob->buckets[i+old_len];//bucket n == head-> ... 
          ob->buckets[i+old_len]=bucket;//bucket n == new
          temp->prev=bucket;// head-> prev =new
          bucket=bucket->next;
          old_bucket->next=temp;//bucket n = new -> head -> ...
          old_bucket->prev=NULL;
        }
      }
    }
    if(!ob->buckets[i]){ob->used--;}
    PRINT_FMT("loop iteration %d",i);
  }
  HERE();
  return 1;
}
