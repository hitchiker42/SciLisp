/*****************************************************************
 * Copyright (C) 2013-2014 Tucker DiNapoli                       *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "env.h"
#include "hash.h"
symbol_name* make_symbol_name(char *restrict name,uint32_t len,uint64_t hashv){
  if(!len){
    len=strlen(name);
  }
  if(!hashv){
    hashv=fnv_hash(name,len);
  }
  //while struct symbol_name technically has a pointer in it
  //gc obviously won't free it since it's allocated in the same chunk of memory
  struct symbol_name *retval=xmalloc_atomic(sizeof(struct symbol_name)+len);
  retval->name=((uint8_t*)retval)+offsetof(struct symbol_name,name);
  memcpy(retval->name,name,len);
  //done with name
  retval->hashv=hashv;
  retval->len=len;
  return retval;
}
symbol *make_new_symbol(char *restrict name,uint32_t len){
  struct symbol_name *sym_name=make_symbol_name(name,len);
  symbol *retval=xmalloc(sizeof(symbol));
  retval->name=sym_name;
  retval->next=NULL;
  retval->plist=NIL;
  return retval;
}
symbol *copy_symbol(symbol *sym,int copy_props){
  symbol *retval=xmalloc(sizeof(symbol_new));
  retval->name=sym->name;
  retval->next=NULL;
  retval->plist=(copy_props?sym->plist:NIL);
  retval->val=UNBOUND;
  return retval;
}
static obarray* obarray_new_rehash(obarray *ob){
  PRINT_MSG("Rehashing Obarray");
  uint32_t old_len=ob->size;
#ifdef OB_POW_OF_2
  ob->size<<=1;
#else
  ob->size=(uint32_t)(ceil(ob->size*ob->gthreshold));
#endif
  ob->capacity/=ob->gthreshold;
  ob->capacity_inc/=ob->gthreshold;
  ob->buckets=xrealloc(ob->buckets,(sizeof(symbol*)*ob->size));
  //suprisingly important, new memory needs to be zeroed
  memset((void*)(ob->buckets+old_len),'\0',old_len);
  int i,j,new_index;
  struct symbol_new *sym,*old_sym;
  for(i=0;i<old_len;i++){
    sym=ob->buckets[i];
    while(sym && sym != sym->next){
      //if hashv%ob->size = hashv%ob->oldsize we don't need to do anything
      //since hashv%ob->oldsize == i we just test i
      if((new_index=sym->name->hashv%ob->size)==i){
        sym=sym->next;
      } else {
        old_sym=sym;
        if(sym->next){
          sym=sym->next;
        } else {
          ob->used--;
        }
        if(!ob->buckets[new_index]){
          ob->buckets[new_index]=old_sym;
          ob->used++;
        } else {
          old_sym->next=ob->buckets[new_index];
          ob->buckets[new_index]=old_sym;
        }
      }
    }
  }
  return 1;
}
obarray *make_obarray_new(uint32_t size,float gthreshold,float gfactor){
#ifdef MULTI_THREADED
  obarray *ob retval=xmalloc(sizeof(struct obarray)+
                                        sizeof(pthread_rwlock_t));
  ob->lock=(pthread_rwlock_t *restrict)((uint8_t*)ob+
                                        offsetof(struct obarray,lock));
  pthread_rwlock_init(ob->lock,NULL);
#else
  struct obarray *ob retval=xmalloc(sizeof(struct obarray));
#endif
  ob->buckets=xmalloc(sizeof(symbol*)*size);
  ob->size=size;
  ob->capacity_inc=1/(size*10);
  ob->gthreshold=gthreshold;
  ob->gfactor=gfactor;
  return ob;
}
//call after adding an element to the obarray
static inline int maybe_rehash_obarray(obarray *ob){
  ob->elements++;
  ob->capacity+=ob->capacity_inc;
  if(ob->capacity>=ob->gthreshold){
    obarray_new_rehash(ob);
    return 1;
  }
  return 0;
}
//use only to initialize primitives at startup, run within pthread_once
//assumes that the symbol argument is not in the obarray already 
void c_intern_unsafe(obarray *ob,symbol* new){
  uint32_t index=sym->name->hashv % ob->size;
  struct symbol *sym;
  if(!(new=ob->buckets[index])){
    ob->buckets[index]=sym;
    sym->next=0;
  } else {
    sym->next=ob->buckets[index];
    ob->buckets[index]=sym;
  }
  maybe_rehash_obarray(ob);
  return;
}
int c_is_interned(symbol *sym,obarray *ob){
  uint32_t bucket=sym->name->hashv%ob->size;
  //if we assume symbols are truely singular values
  symbol *cur_sym=ob->buckets[bucket];
  while(cur_sym){
    if(cur_sym != sym){
      cur_sym=cur_sym->next;
    } else {
      return 1;
    }
  }
  return 0;
  //if not
  while(cur_sym){
    if(cur_sym->name->hashv == sym->name->hashv){
      if(!strcmp(cur_sym->name->name,sym->name->name)){
        return 1;
      }
    }
    cur_sym=cur_sym->next;
  }
  return 0;
}
symbol *obarray_lookup_sym(symbol_name *sym_name,obarray *ob){
  if(!ob){
    ob=current_obarary;
  }
  multithreaded_only(pthread_rwlock_rdlock(ob->lock));
  uint32_t bucket=sym_name->hashv % ob->size;
  struct symbol *cur_symbol;
  if(!(cur_symbol=ob->buckets[bucket])){
    return NULL;
  } else {
    do {
      if(cur_symbol->name->hashv == sym_name->hashv){
        if(!strcmp(cur_symbol->name,sym_name->name)){
          multithreaded_only(pthread_rwlock_unlock(ob->lock));
          return cur_symbol;
        }
      }
    }  while((cur_symbol=cur_symbol->next));
  }
  multithreaded_only(pthread_rwlock_unlock(ob->lock));
  return NULL;
}
    
symbol* c_intern(const char* name,uint32_t len,obarray *ob){
  if(!ob){
    ob=current_obarray;
  }
  if(!len){
    len=strlen(name);
  }
  uint64_t hashv=fnv_hash(name,len);
  multithreaded_only(pthread_rwlock_rdlock(ob->lock);)
  uint32_t bucket=hashv % ob->size;
  struct symbol *cur_symbol;
  if(!(cur_symbol=ob->buckets[bucket])){
    ob->used++;
    goto make_symbol;
  } else {
    do {
      if(cur_symbol->name->hashv == hashv){
        if(!strcmp(cur_symbol->name,name)){
          multithreaded_only(pthread_rwlock_unlock(ob->lock));
          return cur_symbol;
        }
      }
    }  while((cur_symbol=cur_symbol->next));
  }
 make_symbol:
  multithreaded_only(pthread_rwlock_unlock(ob->lock));
  //allocate the name seperately so we can do it atomically(gc atomically)
  struct symbol_name *new_symbol_name=make_symbol_name(name,len,hashv);
  new_symbol_name->interned=1;
  struct symbol *retval=xmalloc(sizeof(struct symbol));
  retval->name=new_symbol_name;
  retval->val=UNBOUND;
  retval->plist=NIL;
  multithreaded_only(pthread_rwlock_wrlock(ob->lock;))
  //set the new symbol to be the new head of the bucket and point to the current head
  retval->next=ob->buckets[bucket];
  //this needs to be done under a lock
  ob->buckets[bucket]=retval;
  maybe_rehash_obarray(ob);
  multithreaded_only(pthread_rwlock_unlock(ob->lock);)
  return (struct symbol *)retval;
}
//add unintern later
sexp lisp_intern(sexp sym_or_name,sexp ob){
  char *name;
  if(STRINGP(sym_or_name)){
    name=CORD_to_const_char_star(sym_or_name);
  } else if (SYMBOLP(sym_or_name)){
    name=sym_or_name.val.sym->name->name;
  } else {
    return format_type_error("intern","string or symbol",sym_or_name.tag);
  }
#ifndef OBARRAYP(obj)
#define OBARRAYP(obj) 0
#endif
  if(!OBARRAYP(obarray)){
    return format_type_error("intern","obarray",ob.tag);
  }
  return c_intern(ob.val.ob,name);
}
//call stack, needs a lot of work, but it's something I probably should add
#define STACK_SIZE 128
function call_stack[STACK_SIZE];
function *stack_ptr=call_stack;
#define push_fun(fun) *stack_ptr=fun;stack_ptr++
#define get_fun(fun) fun=*stack_ptr
#define pop_fun(fun) stack_ptr--
#define peek_fun() (*stack_ptr)
#define check_underflow() (if (stack_ptr<call_stack){handle_error();}
#define check_overflow() (if (stack_ptr>(call_stack+STACK_SIZE)){handle_error()};
//I suppose this fits here
sexp lisp_gensym(){
  symref retval=xmalloc(sizeof(symbol));
  CORD_sprintf(&retval->name,"#:%ld",global_gensym_counter++);
  retval->val=UNBOUND;
  return symref_sexp(retval);
}
