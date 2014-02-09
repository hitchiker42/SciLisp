#include "common.h"
#include "env.h"
#include "hash.h"
static symbol_name* __attribute__((pure))
  make_symbol_name_maybe_copy(const char *name,uint32_t len,
                              uint64_t hashv,int mb,int copy){
  if(!len){
    len=strlen(name);
  }
  if(!hashv){
    //hash_function is a macro, so this isn't examining
    //global memory
    hashv=hash_function(name,len);
  }
  struct symbol_name *retval;
  if(copy){
    //while struct symbol_name technically has a pointer in it
    //gc won't free it since it's allocated in the same chunk of memory
    retval=xmalloc_atomic(sizeof(struct symbol_name)+len);
    retval->name=((uint8_t*)retval)+offsetof(struct symbol_name,name);
    memcpy((char*)retval->name,name,len);
  } else {
    retval=xmalloc(sizeof(struct symbol_name));
    retval->name=name;
  }
  retval->multibyte=mb;
  //done with name
  retval->hashv=hashv;
  retval->name_len=len;
  return retval;
}
//despite calling a non-const function, this is const
//because it will never take the branch in make_symbol_name
//that requires examining name
symbol_name* __attribute__((const)) 
make_symbol_name_no_copy (const char *name,uint32_t len,
                          uint64_t hashv,int mb){
  return make_symbol_name_maybe_copy(name,len,hashv,mb,0);
}
symbol_name* __attribute__((pure)) 
make_symbol_name (const char *name,uint32_t len,
                          uint64_t hashv,int mb){
  return make_symbol_name_maybe_copy(name,len,hashv,mb,1);
}
symbol *make_symbol(const char *name,uint32_t len){
  struct symbol_name *sym_name=make_symbol_name(name,len,0);
  symbol *retval=xmalloc(sizeof(symbol));
  retval->name=sym_name;
  retval->next=NULL;
  retval->plist=NIL;
  return retval;
}
sexp make_symbol_lisp(sexp sym_name){
  if(!STRINGP(sym_name)){
    raise_simple_error(Etype,"Invalid argument to make-symbol expected a string");
  }
  return symref_sexp
    (make_symbol
     (CORD_to_const_char_star(sym_name.val.string->cord),
      sym_name.val.string->len));
}
sexp gensym(){
  //return #:G<num>
  char *restrict name=xmalloc(16*sizeof(char));
  //probably better to include libatomic_ops
  //and do AO_fetch_and_add1full(&gensym_counter)
  snprintf(name,16,"#:G%d",gensym_counter++);
  return symref_sexp(make_symbol(name,strlen(name)));
}
symbol *copy_symbol(symbol *sym,int copy_props){
  symbol *retval=xmalloc(sizeof(symbol));
  retval->name=sym->name;
  retval->next=NULL;
  retval->plist=(copy_props?sym->plist:NIL);
  retval->val=UNBOUND;
  return retval;
}
static obarray* obarray_rehash(obarray *ob){
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
  struct symbol *sym,*old_sym;
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
  return ob;
}
obarray *make_obarray(uint32_t size,float gthreshold,float gfactor){
#ifdef MULTI_THREADED
  obarray *ob=xmalloc(sizeof(struct obarray)+
                                        sizeof(pthread_rwlock_t));
  ob->lock=(pthread_rwlock_t *restrict)((uint8_t*)ob+
                                        offsetof(struct obarray,lock));
  pthread_rwlock_init(ob->lock,NULL);
#else
  struct obarray *ob=xmalloc(sizeof(struct obarray));
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
  ob->entries++;
  ob->capacity+=ob->capacity_inc;
  if(ob->capacity>=ob->gthreshold){
    obarray_rehash(ob);
    return 1;
  }
  return 0;
}
//use only to initialize primitives at startup, run within pthread_once
//assumes that the symbol argument is not in the obarray already 
void c_intern_unsafe(obarray *ob,symbol* new){
  uint32_t index=new->name->hashv % ob->size;
  //  struct symbol *sym;
  if(!(ob->buckets[index])){
    ob->buckets[index]=new;
    new->next=0;
  } else {
    new->next=ob->buckets[index];
    ob->buckets[index]=new;
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
    ob=current_obarray;
  }
  multithreaded_only(pthread_rwlock_rdlock(ob->lock));
  uint32_t bucket=sym_name->hashv % ob->size;
  struct symbol *cur_symbol;
  if(!(cur_symbol=ob->buckets[bucket])){
    return NULL;
  } else {
    do {
      if(cur_symbol->name->hashv == sym_name->hashv){
        if(!strcmp(cur_symbol->name->name,sym_name->name)){
          multithreaded_only(pthread_rwlock_unlock(ob->lock));
          return cur_symbol;
        }
      }
    }  while((cur_symbol=cur_symbol->next));
  }
  multithreaded_only(pthread_rwlock_unlock(ob->lock));
  return NULL;
}
symbol* lookup_symbol(const char* name,obarray *ob){
  symbol_name* sym_name=make_symbol_name(name,0,0);
  return obarray_lookup_sym(sym_name,ob);
}
static inline symbol *c_intern_maybe_copy(const char *name,uint32_t len,
                                          obarray *ob,int copy){
  if(!ob){
    ob=current_obarray;
  }
  if(!len){
    len=strlen(name);
  }
  uint64_t hashv=hash_function(name,len);
  multithreaded_only(pthread_rwlock_rdlock(ob->lock);)
  uint32_t bucket=hashv % ob->size;
  struct symbol *cur_symbol;
  if(!(cur_symbol=ob->buckets[bucket])){
    ob->used++;
    goto make_symbol;
  } else {
    do {
      if(cur_symbol->name->hashv == hashv){
        if(!strcmp(cur_symbol->name->name,name)){
          multithreaded_only(pthread_rwlock_unlock(ob->lock));
          return cur_symbol;
        }
      }
    }  while((cur_symbol=cur_symbol->next));
  }
 make_symbol:
  multithreaded_only(pthread_rwlock_unlock(ob->lock));
  //allocate the name seperately so we can do it atomically(gc atomically)
  struct symbol_name *new_symbol_name = 
    (copy ? make_symbol_name(name,len,hashv) :
     make_symbol_name_no_copy(name,len,hashv));
  struct symbol *retval=xmalloc(sizeof(struct symbol));
  retval->interned=1;
  retval->name=new_symbol_name;
  retval->val=UNBOUND;
  retval->plist=NIL;
  multithreaded_only(pthread_rwlock_wrlock(ob->lock));
  //set the new symbol to be the new head of the bucket and point to the current head
  retval->next=ob->buckets[bucket];
  //this needs to be done under a lock
  ob->buckets[bucket]=retval;
  maybe_rehash_obarray(ob);
  multithreaded_only(pthread_rwlock_unlock(ob->lock);)
  return (struct symbol *)retval;
}
symbol* c_intern(const char* name,uint32_t len,obarray *ob){
  return c_intern_maybe_copy(name,len,ob,1);
}
symbol* c_intern_no_copy(const char* name,uint32_t len,obarray *ob){
  return c_intern_maybe_copy(name,len,ob,0);
}
//add unintern later
sexp lisp_intern(sexp sym_name,sexp ob){
  const char *name;
  if(STRINGP(sym_name)){
    name=CORD_to_const_char_star(sym_name.val.string->cord);
    /*  } else if (SYMBOLP(sym_or_name)){
        name=sym_or_name.val.sym->name->name;*/
  } else {
    raise_simple_error(Etype,format_type_error("intern","string or symbol",sym_name.tag));
  }
#ifndef OBARRAYP
#define OBARRAYP(obj) 0
#endif
  if(!OBARRAYP(ob)){
    raise_simple_error(Etype,format_type_error("intern","obarray",ob.tag));
  }
  return symref_sexp(c_intern(name,sym_name.val.string->len,ob.val.ob));
}
