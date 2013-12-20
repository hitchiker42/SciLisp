/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "hash.h"
#include "hash_fn.h"
#define DEFAULT_SIZE 32
#define DEFAULT_GROWTH_THRESHOLD 0.8
#define DEFAULT_GROWTH_FACTOR 1.414f
#define DEFAULT_SHRINK_THRESHOLD 0.0f
#define DEFAULT_SHRINK_FACTOR 1.0f
static hash_table* hashtable_rehash(hash_table *ht);
//assume keyword args are implemented
//bit long, come up with a better way to parse and typecheck
//keyword args
sexp make_hashtable_default(){
  return makeHashtable(NIL,NIL,NIL,NIL,NIL,NIL,NIL);
}
sexp makeHashtable(sexp comp_fn,sexp size,sexp hash_fn,
                   sexp growth_threshold,sexp growth_factor,
                   sexp shrink_threshold,sexp shrink_factor){
  hash_table *ht=xmalloc(sizeof(hash_table));
  if(NILP(comp_fn)){
    ht->hash_cmp=lisp_eq;
    ht->test_fn=_heq;
    //need to add a test for function arity
  } else if(!KEYWORDP(comp_fn)){
    return format_type_error_key("make-hashtable","comp-fun","test",comp_fn.tag);
  } else {
    if(KEYWORD_COMPARE("eq",comp_fn)){
      ht->hash_cmp=lisp_eq;
      ht->test_fn=_heq;
    } else if(KEYWORD_COMPARE("eql",comp_fn)){
      ht->hash_cmp=lisp_eql;
      ht->test_fn=_heql;
    } else if(KEYWORD_COMPARE("equal",comp_fn)){
      ht->hash_cmp=lisp_equal;
      ht->test_fn=_hequal;
    } else {
      return format_error_sexp
        ("invalid hash test function %r",comp_fn.val.keyword->name);
    }
  }
  if(NILP(size)){
    ht->size=DEFAULT_SIZE;
  } else if(!INTP(size)){
    return format_type_error_key("make-hashtable","size","integer",size.tag);
  } else {
    ht->size=size.val.int64;
  }
  //allocate at end, that way we don't waste time if one of the other
  //parameters is erroneous
  if(NILP(hash_fn)){
    ht->hash_fn=fnv_hash;
  } else if(!KEYWORDP(hash_fn)){
    return format_type_error_key("make-hashtable","hash-fun","keyword",hash_fn.tag);
  } else {
    return error_sexp("selectable hash functions unimplemented");
  }
  if(NILP(growth_threshold)){
    ht->gthresh=DEFAULT_GROWTH_THRESHOLD;
    //FIXME: need to change this to be float or double
  } else if(!(REAL64P(growth_threshold))){
    return format_type_error_opt("make-hashtable","growth-threshold"
                                 "floating point",growth_threshold.tag);
  } else {
    double gt=growth_threshold.val.real64;
    if(gt>1 || gt<=0){
      return error_sexp("invalid growth threshold value in make-hashtable");
    } else {
      ht->gthresh=gt;
    }
  }
  if(NILP(growth_factor)){
    ht->gfactor=DEFAULT_GROWTH_FACTOR;
    //FIXME: need to change this to be float or double
  } else if(!(REAL64P(growth_factor))){
    return format_type_error_opt("make-hashtable","growth-factor"
                                 "floating point",growth_factor.tag);
  } else {
    double gf=growth_factor.val.real64;
    if(gf<=1){
      return error_sexp("invalid growth factor value in make-hashtable");
    } else {
      ht->gfactor=gf;
    }
  }
  if(NILP(shrink_threshold)){
    ht->sthresh=DEFAULT_SHRINK_THRESHOLD;
    //FIXME: need to change this to be float or double
  } else if(!(REAL64P(shrink_threshold))){
    return format_type_error_opt("make-hashtable","shrink-threshold"
                                 "floating point",shrink_threshold.tag);
  } else {
    double st=shrink_threshold.val.real64;
    if(st>1 || st<0){
      return error_sexp("invalid shrink threshold value in make-hashtable");
    } else {
      ht->sthresh=st;
    }
  }
  if(NILP(shrink_factor)){
    ht->sfactor=DEFAULT_SHRINK_FACTOR;
    //FIXME: need to change this to be float or double
  } else if(!(REAL64P(shrink_factor))){
    return format_type_error_opt("make-hashtable","shrink-factor"
                                 "floating point",shrink_factor.tag);
  } else {
    double sf=shrink_factor.val.real64;
    if(sf>1 || sf<=0){
      return error_sexp("invalid shrink factor value in make-hashtable");
    } else {
      ht->sfactor=sf;
    }
  }
  //phew, lots of args here
  ht->buckets=xmalloc(sizeof(hash_entry*)*ht->size);
  return hashtable_sexp(ht);
}
#define hash_get_param(name,param_name,type)    \
  sexp hashtable_##name(sexp ht){               \
  if(!HASHTABLEP(ht)){                          \
  return format_type_error("hashtable_"#name,"hashtable",ht.tag);\
  } else {                                                       \
  return(type##_sexp(ht.val.hashtable->param_name));                           \
  }                                                              \
}
hash_get_param(size,size,long);//actually an int, but shhh
hash_get_param(buckets_used,used,long);
hash_get_param(num_entries,entries,long)
hash_get_param(growth_threshold,gthresh,double);
hash_get_param(growth_factor,gfactor,double);
hash_get_param(shrink_threshold,sthresh,double);
hash_get_param(shrink_factor,sfactor,double);
uint64_t hash_sexp(sexp key,sexp hash_fun){
  //temporary untill I actually implement selectable hash functions
  uint64_t(*hash_fn)(const void*,int)=fnv_hash;
  switch(key.tag){
    case _cord:
      return hash_fn(key.val.cord,CORD_len(key.val.cord));
    case _symbol:
      //symbols should hash the same regardless of type, so
      //use only the bits common to all symbols
      return hash_fn(key.val.var,sizeof(symbol));
    case _list:
    case _cons:
      return hash_fn(key.val.cons,sizeof(cons));
    default:
      return hash_fn(key.val.opaque,8);
  }
}
static uint64_t _hash_sexp(sexp key,sexp hash_fn){
  //temporary untill I actually implement selectable hash functions
  //  hash_table *ht=ht_sexp.val.hashtable;
  uint64_t(*hash_fp)(const void*,int);
  hash_fp=fnv_hash;
  switch(key.tag){
    case _cord:
      return hash_fp(key.val.cord,CORD_len(key.val.cord));
    case _symbol:
      //symbols should hash the same regardless of the kind of symbol, so
      //use only the bits common to all symbols
      return hash_fp(key.val.var,sizeof(symbol));
    case _list:
    case _cons:
      return hash_fp(key.val.cons,sizeof(cons));
    default:
      return hash_fp(&key.val.uint64,sizeof(void*));
  }
}
sexp lisp_hash_sexp(sexp obj){
  return uint64_sexp(_hash_sexp(obj,NIL));
}
static hash_entry *_get_entry(hashtable *ht,sexp key){
  uint64_t hashv=_hash_sexp(key,hashtable_sexp(ht));
  uint64_t index=hashv%ht->size;
  hash_entry *bucket_head=ht->buckets[index];
  if(!bucket_head){
    return NULL;
  } else {
    while(bucket_head && bucket_head != bucket_head->next){
      if(isTrue(ht->hash_cmp(bucket_head->key,key))){
        return bucket_head;
      } else {
        bucket_head=bucket_head->next;
      }
    }
    return NULL;
  }
}
sexp hashtable_get_entry(sexp ht_sexp,sexp key){
  if(!HASHTABLEP(ht_sexp)){
    return format_type_error("hashtable-lookup-entry","hashtable",ht_sexp.tag);
  }
  hash_table *ht=ht_sexp.val.hashtable;
  hash_entry *entry=_get_entry(ht,key);
  if(!entry){
    return NIL;
  } else {
    return entry->val;
  }
}
static uint64_t _delete_entry(hash_table *ht,hash_entry *entry){
  if(!entry->prev){//first entry in the bucket
    uint64_t index=entry->hashv%ht->size;
    ht->buckets[index]=entry->next;
    if(entry->next){
      entry->next->prev=NULL;
    } else {
      ht->used--;
    }
  } else {
    entry->prev->next=entry->next;
    if(entry->next){
      entry->next->prev=entry->prev;
    }
  }
  ht->capacity-=ht->capacity_inc;
  ht->entries--;
  return entry->hashv;
}
sexp hashtable_delete_key(sexp ht_sexp,sexp key){
  if(!HASHTABLEP(ht_sexp)){
    return format_type_error("hashtable-delete-key","hashtable",ht_sexp.tag);
  }
  hash_entry *entry=_get_entry(ht_sexp.val.hashtable,key);
  if(!entry){
    return LISP_FALSE;
  } else {
    return uint64_sexp(_delete_entry(ht_sexp.val.hashtable,entry));
  }
} 
sexp hashtable_add_entry(sexp ht_sexp,sexp key,sexp val,sexp add_opt){
  if(!HASHTABLEP(ht_sexp)){
    return format_type_error("hashtable-add-entry","hashtable",ht_sexp.tag);
  }
  hash_table *ht=ht_sexp.val.hashtable;
  uint64_t hashv=_hash_sexp(key,ht_sexp);
  uint64_t index=hashv%ht->size;
  enum add_option conflict_opt;
  if(NILP(add_opt)){
    conflict_opt=_update;
  } else {
    conflict_opt=_update;//implement later
  }
  if(ht->capacity>=ht->gthresh){
    ht_sexp=hashtable_sexp(hashtable_rehash(ht));
  }
  if(!ht->buckets[index]){
    ht->buckets[index]=xmalloc(sizeof(hash_entry));
    *ht->buckets[index]=(hash_entry)
      {.prev=NULL,.next=NULL,.key=key,.val=val,.hashv=hashv};
    ht->used++;
    ht->entries++;
    ht->capacity+=ht->capacity_inc;
    return val;
  }
  hash_entry *existing_entry=_get_entry(ht,key);
  if(conflict_opt==_overwrite){
    _delete_entry(ht,existing_entry);
    existing_entry=NULL;
  }
  if(!existing_entry || conflict_opt==_ignore){
    hash_entry *cur_head=ht->buckets[index];
    hash_entry *new_entry=xmalloc(sizeof(hash_entry));
    new_entry->val=val;
    new_entry->key=key;
    new_entry->hashv=hashv;
    //insert new entry at head of list
    cur_head->prev=new_entry;
    new_entry->next=cur_head;
    ht->buckets[index]=new_entry;
    ht->entries++;
    ht->capacity+=ht->capacity_inc;
    return val;
  } else {
    switch(conflict_opt){
      case _update:
        existing_entry->val=val;//key is the same, I hope
        return val;
      case _use_current:
        return existing_entry->val;
    }
  }
}
static hash_table* hashtable_rehash(hash_table *ht){
  uint64_t old_len=ht->size;
  //update hash parameters
  ht->size*=ht->gfactor;
  ht->capacity/=ht->gfactor;
  ht->capacity_inc/=ht->gfactor;
  ht->buckets=xrealloc(ht->buckets,(sizeof(hash_entry*)*ht->size));
  //suprisingly important, new memory needs to be zeroed
  memset((void*)(ht->buckets+old_len),'\0',old_len);
  int i,j;
  //I use bucket as the name of the variable, but it actually
  //holds the hash entries as we iterate through the bucket
  hash_entry *bucket,*temp,*old_bucket;
  for(i=0;i<old_len;i++){
    bucket=ht->buckets[i];
    while(bucket && bucket != bucket->next){
      if(bucket->hashv%ht->size==i){
        bucket=bucket->next;
      } else {
        old_bucket=bucket;
        if(bucket->prev){
          bucket->prev->next=bucket->next;
        }
        if(bucket->next){
          bucket->next->prev=bucket->prev;
        }
        if(!ht->buckets[i+old_len%ht->size]){
          //this is an unused bucket
          ht->buckets[i+old_len]=bucket;
          bucket=bucket->next;
          old_bucket->prev=old_bucket->next=NULL;
          ht->used++;
        } else {
          //put old bucket list into a temp variable
          temp=ht->buckets[i+old_len%ht->size];
          ht->buckets[i+old_len%ht->size]=bucket;//set bucket to new value;
          temp->prev=bucket;//relink old bucket list
          //get next value in the bucket we're iterating through
          bucket=bucket->next;
          //now link the current value into the bucket list
          old_bucket->next=temp;
          old_bucket->prev=NULL;
        }
      }
    }
    if(!ht->buckets[i]){ht->used--;}
  }
  return ht;
}
sexp hashtable_lisp_rehash(sexp ht){
  if(!HASHTABLEP(ht)){
    return format_type_error("rehash","hashtable",ht.tag);
  } else {
    return hashtable_sexp(hashtable_rehash(ht.val.hashtable));
  }
}
CORD hashtable_test_fn_name(sexp ht){
  switch(ht.val.hashtable->test_fn){
    case _heq:
      return "eq";
    case _heql:
      return "eql";
    case _hequal:
      return "equal";
  }
}
sexp hashtable_test_fn(sexp ht){
  if(!HASHTABLEP(ht)){
    return format_type_error("hashtable-test-fn","hashtable",ht.tag);
  }
  return cord_sexp(hashtable_test_fn_name(ht));
}
