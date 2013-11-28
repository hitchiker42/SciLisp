/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef HASH_H_
#define HASH_H_
#include "common.h"
#include "prim.h"
#include "hash_fn.h"
//ht=hash_table
typedef struct hash_table hash_table;
typedef struct hash_entry hash_entry;
//hash tables use a doubly linked list rather than cons cells internally
struct hash_entry {
  hash_entry *prev;
  hash_entry *next;
  sexp key;
  sexp val;
  uint64_t hashv;
};
struct hash_table {
  hash_entry **buckets;//points to first bucket in array of (pointers to) buckets
  uint32_t size;
  uint32_t used;
  uint32_t entries;
  int is_weak_hash;
  float capacity;//average bucket capicity
  float capacity_inc;//convience value
  float gthresh://growth threshold, in terms of capicity
  float gfactor;//amount to grow by
  froat sthresh;//shrink threshold, defaults to 0,i.e never shrink
  float sfatcor;
  uint64_t (*hash_fn)(const void*,int);
  sexp (*hash_cmp)(sexp,sexp);
};
/* return the total number of buckets in the hash table */
sexp hashTable_size(sexp ht) __attribute__((pure));
/* Return the number of slots in use (non-empty buckets).  */
sexp hashTable_buckets_used(sexp ht) __attribute__((pure));
/* Return the number of active entries.  */
sexp hashTable_num_entries(sexp ht) __attribute__((pure));
/* Return the length of the longest chain (bucket).  */
sexp hashTable_max_length(sexp ht) __attribute__((pure));
sexp hashTable_avg_capacity(sexp ht)__attribute__((pure));
sexp hashTable_growth_threshold(sexp ht)__attribute__((pure));
sexp hashTable_growth_factor(sexp ht)__attribute__((pure));
sexp hashTable_shrink_threshold(sexp ht)__attribute__((pure));
sexp hashTable_shrink_factor(sexp ht)__attribute__((pure));
/* If ENTRY matches an entry already in the hash table, return the
   entry from the table.  Otherwise, return NIL.  */
sexp hashTable_get_entry(sexp ht, sexp key);
sexp hashTable_add_entry(sexp ht,sexp key,sexp value);
sexp hashTable_walk(sexp ht,sexp walk_fn);
sexp hashTable_lisp_rehash(sexp ht);
//all arguments are optional, should probably be keyargs
//also should proba
sexp makeHashTable(sexp comp_fun,sexp size,sexp hash_fn,
                   sexp growth_threshold,sexp growth_factor,
                   sexp shrink_threshold,sexp shrink_factor);
sexp hashTable_reinit(sexp ht,sexp comp_fun,sexp size,sexp hash_fn,
                   sexp growth_threshold,sexp growth_factor,
                   sexp shrink_threshold,sexp shrink_factor);
//hash an sexp
uint64_t hash_sexp(sexp key,sexp hash_fn);
static sexp lisp_hash_sexp(sexp key,sexp hash_fn){
  return ulong_sexp(hash_sexp(key,hash_fn));
}
#endif
