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
//type alias, should change to one type sooner or later
typedef struct hash_table hashtable;
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
  float gthresh;//growth threshold, in terms of capicity
  float gfactor;//amount to grow by
  float sthresh;//shrink threshold, defaults to 0,i.e never shrink
  float sfactor;
  uint64_t (*hash_fn)(const void*,int);
  sexp (*hash_cmp)(sexp,sexp);
  enum {
    _heq,
    _heql,
    _hequal,
  } test_fn;
};
/* return the total number of buckets in the hash table */
sexp hashtable_size(sexp ht) __attribute__((pure));
/* Return the number of slots in use (non-empty buckets).  */
sexp hashtable_buckets_used(sexp ht) __attribute__((pure));
/* Return the number of active entries.  */
sexp hashtable_num_entries(sexp ht) __attribute__((pure));
/* Return the length of the longest chain (bucket).  */
sexp hashtable_max_length(sexp ht) __attribute__((pure));
sexp hashtable_avg_capacity(sexp ht)__attribute__((pure));
sexp hashtable_growth_threshold(sexp ht)__attribute__((pure));
sexp hashtable_growth_factor(sexp ht)__attribute__((pure));
sexp hashtable_shrink_threshold(sexp ht)__attribute__((pure));
sexp hashtable_shrink_factor(sexp ht)__attribute__((pure));
/* If ENTRY matches an entry already in the hash table, return the
   entry from the table.  Otherwise, return NIL.  */
CORD hashtable_test_fn_name(sexp ht);
sexp hashtable_test_fn(sexp ht);
sexp hashtable_get_entry(sexp ht, sexp key);
sexp hashtable_add_entry(sexp ht,sexp key,sexp value,sexp add_opt);
sexp hashtable_walk(sexp ht,sexp walk_fn);
sexp hashtable_lisp_rehash(sexp ht);
sexp make_hashtable_default();
//all arguments are optional, should probably be keyargs
sexp makeHashtable(sexp comp_fun,sexp size,sexp hash_fn,
                   sexp growth_threshold,sexp growth_factor,
                   sexp shrink_threshold,sexp shrink_factor);
sexp hashtable_reinit(sexp ht,sexp comp_fun,sexp size,sexp hash_fn,
                   sexp growth_threshold,sexp growth_factor,
                   sexp shrink_threshold,sexp shrink_factor);
//hash a(n) sexp
uint64_t hash_sexp(sexp key,sexp hash_fn);
sexp lisp_hash_sexp(sexp obj);
sexp hashtable_delete_key(sexp ht_sexp,sexp key);
#endif
