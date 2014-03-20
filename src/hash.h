/* Header file for lisp hash tables

   Copyright (C) 2013-2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
#ifndef HASH_H_
#define HASH_H_
#include "common.h"
//#include "prim.h"
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
  float capacity;//average bucket capacity
  float capacity_inc;//convience value
  float gthreshold;//growth threshold, in terms of capacity
  float gfactor;//amount to grow by
  //  float sthresh;//shrink threshold, defaults to 0,i.e never shrink
  //  float sfactor;
  uint64_t (*hash_fn)(const void*,int);
  sexp (*hash_cmp)(sexp,sexp);
  symbol *test_fn;
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
static const char *hashtable_test_fn_name(int test_fn){
  switch(test_fn){
    case(hash_eq):
      return "eq";
    case(hash_eql):
      return "eql";      
    case(hash_equal):
      return "equal";
  }
}
sexp hashtable_test_fn(sexp ht);
sexp hashtable_get_entry(sexp ht, sexp key);
sexp hashtable_add_entry(sexp ht,sexp key,sexp value,sexp add_opt);
sexp hashtable_walk(sexp ht,sexp walk_fn);
sexp hashtable_lisp_rehash(sexp ht);
sexp make_hashtable_default();
//all arguments are optional, should probably be keyargs
sexp make_hashtable(sexp comp_fun,sexp size,sexp hash_fn,
                   sexp growth_threshold,sexp growth_factor,
                   sexp shrink_threshold,sexp shrink_factor);
sexp hashtable_reinit(sexp ht,sexp comp_fun,sexp size,sexp hash_fn,
                   sexp growth_threshold,sexp growth_factor,
                   sexp shrink_threshold,sexp shrink_factor);
//hash a(n) sexp
uint64_t hash_sexp(sexp key,sexp hash_fn);
sexp lisp_hash_sexp(sexp obj);
sexp hashtable_delete_key(sexp ht_sexp,sexp key);
//#include "hash_fn.h"//has a bunch of stuff that currently we don't need
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
static uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
#ifdef __X86_64__//never defined cause its __x86_64__
#include "city_hash.h"
#define hash_function CityHash64
#else
#define hash_function fnv_hash
#endif
#endif
