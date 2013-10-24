/* hash - hashing table processing.
   Copyright (C) 1998-1999, 2001, 2003, 2009-2013 Free Software Foundation,
   Inc.
   Written by Jim Meyering <meyering@ascend.com>, 1998.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* A generic hash table package.  */
/* Modified for use in SciLisp by Tucker DiNapoli (C) 2013 */
#ifndef HASH_H_
# define HASH_H_
#include "common.h"
#include "hash_fn.h"
# include <stdint.h>
# include <stdio.h>
#define _GL_ATTRIBUTE_WUR __attribute__((warn_unused_result))
typedef void (*Hash_Free) (void *);
typedef long (*Hash_processor) (data, data);
typedef long (*Hash_Function) (data);
typedef long (*Hash_Compare) (data,data);
typedef void* (*Hash_Malloc) (size_t);
typedef struct hash_tuning hash_tuning;

/* A hash table contains many internal entries, each holding a pointer to
   some user-provided data (also called a user entry).  An entry indistinctly
   refers to both the internal entry and its associated user entry.  A user
   entry contents may be hashed by a randomization function (the hashing
   function, or just "hasher" for short) into a number (or "slot") between 0
   and the current table size.  At each slot position in the hash table,
   starts a linked chain of entries for which the user data all hash to this
   slot.  A bucket is the collection of all entries hashing to the same slot.

   Long buckets slow down the lookup algorithm.  One might use big hash table
   sizes in hope to reduce the average length of buckets, but this might
   become inordinate, as unused slots in the hash table take some space.  The
   best bet is to make sure you are using a good "hasher" function (beware
   that those are not that easy to write! :-), and to use a table size
   larger than the actual number of entries.  */
typedef struct hash_table hash_table;
typedef struct hash_entry hash_entry;
struct hash_tuning {
  /* This structure is mainly used for 'hash_initialize', see the block
     documentation of 'hash_reset_tuning' for more complete comments.  */
  float shrink_threshold;     /* ratio of used buckets to trigger a shrink */
  float shrink_factor;        /* ratio of new smaller size to original size */
  float growth_threshold;     /* ratio of used buckets to trigger a growth */
  float growth_factor;        /* ratio of new bigger size to original size */
  int is_n_buckets;          /* if CANDIDATE really means table size */
};
/* Information and lookup.  */
/* Return the number of buckets in the hash table.  The table size, the total
   number of buckets (used plus unused), or the maximum number of slots, are
   the same quantity.  */
long hash_get_num_buckets (const hash_table *) __attribute__((pure));
/* Return the number of slots in use (non-empty buckets).  */
long hash_get_num_buckets_used (const hash_table *) __attribute__((pure));
/* Return the number of active entries.  */
long hash_get_num_entries (const hash_table *) __attribute__((pure));
/* Return the length of the longest chain (bucket).  */
long hash_get_max_bucket_length (const hash_table *) __attribute__((pure));
/* Do a mild validation of a hash table, by traversing it and checking two
   statistics.  */
int hash_table_ok (const hash_table *) __attribute__((pure));
void hash_print_statistics (const hash_table *, FILE *);
/* If ENTRY matches an entry already in the hash table, return the
   entry from the table.  Otherwise, return NULL.  */
data hash_lookup (const hash_table *, data);

/* Walking.  */
sexp hash_get_first (const hash_table *) __attribute__((pure));
sexp hash_get_next (const hash_table *, data);
long hash_get_entries (const hash_table *, data*, long);
/* Call a PROCESSOR function for each entry of a hash table, and return the
   number of entries for which the processor function returned success.  A
   pointer to some PROCESSOR_DATA which will be made available to each call to
   the processor function.  The PROCESSOR accepts two arguments: the first is
   the user entry being walked into, the second is the value of PROCESSOR_DATA
   as received.  The walking continue for as long as the PROCESSOR function
   returns nonzero.  When it returns zero, the walking is interrupted.  */
long hash_do_for_each (const hash_table *, Hash_processor, data);

/* Allocation and clean-up.  */
long hash_string (const char*, long) __attribute__((pure));
void hash_reset_tuning (Hash_tuning *);
/* Allocate and return a new hash table, or NULL upon failure.  The initial
   number of buckets is automatically selected so as to _guarantee_ that you
   may insert at least CANDIDATE different user entries before any growth of
   the hash table size occurs.  So, if have a reasonably tight a-priori upper
   bound on the number of entries you intend to insert in the hash table, you
   may save some table memory and insertion time, by specifying it here.  If
   the IS_N_BUCKETS field of the TUNING structure is 1, the CANDIDATE
   argument has its meaning changed to the wanted number of buckets.

   TUNING points to a structure of user-supplied values, in case some fine
   tuning is wanted over the default behavior of the hasher.  If TUNING is
   NULL, the default tuning parameters are used instead.  If TUNING is
   provided but the values requested are out of bounds or might cause
   rounding errors, return NULL.

   The user-supplied HASHER function, when not NULL, accepts two
   arguments ENTRY and TABLE_SIZE.  It computes, by hashing ENTRY contents, a
   slot number for that entry which should be in the range 0..TABLE_SIZE-1.
   This slot number is then returned.

   The user-supplied COMPARATOR function, when not NULL, accepts two
   arguments pointing to user data, it then returns 1 for a pair of entries
   that compare equal, or 0 otherwise.  This function is internally called
   on entries which are already known to hash to the same bucket index,
   but which are distinct pointers.

   The user-supplied FREE_FN function, when not NULL, may be later called
   with the user data as an argument, just before the entry containing the
   data gets freed.  This happens from within 'hash_free' or 'hash_clear'.
   You should specify this function only if you want these functions to free
   all of your 'data' data.  This is typically the case when your data is
   simply an auxiliary struct that you have malloc'd to aggregate several
   values.  */
hash_table* make_hash_table(long, const Hash_tuning*,Hash_function,
                            Hash_Compare, Hash_Weak) _GL_ATTRIBUTE_WUR;
/* Make all buckets empty Apply the user-specified function free_fn
   (if any) to the datas of any affected entries.  */
void hash_clear (hash_table*);
/* Reclaim all storage associated with a hash table.  If a free_fn
   function has been supplied by the user when the hash table was created,
   this function applies it to the data of each entry before freeing that
   entry.  */
void hash_free (hash_table*);

/* Insertion and deletion.  */
int hash_rehash (hash_table *, long) _GL_ATTRIBUTE_WUR;
void *hash_insert (hash_table *, data) _GL_ATTRIBUTE_WUR;

long hash_insert_if_absent (hash_table *table, data entry,
                           const void **matched_ent);
void *hash_delete (hash_table *, const void *);
#endif
