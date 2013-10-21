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

#ifndef HASH_H_
# define HASH_H_
#include "common.h"
#include <stdint.h>
# include <stdio.h>
# include <stdbool.h>

typedef size_t (*Hash_hasher) (const void *, size_t);
typedef bool (*Hash_comparator) (const void *, const void *);
typedef void (*Hash_data_freer) (void *);
typedef bool (*Hash_processor) (void *, void *);
typedef int (*Hash_Function)(data);
typedef int (*Hash_Compare)(data,data);
typedef struct hash_tuning hash_tuning;
typedef struct hash_table hash_table;
typedef struct hash_entry hash_entry;
struct hash_tuning {
    /* This structure is mainly used for 'hash_initialize', see the block
       documentation of 'hash_reset_tuning' for more complete comments.  */

    float shrink_threshold;     /* ratio of used buckets to trigger a shrink */
    float shrink_factor;        /* ratio of new smaller size to original size */
    float growth_threshold;     /* ratio of used buckets to trigger a growth */
    float growth_factor;        /* ratio of new bigger size to original size */
    bool is_n_buckets;          /* if CANDIDATE really means table size */
  };


/* Information and lookup.  */
size_t hash_get_n_buckets (const hash_table *) __attribute__((pure));
size_t hash_get_n_buckets_used (const hash_table *) __attribute__((pure));
size_t hash_get_n_entries (const hash_table *) __attribute__((pure));
size_t hash_get_max_bucket_length (const hash_table *) __attribute__((pure));
bool hash_table_ok (const hash_table *) __attribute__((pure));
void hash_print_statistics (const hash_table *, FILE *);
void *hash_lookup (const hash_table *, const void *);

/* Walking.  */
void *hash_get_first (const hash_table *) __attribute__((pure));
void *hash_get_next (const hash_table *, const void *);
size_t hash_get_entries (const hash_table *, void **, size_t);
size_t hash_do_for_each (const hash_table *, Hash_processor, void *);

/* Allocation and clean-up.  */
size_t hash_string (const char*, size_t) __attribute__((pure));
void hash_reset_tuning (Hash_tuning *);
hash_table* make_hash_table(size_t, const Hash_tuning*,Hash_function,
                            Hash_Compare, Hash_Weak) _GL_ATTRIBUTE_WUR;
void hash_clear (hash_table*);
void hash_free (hash_table*);

/* Insertion and deletion.  */
bool hash_rehash (hash_table *, size_t) _GL_ATTRIBUTE_WUR;
void *hash_insert (hash_table *, const void *) _GL_ATTRIBUTE_WUR;

int hash_insert_if_absent (hash_table *table, const void *entry,
                           const void **matched_ent);
void *hash_delete (hash_table *, const void *);

#endif
