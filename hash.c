/* hash - hashing table processing.

   Copyright (C) 1998-2004, 2006-2007, 2009-2013 Free Software Foundation, Inc.

   Written by Jim Meyering, 1992.

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
/* Modifications for SciLisp (C) 2013 Tucker DiNapoli */
/* Hash table package from gnulib tweaked for use with SciLisp */
/* Note:
 * Its probably best to have the option for weak hash pointers,
 * because gc doesn't actually have weak pointers I think we can
 * emulate them by allocating the hash table with gc_malloc_atomic
 * which means the hash table won't get scanned for pointers so if
 * there are only pointers to an object in the hash table it'll get freed*/
#include "hash.h"

//hash tables use a doubly linked list rather than cons cells internally
struct hash_entry{
  hash_entry *prev;
  data car;
  hash_entry *cdr;
};
/* old hash entry
   hash_entry
   {
   void *data;
   hash_entry *next;
   };
*/
struct hash_table {
  /* The array of buckets starts at BUCKET and extends to BUCKET_LIMIT-1,
     for a possibility of NUM_BUCKETS.  Among those, NUM_BUCKETS_USED buckets
     are not empty, there are NUM_ENTRIES active entries in the table.  */
  _tag value_type;
  hash_entry *buckets;//points to first bucket in array of buckets
  hash_entry *bucket_limit;//""   last  ""
  //we only ever use a pointer to this, so using longs is fine
  long num_buckets;
  long num_buckets_used;
  long num_entries;
  int is_weak_hash;
  /* Tuning arguments, kept in a physically separate structure.  */
  const Hash_tuning *tuning;
  //hash_fn creates a hash from a evalue of type value_type
  //compare_fn compares two values of type value_type for equality
  Hash_Function hash_fn;
  Hash_Compare compare_fn;
  Hash_Malloc allocate_fn;
  //NULL by default, if not null explictly deallocate hash values
  //when they are removed(should only be used if the hash table
  //is the only place the data is used
  Hash_Free free_fn;
};

/* If an insertion makes the ratio of nonempty buckets to table size larger
   than the growth threshold (a number between 0.0 and 1.0), then increase
   the table size by multiplying by the growth factor (a number greater than
   1.0).  The growth threshold defaults to 0.8, and the growth factor
   defaults to 1.414, meaning that the table will have doubled its size
   every second time 80% of the buckets get used.  */
#define DEFAULT_GROWTH_THRESHOLD 0.8
#define DEFAULT_GROWTH_FACTOR 1.414f

/* If a deletion empties a bucket and causes the ratio of used buckets to
   table size to become smaller than the shrink threshold (a number between
   0.0 and 1.0), then shrink the table by multiplying by the shrink factor (a
   number greater than the shrink threshold but smaller than 1.0).  The shrink
   threshold and factor default to 0.0 and 1.0, meaning that the table never
   shrinks.  */
#define DEFAULT_SHRINK_THRESHOLD 0.0f
#define DEFAULT_SHRINK_FACTOR 1.0f

/* Use this to initialize or reset a TUNING structure to
   some sensible values. */
static const Hash_tuning default_tuning ={
  DEFAULT_SHRINK_THRESHOLD,//ratio of num/used buckets needed to shrink table
  DEFAULT_SHRINK_FACTOR,//ratio of new size to old size when shrinking
  DEFAULT_GROWTH_THRESHOLD,//ratio of num/used buckets needed to grow table
  DEFAULT_GROWTH_FACTOR,//ratio of new size to old size when growing
  0,
};

/* Information and lookup.  */

/* The following few functions provide information about the overall hash
   table organization: the number of entries, number of buckets and maximum
   length of buckets.  */
int hash_get_num_buckets (const hash_table *table){
  return table->num_buckets;
}
int hash_get_num_buckets_used (const hash_table *table){
  return table->num_buckets_used;
}
int hash_get_num_entries (const hash_table *table){
  return table->num_entries;
}
int hash_get_max_bucket_length (const hash_table *table) {
  hash_entry const *buckets=table->buckets;
  hash_entry *cursor;
  size_t max_bucket_length = 0;
  size_t bucket_length;
  int i;
  for (i=0;i<table->bucket_limit;i++){
    if (buckets[i]->car) {
      cursor = buckets[i];
      bucket_length = 1;
      while (cursor = cursor->cdr, cursor){
        bucket_length++;
      }
      if (bucket_length > max_bucket_length){
        max_bucket_length = bucket_length;
      }
    }
  }
  return max_bucket_length;
}
int hash_table_ok (const hash_table *table) {
  hash_entry const *buckets;
  hash_entry restrict *cursor;
  int num_buckets_used = 0;
  int num_entries = 0;
  int i;
  for (i=0;i<table->num_buckets;i++) {
    if (buckets[i]->car) {
      cursor = buckets[i];
      /* Count bucket head.  */
      num_buckets_used++;
      num_entries++;
      /* Count bucket overflow.  */
      while (cursor = cursor->cdr, cursor){
        num_entries++;
      }
    }
  }
  if (num_buckets_used == table->num_buckets_used &&
      num_entries == table->num_entries){
    return 1;
  }
  return 0;
}

void hash_print_statistics (const hash_table *table, FILE *stream) {
  int num_entries = hash_get_num_entries (table);
  int num_buckets = hash_get_num_buckets (table);
  int num_buckets_used = hash_get_num_buckets_used (table);
  int max_bucket_length = hash_get_max_bucket_length (table);
  fprintf (stream, "# entries:         %lu\n", (unsigned long int) num_entries);
  fprintf (stream, "# buckets:         %lu\n", (unsigned long int) num_buckets);
  fprintf (stream, "# buckets used:    %lu (%.2f%%)\n",
           (unsigned long int) num_buckets_used,
           (100.0 * num_buckets_used) / num_buckets);
  fprintf (stream, "max bucket length: %lu\n",
           (unsigned long int) max_bucket_length);
}
/* Hash KEY and return a pointer to the selected bucket.
 * or NULL if bucket doesn't exist*/
static hash_entry* safe_hasher (const hash_table *table, const void *key){
  size_t n = table->hasher (key, table->num_buckets);
  if (n >= table->num_buckets){
    return NULL;
  }
  return table->buckets + n;
}
data hash_lookup (const hash_table *table, data entry){
  hash_entry const *bucket = safe_hasher (table, entry);
  hash_entry const *cursor;
  if (!bucket || !bucket->car){
    return _nil;
  }
  for (cursor = bucket; cursor; cursor = cursor->cdr){
    if (entry == cursor->car || table->comparator (entry, cursor->car)){
      return cursor->car;
    }
  }
  return NULL;
}

/* Walking.  */
/* The functions in this page traverse the hash table and process the
   contained entries.  For the traversal to work properly, the hash table
   should not be resized nor modified while any particular entry is being
   processed.  In particular, entries should not be added, and an entry
   may be removed only if there is no shrink threshold and the entry being
   removed has already been passed to hash_get_next.  */
/* Return the first data in the table, or NULL if the table is empty.  */
sexp hash_get_first (const hash_table *table){
  if (table->num_entries == 0){
    return NULL;
  }
  int i;
  for(i=0;i<table->num_buckets;i++){
    if(table->buckets[i]->car){
      return (sexp){.tag=table->value_type,.val=table->buckets[i]->car};
    }
  }
  return error_sexp("hash table is empty");
}

/* Return the user data for the entry following ENTRY, where ENTRY has been
   returned by a previous call to either 'hash_get_first' or 'hash_get_next'.
   Return NULL if there are no more entries.  */
sexp hash_get_next (const hash_table *table, data entry){
  hash_entry const *bucket = safe_hasher (table, entry);
  hash_entry const *cursor;
  /* Find next entry in the same bucket.  */
  cursor = bucket;
  do {
    if (cursor->car == entry && cursor->cdr){
      return cursor->cdr->car;
    }
    cursor = cursor->cdr;
  } while (cursor != NULL);
  /* Find first entry in any subsequent bucket.  */
  while (++bucket < table->bucket_limit){
    if (bucket->car){
      return bucket->car;
    }
  }
  /* None found.  */
  return NULL;
}
/* Fill BUFFER with pointers to active user entries in the hash table, then
   return the number of pointers copied.  Do not copy more than BUFFER_SIZE
   pointers.  */
long
hash_get_entries (const hash_table *table, void **buffer,long buffer_size){
  int counter = 0;
  hash_entry restrict *bucket;
  hash_entry restrict *cursor;
  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++){
    if (bucket->car){
      for (cursor = bucket; cursor; cursor = cursor->cdr){
        if (counter >= buffer_size){
          return counter;
        }
        buffer[counter++] = cursor->car;
      }
    }
  }
  return counter;
}
long hash_do_for_each (const hash_table *table, Hash_processor processor,
                       void *processor_data){
  size_t counter = 0;
  hash_entry const *bucket;
  hash_entry const *cursor;
  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++){
      if (bucket->car){
          for (cursor = bucket; cursor; cursor = cursor->cdr){
            if (! processor (cursor->car, processor_data)){
                return counter;
            }
            counter++;
          }
      }
  }
  return counter;
}
/* Allocation and clean-up.  */
/* Return a hash index for a NUL-terminated STRING between 0 and NUM_BUCKETS-1.
   This is a convenience routine for constructing other hashing functions.  */
#if USE_DIFF_HASH
/* About hashings, Paul Eggert writes to me (FP), on 1994-01-01: "Please see
   B. J. McKenzie, R. Harries & T. Bell, Selecting a hashing algorithm,
   Software--practice & experience 20, 2 (Feb 1990), 209-224.  Good hash
   algorithms tend to be domain-specific, so what's good for [diffutils'] io.c
   may not be good for your application."  */
size_t diff_hash_string (const char *string, size_t num_buckets){
# define HASH_ONE_CHAR(Value, Byte)             \
  ((Byte) + rotl_sz (Value, 7))
  size_t value = 0;
  unsigned char ch;
  for (; (ch = *string); string++){
    value = HASH_ONE_CHAR (value, ch);
  }
  return value % num_buckets;
# undef HASH_ONE_CHAR
}
#else /* not USE_DIFF_HASH */
/* This one comes from 'recode', and performs a bit better than the above as
   per a few experiments.  It is inspired from a hashing routine found in the
   very old Cyber 'snoop', itself written in typical Greg Mansfield style.
   (By the way, what happened to this excellent man?  Is he still alive?)  */
int recode_hash_string (const char *string, size_t num_buckets){
  size_t value = 0;
  unsigned char ch;
  for (; (ch = *string); string++){
    value = (value * 31 + ch) % num_buckets;
  }
  return value;
}
#endif /* not USE_DIFF_HASH */

/* Return 1 if CANDIDATE is a prime number.  CANDIDATE should be an odd
   number at least equal to 11.  */
static long __attribute__((const)) is_prime (long candidate){
  long divisor = 3;
  long square = divisor * divisor;
  while (square < candidate && (candidate % divisor)){
    divisor++;
    square += 4 * divisor;
    divisor++;
  }
  return (candidate % divisor ? 1 : 0);
}

/* Round a given CANDIDATE number up to the nearest prime, and return that
   prime.  Primes lower than 10 are merely skipped.  */

static long __attribute__((const))
next_prime (long candidate) {
  if (candidate < 10){
    candidate = 10;
  }
  /* Make it definitely odd.  */
  candidate |= 1;
  while (SIZE_MAX != candidate && !is_prime (candidate)){
    candidate += 2;
  }
  return candidate;
}

void hash_reset_tuning (Hash_tuning *tuning){
  *tuning = default_tuning;
}

/*If the hash function is explictly NULL use the literal value
 *of the value to hash (since it's a union just use the long value
 *of the union)*/
static long raw_hasher (data data, long n){
  /* When hashing unique pointers, it is often the case that they were
     generated by malloc and thus have the property that the low-order
     bits are 0.  As this tends to give poorer performance with small
     tables, we rotate the pointer value before performing division,
     in an attempt to improve hash quality.  */
  long val = rotr_sz (data.int64, 3);
  return val % n;
}
/* If the user passes a NULL comparator, we use pointer comparison.  */
static int raw_comparator (data a, data b){
  return a.int64 == b.int64;
}
/* For the given hash TABLE, check the user supplied tuning structure for
   reasonable values, and return 1 if there is no gross error with it.
   Otherwise, definitively reset the TUNING field to some acceptable default
   in the hash table (that is, the user loses the right of further modifying
   tuning arguments), and return 0.  */
static int check_tuning (hash_table *table){
  const Hash_tuning *tuning = table->tuning;
  float epsilon;
  if (tuning == &default_tuning){
    return 1;
  }
  /* Be a bit stricter than mathematics would require, so that
     rounding errors in size calculations do not cause allocations to
     fail to grow or shrink as they should.  The smallest allocation
     is 11 (due to next_prime's algorithm), so an epsilon of 0.1
     should be good enough.  */
  epsilon = 0.1f;
  if (epsilon < tuning->growth_threshold &&
      tuning->growth_threshold < 1 - epsilon &&
      1 + epsilon < tuning->growth_factor &&
      0 <= tuning->shrink_threshold &&
      tuning->shrink_threshold + epsilon < tuning->shrink_factor &&
      tuning->shrink_factor <= 1 &&
      tuning->shrink_threshold + epsilon < tuning->growth_threshold){
    return 1;
  } else {
    table->tuning = &default_tuning;
    return 0;
  }
}
/* Compute the size of the bucket array for the given CANDIDATE and
   TUNING, or return 0 if there is no possible way to allocate that
   many entries.  */
static long __attribute__((pure))
compute_bucket_size (long candidate, const Hash_tuning *tuning){
  if (!tuning->is_num_buckets){
    float new_candidate = candidate / tuning->growth_threshold;
    if (SIZE_MAX <= new_candidate){
      return 0;
    }
    candidate = new_candidate;
  }
  candidate = next_prime (candidate);
  //?? not really sure of the point of this function
  if (candidate, sizeof (hash_entry *))>INT_MAX){
    return 0;
  }
  return candidate;
}
hash_table* make_hash_table (long candidate, const Hash_tuning *tuning,
                             Hash_Function hash_fn, Hash_Compare compare_fn,
                             Hash_Free free_fn,int is_weak_hash){
  hash_table *table;
  if (hasher == NULL){
    hasher = raw_hasher;
  }
  if (comparator == NULL){
    comparator = raw_comparator;
  }
  Hash_Malloc allocate_fn
  if(is_weak_hash){
    allocate_fn=xmalloc_atomic;
  } else {
    allocate_fn=xmalloc;
  }
  table = allocate_fn(sizeof(hash_table));
  table->allocate_fn=allocate_fn;
  if (table == NULL){
    return NULL;
  }
  if (!tuning) {
    tuning = &default_tuning;
  }
  table->tuning = tuning;
  if (!check_tuning (table)){
    /* Fail if the tuning options are invalid.  This is the only occasion
       when the user gets some feedback about it.  Once the table is created,
       if the user provides invalid tuning options, we silently revert to
       using the defaults, and ignore further request to change the tuning
       options.  */
    goto fail;
  }
  table->num_buckets = compute_bucket_size (candidate, tuning);
  if (!table->num_buckets){
    goto fail;
  }
  //allocate the initial buckets
  table->buckets = table->allocate_fn
    (table->num_buckets*sizeof(hash_entry));
  if (table->buckets == NULL){
    goto fail;
  }
  //set pointer to last bucket
  table->bucket_limit = table->bucket + table->num_buckets;
  //initalize defaults
  table->num_buckets_used = 0;
  table->num_entries = 0;
  table->hash_fn = hash_fn;
  table->compare_fn = compare_fn;
  table->free_fn = free_fn;
  table->free_entry_list = NULL;
  return table;
 fail:
  xfree (table);
  return NULL;
}
void hash_clear (hash_table *table){
  hash_entry *buckets;
  //if this is a weak hash we can just do memset and let gc clean up
  if(table->is_weak_hash){
    memset(table->buckets,'\0',table->num_buckets*sizeof(hash_entry));
  }
  //else we need to explictly free stuff so that we don't have
  //a bunch of unused referances
  int i;
  for (i=0;i<table->num_buckets;i++){
    if (buckets[i]->car){
      hash_entry *cursor;
      hash_entry *next;
      /* Free the bucket overflow.  */
      for (cursor = buckets[i]->cdr; cursor; cursor = next){
        if(table->free_fn){
          table->free->fn(bucket->car);
        }
        cursor->car = NULL;//if this is the last reference to car
                           //gc will clean it up
        next = cursor->cdr;
      }
      /* Free the bucket head.  */
      if (table->free_fn){
        table->free_fn (bucket->car);
      }
      buckets[i]->car = NULL;
      buckets[i]->cdr = NULL;
    }
  }
  table->num_buckets_used = 0;
  table->num_entries = 0;
}
void hash_free (hash_table *table){
  if(table->is_weak_hash){
    xfree(table->buckets);
    xfree(table);
    return;
  }
  hash_entry *bucket;
  hash_entry *cursor;
  hash_entry *next;
  /* Call the user free_fn function.(if it exists and the table isn't empty */
  if (table->free_fn && table->num_entries){
    for (bucket = table->bucket; bucket < table->bucket_limit; bucket++){
      if (bucket->car){
        for (cursor = bucket; cursor; cursor = cursor->cdr){
          table->free_fn (cursor->car);
        }
      }
    }
  }
  /* Free all bucket overflowed entries.  */
  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++){
    for (cursor = bucket->cdr; cursor; cursor = next){
      next = cursor->cdr;
      xfree(cursor);
    }
  }
  /* Free the remainder of the hash table structure.  */
  xfree (table->buckets);
  xfree (table);
}

/* Insertion and deletion.  */
static inline hash_entry *allocate_entry (hash_table *table){
  return (hash_entry*)table->allocate_fn(sizeof(hash_entry));
}
/* Free a hash entry which was part of some bucket overflow,
   saving it for later recycling.  */
static void free_entry (hash_table *table, hash_entry *entry){
  //if this isn't the first entry relink next/prev entries
  if(entry->prev){
    if(entry-cdr){
      entry->cdr->prev=entry->prev;
      entry->prev->cdr=entry->cdr;
    } else {
      entry->prev->cdr=NULL;
    }
  }
  entry->car = NULL;
}

/* This private function is used to help with insertion and deletion.  When
   ENTRY matches an entry in the table, return a pointer to the corresponding
   user data and set *BUCKET_HEAD to the head of the selected bucket.
   Otherwise, return NULL.  When DELETE is 1 and ENTRY matches an entry in
   the table, unlink the matching entry.  */

static void* hash_find_entry (hash_table *table, data,hash_entry **bucket_head,
                              int delete){
  hash_entry *bucket = safe_hasher (table, entry);
  hash_entry *cursor;
  *bucket_head = bucket;
  /* Test for empty bucket.  */
  if (bucket->car == NULL){
    return NULL;
  }
  /* See if the entry is the first in the bucket.  */
  if (entry == bucket->car || table->comparator (entry, bucket->car)){
    void *data = bucket->car;
    if (delete){
      if (bucket->cdr){
        hash_entry *next = bucket->cdr;
        /* Bump the first overflow entry into the bucket head, then save
           the previous first overflow entry for later recycling.  */
        *bucket = *next;
        free_entry (table, next);
      }
      else {
        bucket->car = NULL;
      }
    }
    return data;
  }
  /* Scan the bucket overflow.  */
  for (cursor = bucket; cursor->cdr; cursor = cursor->cdr){
    if (entry == cursor->cdr->car
        || table->comparator (entry, cursor->cdr->car)){
      void *data = cursor->cdr->car;
      if (delete){
        hash_entry *next = cursor->cdr;
        /* Unlink the entry to delete, then save the freed entry for later
           recycling.  */
        cursor->cdr = next->cdr;
        free_entry (table, next);
      }
      return data;
    }
  }
  /* No entry found.  */
  return NULL;
}

/* Internal helper, to move entries from SRC to DST.  Both tables must
   share the same free entry list.  If SAFE, only move overflow
   entries, saving bucket heads for later, so that no allocations will
   occur.  Return 0 if the free entry list is exhausted and an
   allocation fails.  */

static int
transfer_entries (hash_table *dst, hash_table *src, int safe)
{
  hash_entry *bucket;
  hash_entry *cursor;
  hash_entry *next;
  for (bucket = src->bucket; bucket < src->bucket_limit; bucket++)
    if (bucket->car)
      {
        void *data;
        hash_entry *new_bucket;

        /* Within each bucket, transfer overflow entries first and
           then the bucket head, to minimize memory pressure.  After
           all, the only time we might allocate is when moving the
           bucket head, but moving overflow entries first may create
           free entries that can be recycled by the time we finally
           get to the bucket head.  */
        for (cursor = bucket->cdr; cursor; cursor = next)
          {
            data = cursor->car;
            new_bucket = safe_hasher (dst, data);

            next = cursor->cdr;

            if (new_bucket->car)
              {
                /* Merely relink an existing entry, when moving from a
                   bucket overflow into a bucket overflow.  */
                cursor->cdr = new_bucket->cdr;
                new_bucket->cdr = cursor;
              }
            else
              {
                /* Free an existing entry, when moving from a bucket
                   overflow into a bucket header.  */
                new_bucket->car = data;
                dst->num_buckets_used++;
                free_entry (dst, cursor);
              }
          }
        /* Now move the bucket head.  Be sure that if we fail due to
           allocation failure that the src table is in a consistent
           state.  */
        data = bucket->car;
        bucket->cdr = NULL;
        if (safe)
          continue;
        new_bucket = safe_hasher (dst, data);

        if (new_bucket->car)
          {
            /* Allocate or recycle an entry, when moving from a bucket
               header into a bucket overflow.  */
            hash_entry *new_entry = allocate_entry (dst);

            if (new_entry == NULL)
              return 0;

            new_entry->car = data;
            new_entry->cdr = new_bucket->cdr;
            new_bucket->cdr = new_entry;
          }
        else
          {
            /* Move from one bucket header to another.  */
            new_bucket->car = data;
            dst->num_buckets_used++;
          }
        bucket->car = NULL;
        src->num_buckets_used--;
      }
  return 1;
}

/* For an already existing hash table, change the number of buckets through
   specifying CANDIDATE.  The contents of the hash table are preserved.  The
   new number of buckets is automatically selected so as to _guarantee_ that
   the table may receive at least CANDIDATE different user entries, including
   those already in the table, before any other growth of the hash table size
   occurs.  If TUNING->IS_N_BUCKETS is 1, then CANDIDATE specifies the
   exact number of buckets desired.  Return 1 iff the rehash succeeded.  */

int hash_rehash (hash_table *table, long candidate) {
  hash_table storage;
  hash_table *new_table;
  size_t new_size = compute_bucket_size (candidate, table->tuning);
  if (!new_size){
    return 0;
  }
  //if the size doesn't change don't do anything
  if (new_size == table->num_buckets){
    return 1;
  }
  new_table = &storage;
  new_table->bucket = table->allocate_fn(new_size, sizeof *new_table->bucket);
  if (new_table->bucket == NULL){
    return 0;
  }
  new_table->num_buckets = new_size;
  new_table->bucket_limit = new_table->bucket + new_size;
  new_table->num_buckets_used = 0;
  new_table->num_entries = 0;
  new_table->tuning = table->tuning;
  new_table->hasher = table->hasher;
  new_table->comparator = table->comparator;
  new_table->free_fn = table->free_fn;
  /* In order for the transfer to successfully complete, we need
     additional overflow entries when distinct buckets in the old
     table collide into a common bucket in the new table.  The worst
     case possible is a hasher that gives a good spread with the old
     size, but returns a constant with the new size; if we were to
     guarantee table->num_buckets_used-1 free entries in advance, then
     the transfer would be guaranteed to not allocate memory.
     However, for large tables, a guarantee of no further allocation
     introduces a lot of extra memory pressure, all for an unlikely
     corner case (most rehashes reduce, rather than increase, the
     number of overflow entries needed).  So, we instead ensure that
     the transfer process can be reversed if we hit a memory
     allocation failure mid-transfer.  */
  if (transfer_entries (new_table, table, 0)){
    /* Entries transferred successfully; tie up the loose ends.  */
    xfree (table->bucket);
    table->bucket = new_table->bucket;
    table->bucket_limit = new_table->bucket_limit;
    table->num_buckets = new_table->num_buckets;
    table->num_buckets_used = new_table->num_buckets_used;
    /* table->num_entries and table->entry_stack already hold their value.  */
    return 1;
  }
  /* We've allocated new_table->bucket (and possibly some entries),
     exhausted the free list, and moved some but not all entries into
     new_table.  We must undo the partial move before returning
     failure.  The only way to get into this situation is if new_table
     uses fewer buckets than the old table, so we will reclaim some
     free entries as overflows in the new table are put back into
     distinct buckets in the old table.

     There are some pathological cases where a single pass through the
     table requires more intermediate overflow entries than using two
     passes.  Two passes give worse cache performance and takes
     longer, but at this point, we're already out of memory, so slow
     and safe is better than failure.  */
  if (!(transfer_entries (table, new_table, 1)
        && transfer_entries (table, new_table, 0))){
    return 0;
  }
  /* table->num_entries already holds its value.  */
  xfree (new_table->bucket);
  return 0;
}
/* Insert ENTRY into hash TABLE if there is not already a matching entry.
   Return -1 upon memory allocation failure.
   Return 1 if insertion succeeded.
   Return 0 if there is already a matching entry in the table,
   and in that case, if MATCHED_ENT is non-NULL, set *MATCHED_ENT
   to that entry.

   This interface is easier to use than hash_insert when you must
   distinguish between the latter two cases.  More importantly,
   hash_insert is unusable for some types of ENTRY values.  When using
   hash_insert, the only way to distinguish those cases is to compare
   the return value and ENTRY.  That works only when you can have two
   different ENTRY values that point to data that compares "equal".  Thus,
   when the ENTRY value is a simple scalar, you must use
   hash_insert_if_absent.  ENTRY must not be NULL.  */
int hash_insert_if_absent (hash_table *table, data entry,
                           void const **matched_ent){
  void *data;
  hash_entry *bucket;
  /* The caller cannot insert a NULL entry, since hash_lookup returns NULL
     to indicate "not found", and hash_find_entry uses "bucket->car == NULL"
     to indicate an empty bucket.  */
  if (!entry){
    return -1;
  }
  /* If there's a matching entry already in the table, return that.  */
  if ((data = hash_find_entry (table, entry, &bucket, 0)) != NULL){
    if (matched_ent){
      *matched_ent = data;
    }
    return 0;
  }
  /* If the growth threshold of the buckets in use has been reached, increase
     the table size and rehash.  There's no point in checking the number of
     entries:  if the hashing function is ill-conditioned, rehashing is not
     likely to improve it.  */
  if (table->num_buckets_used >
      table->tuning->growth_threshold * table->num_buckets){
    /* Check more fully, before starting real work.  If tuning arguments
       became invalid, the second check will rely on proper defaults.  */
    check_tuning (table);
    if (table->num_buckets_used >
        table->tuning->growth_threshold * table->num_buckets){
      const Hash_tuning *tuning = table->tuning;
      float candidate =
        (tuning->is_n_buckets
         ? (table->num_buckets * tuning->growth_factor)
         : (table->num_buckets * tuning->growth_factor
            * tuning->growth_threshold));
      if (SIZE_MAX <= candidate){
        return -1;
      }
      /* If the rehash fails, arrange to return NULL.  */
      if (!hash_rehash (table, candidate)){
        return -1;
      }
      /* Update the bucket we are interested in.  */
      if (hash_find_entry (table, entry, &bucket, 0) != NULL){
        return -1;
      }
    }
  }
  /* ENTRY is not matched, it should be inserted.  */
  if (bucket->car){
    hash_entry *new_entry = allocate_entry (table);
    if (new_entry == NULL){
      return -1;
    }
    /* Add ENTRY in the overflow of the bucket.  */
    *new_entry=(hash_entry){.car=entry,.cdr=bucket->cdr,.prev=0};
    bucket->cdr->prev = new_entry;
    bucket->cdr=new_entry
    table->num_entries++;
    return 1;
  }
  /* Add ENTRY right in the bucket head.  */
  bucket->car = entry;
  table->num_entries++;
  table->num_buckets_used++;
  return 1;
}
/* If ENTRY matches an entry already in the hash table, return the pointer
   to the entry from the table.  Otherwise, insert ENTRY and return ENTRY.
   Return NULL if the storage required for insertion cannot be allocated.
   This implementation does not support duplicate entries or insertion of
   NULL.  */

data hash_insert (hash_table *table, data entry){
  void const *matched_ent;
  int err = hash_insert_if_absent (table, entry, &matched_ent);
  return (err == -1
          ? NULL
          : (data)(err == 0 ? matched_ent : entry));
}

/* If VALUE is already in the table, remove it and return the just-deleted
   data (the user may want to deallocate its storage).  If VALUE is not in the
   table, don't modify the table and return NULL.  */

data hash_delete (hash_table *table, data value){
  data hash_entry;
  hash_entry *bucket;
  hash_entry = hash_find_entry (table, value, &bucket, 1);
  if (!hash_entry){
    return NULL;
  }
  table->num_entries--;
  if (!bucket->car){
      table->num_buckets_used--;
      /* If the shrink threshold of the buckets in use has been reached,
         rehash into a smaller table.  */
      if (table->num_buckets_used <
          table->tuning->shrink_threshold * table->num_buckets){
        /* Check more fully, before starting real work.  If tuning arguments
           became invalid, the second check will rely on proper defaults.  */
        check_tuning (table);
        if (table->num_buckets_used <
            table->tuning->shrink_threshold * table->num_buckets){
          const Hash_tuning *tuning = table->tuning;
          long candidate =
            (tuning->is_n_buckets
             ? table->num_buckets * tuning->shrink_factor
             : (table->num_buckets * tuning->shrink_factor
                * tuning->growth_threshold));
        }
      }
  }
  return data;
}
/* The MurmurHash exploits some CPU's (x86,x86_64) tolerance for unaligned reads.
 * For other types of CPU's (e.g. Sparc) an unaligned read causes a bus error.
 * MurmurHash uses the faster approach only on CPU's where we know it's safe.
 *
 * Note the preprocessor built-in defines can be emitted using:
 *
 *   gcc -m64 -dM -E - < /dev/null                  (on gcc)
 *   cc -## a.c (where a.c is a simple test file)   (Sun Studio)
 */
#if (defined(__i386__) || defined(__x86_64__)  || defined(_M_IX86))
#define MUR_GETBLOCK(p,i) p[i]
#else /* non intel */
#define MUR_PLUS0_ALIGNED(p) (((unsigned long)p & 0x3) == 0)
#define MUR_PLUS1_ALIGNED(p) (((unsigned long)p & 0x3) == 1)
#define MUR_PLUS2_ALIGNED(p) (((unsigned long)p & 0x3) == 2)
#define MUR_PLUS3_ALIGNED(p) (((unsigned long)p & 0x3) == 3)
#define WP(p) ((uint32_t*)((unsigned long)(p) & ~3UL))
#if (defined(__BIG_ENDIAN__) || defined(SPARC) || defined(__ppc__) || defined(__ppc64__))
#define MUR_THREE_ONE(p) ((((*WP(p))&0x00ffffff) << 8) | (((*(WP(p)+1))&0xff000000) >> 24))
#define MUR_TWO_TWO(p)   ((((*WP(p))&0x0000ffff) <<16) | (((*(WP(p)+1))&0xffff0000) >> 16))
#define MUR_ONE_THREE(p) ((((*WP(p))&0x000000ff) <<24) | (((*(WP(p)+1))&0xffffff00) >>  8))
#else /* assume little endian non-intel */
#define MUR_THREE_ONE(p) ((((*WP(p))&0xffffff00) >> 8) | (((*(WP(p)+1))&0x000000ff) << 24))
#define MUR_TWO_TWO(p)   ((((*WP(p))&0xffff0000) >>16) | (((*(WP(p)+1))&0x0000ffff) << 16))
#define MUR_ONE_THREE(p) ((((*WP(p))&0xff000000) >>24) | (((*(WP(p)+1))&0x00ffffff) <<  8))
#endif
#define MUR_GETBLOCK(p,i) (MUR_PLUS0_ALIGNED(p) ? ((p)[i]) :           \
                            (MUR_PLUS1_ALIGNED(p) ? MUR_THREE_ONE(p) : \
                             (MUR_PLUS2_ALIGNED(p) ? MUR_TWO_TWO(p) :  \
                                                      MUR_ONE_THREE(p))))
#endif
#define MUR_ROTL32(x,r) (((x) << (r)) | ((x) >> (32 - (r))))
#define MUR_FMIX(_h) \
do {                 \
  _h ^= _h >> 16;    \
  _h *= 0x85ebca6b;  \
  _h ^= _h >> 13;    \
  _h *= 0xc2b2ae35l; \
  _h ^= _h >> 16;    \
} while(0)

//this is just a hash function, it's up to the table to translate
//the hash value into a bucket number
uint32_t murmur_hash(data key,int keylen){
  const uint8_t *_mur_data = (const uint8_t*)(key);
  const int _mur_nblocks = (keylen) / 4;
  uint32_t _mur_h1 = 0xf88D5353;
  uint32_t _mur_c1 = 0xcc9e2d51;
  uint32_t _mur_c2 = 0x1b873593;
  uint32_t _mur_k1 = 0;
  const uint8_t *_mur_tail;
  const uint32_t *_mur_blocks = (const uint32_t*)(_mur_data+_mur_nblocks*4);
  int _mur_i;
  for(_mur_i = -_mur_nblocks; _mur_i; _mur_i++) {
    _mur_k1 = MUR_GETBLOCK(_mur_blocks,_mur_i);
    _mur_k1 *= _mur_c1;
    _mur_k1 = MUR_ROTL32(_mur_k1,15);
    _mur_k1 *= _mur_c2;
    _mur_h1 ^= _mur_k1;
    _mur_h1 = MUR_ROTL32(_mur_h1,13);
    _mur_h1 = _mur_h1*5+0xe6546b64;
  }
  _mur_tail = (const uint8_t*)(_mur_data + _mur_nblocks*4);
  _mur_k1=0;
  switch((keylen) & 3) {
    case 3: _mur_k1 ^= _mur_tail[2] << 16;
    case 2: _mur_k1 ^= _mur_tail[1] << 8;
    case 1: _mur_k1 ^= _mur_tail[0];
    _mur_k1 *= _mur_c1;
    _mur_k1 = MUR_ROTL32(_mur_k1,15);
    _mur_k1 *= _mur_c2;
    _mur_h1 ^= _mur_k1;
  }
  _mur_h1 ^= (keylen);
  MUR_FMIX(_mur_h1);
  return _mur_h1;
}
/*given char* data, int data_len;
  hash=offset_basis;int i;
  for(i=0;i<data_len;i++){
  hash = hash ^ data[i];
  hash = hash * fnv_prime
  return hash;*/
#define offset_basis 14695981039346656037UL
#define fnv_prime 1099511628211UL
long fnv_hash(data key,int keylen){
  char *raw_data=(char*)(key);
  int i;
  long hash=offset_basis;
  for(i=0; i < keylen; i++){
      hashv = (hashv * fnv_prime) ^ raw_data[i];
  }
  return hash;
}
