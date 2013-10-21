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

//hash tables use a linked list rather than cons cells internally
struct hash_entry{
  data car;
  hash_entry *cdr;
};
/* old hash entry
   struct hash_entry
   {
   void *data;
   struct hash_entry *next;
   };
*/
struct hash_table {
  /* The array of buckets starts at BUCKET and extends to BUCKET_LIMIT-1,
     for a possibility of NUM_BUCKETS.  Among those, NUM_BUCKETS_USED buckets
     are not empty, there are N_ENTRIES active entries in the table.  */
  _tag value_type;
  hash_entry *buckets;//points to first bucket in array of buckets
  hash_entry *bucket_limit;//""   last  ""
  size_t num_buckets;
  size_t num_buckets_used;
  size_t n_entries;
  /* Tuning arguments, kept in a physically separate structure.  */
  const Hash_tuning *tuning;
  /* Three functions are given to 'hash_initialize', see the documentation
     block for this function.  In a word, HASHER randomizes a user entry
     into a number up from 0 up to some maximum minus 1; COMPARATOR returns
     true if two user entries compare equally; and DATA_FREER is the cleanup
     function for a user entry.  */
  Hash_Function hash_fn;
  Hash_Compare compare_fn;

  /* A linked list of freed struct hash_entry structs.  */
  struct hash_entry *free_entry_list;
};

/* A hash table contains many internal entries, each holding a pointer to
   some user-provided data (also called a user entry).  An entry indistinctly
   refers to both the internal entry and its associated user entry.  A user
   entry contents may be hashed by a randomization function (the hashing
   function, or just "hasher" for short) into a number (or "slot") between 0
   and the current table size.  At each slot position in the hash table,
   starts a linked chain of entries for which the user data all hash to this
   slot.  A bucket is the collection of all entries hashing to the same slot.

   A good "hasher" function will distribute entries rather evenly in buckets.
   In the ideal case, the length of each bucket is roughly the number of
   entries divided by the table size.  Finding the slot for a data is usually
   done in constant time by the "hasher", and the later finding of a precise
   entry is linear in time with the size of the bucket.  Consequently, a
   larger hash table size (that is, a larger number of buckets) is prone to
   yielding shorter chains, *given* the "hasher" function behaves properly.

   Long buckets slow down the lookup algorithm.  One might use big hash table
   sizes in hope to reduce the average length of buckets, but this might
   become inordinate, as unused slots in the hash table take some space.  The
   best bet is to make sure you are using a good "hasher" function (beware
   that those are not that easy to write! :-), and to use a table size
   larger than the actual number of entries.  */

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
static const Hash_tuning default_tuning =
  {
    DEFAULT_SHRINK_THRESHOLD,
    DEFAULT_SHRINK_FACTOR,
    DEFAULT_GROWTH_THRESHOLD,
    DEFAULT_GROWTH_FACTOR,
    false
  };

/* Information and lookup.  */

/* The following few functions provide information about the overall hash
   table organization: the number of entries, number of buckets and maximum
   length of buckets.  */

/* Return the number of buckets in the hash table.  The table size, the total
   number of buckets (used plus unused), or the maximum number of slots, are
   the same quantity.  */

size_t hash_get_num_buckets (const hash_table *table){
  return table->num_buckets;
}

/* Return the number of slots in use (non-empty buckets).  */

size_t hash_get_num_buckets_used (const hash_table *table){
  return table->num_buckets_used;
}

/* Return the number of active entries.  */

size_t hash_get_n_entries (const hash_table *table)
{
  return table->n_entries;
}

/* Return the length of the longest chain (bucket).  */

size_t hash_get_max_bucket_length (const hash_table *table) {
  hash_entry const *bucket;
  size_t max_bucket_length = 0;
  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++){
    if (bucket->data) {
      hash_entry const *cursor = bucket;
      size_t bucket_length = 1;
      while (cursor = cursor->next, cursor){
        bucket_length++;
      }
      if (bucket_length > max_bucket_length){
        max_bucket_length = bucket_length;
      }
    }
  }
  return max_bucket_length;
}

/* Do a mild validation of a hash table, by traversing it and checking two
   statistics.  */

bool hash_table_ok (const hash_table *table) {
  struct hash_entry const *bucket;
  size_t num_buckets_used = 0;
  size_t n_entries = 0;
  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++){
      if (bucket->data) {
          struct hash_entry const *cursor = bucket;
          /* Count bucket head.  */
          num_buckets_used++;
          n_entries++;
          /* Count bucket overflow.  */
          while (cursor = cursor->next, cursor){
            n_entries++;
          }
      }
  }

  if (num_buckets_used == table->num_buckets_used && n_entries == table->n_entries){
    return true;
  }
  return false;
}

void hash_print_statistics (const hash_table *table, FILE *stream) {
  size_t n_entries = hash_get_n_entries (table);
  size_t num_buckets = hash_get_num_buckets (table);
  size_t num_buckets_used = hash_get_num_buckets_used (table);
  size_t max_bucket_length = hash_get_max_bucket_length (table);

  fprintf (stream, "# entries:         %lu\n", (unsigned long int) n_entries);
  fprintf (stream, "# buckets:         %lu\n", (unsigned long int) num_buckets);
  fprintf (stream, "# buckets used:    %lu (%.2f%%)\n",
           (unsigned long int) num_buckets_used,
           (100.0 * num_buckets_used) / num_buckets);
  fprintf (stream, "max bucket length: %lu\n",
           (unsigned long int) max_bucket_length);
}

/* Hash KEY and return a pointer to the selected bucket.
   If TABLE->hasher misbehaves, abort.  */
static hash_entry* safe_hasher (const hash_table *table, const void *key) {
  size_t n = table->hasher (key, table->num_buckets);
  if (! (n < table->num_buckets)){
    abort ();
  }
  return table->bucket + n;
}

/* If ENTRY matches an entry already in the hash table, return the
   entry from the table.  Otherwise, return NULL.  */

void * hash_lookup (const hash_table *table, const void *entry){
  struct hash_entry const *bucket = safe_hasher (table, entry);
  struct hash_entry const *cursor;
  if (bucket->data == NULL){
    return NULL;
  }
  for (cursor = bucket; cursor; cursor = cursor->next){
    if (entry == cursor->data || table->comparator (entry, cursor->data)){
      return cursor->data;
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
  if (table->n_entries == 0){
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

void *
hash_get_next (const hash_table *table, const void *entry)
{
  struct hash_entry const *bucket = safe_hasher (table, entry);
  struct hash_entry const *cursor;

  /* Find next entry in the same bucket.  */
  cursor = bucket;
  do
    {
      if (cursor->data == entry && cursor->next)
        return cursor->next->data;
      cursor = cursor->next;
    }
  while (cursor != NULL);

  /* Find first entry in any subsequent bucket.  */
  while (++bucket < table->bucket_limit)
    if (bucket->data)
      return bucket->data;

  /* None found.  */
  return NULL;
}

/* Fill BUFFER with pointers to active user entries in the hash table, then
   return the number of pointers copied.  Do not copy more than BUFFER_SIZE
   pointers.  */

size_t
hash_get_entries (const hash_table *table, void **buffer,
                  size_t buffer_size)
{
  size_t counter = 0;
  struct hash_entry const *bucket;
  struct hash_entry const *cursor;

  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++)
    {
      if (bucket->data)
        {
          for (cursor = bucket; cursor; cursor = cursor->next)
            {
              if (counter >= buffer_size)
                return counter;
              buffer[counter++] = cursor->data;
            }
        }
    }

  return counter;
}

/* Call a PROCESSOR function for each entry of a hash table, and return the
   number of entries for which the processor function returned success.  A
   pointer to some PROCESSOR_DATA which will be made available to each call to
   the processor function.  The PROCESSOR accepts two arguments: the first is
   the user entry being walked into, the second is the value of PROCESSOR_DATA
   as received.  The walking continue for as long as the PROCESSOR function
   returns nonzero.  When it returns zero, the walking is interrupted.  */

size_t
hash_do_for_each (const hash_table *table, Hash_processor processor,
                  void *processor_data)
{
  size_t counter = 0;
  struct hash_entry const *bucket;
  struct hash_entry const *cursor;

  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++)
    {
      if (bucket->data)
        {
          for (cursor = bucket; cursor; cursor = cursor->next)
            {
              if (! processor (cursor->data, processor_data))
                return counter;
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

size_t
hash_string (const char *string, size_t num_buckets)
{
# define HASH_ONE_CHAR(Value, Byte)             \
  ((Byte) + rotl_sz (Value, 7))

  size_t value = 0;
  unsigned char ch;

  for (; (ch = *string); string++)
    value = HASH_ONE_CHAR (value, ch);
  return value % num_buckets;

# undef HASH_ONE_CHAR
}

#else /* not USE_DIFF_HASH */

/* This one comes from 'recode', and performs a bit better than the above as
   per a few experiments.  It is inspired from a hashing routine found in the
   very old Cyber 'snoop', itself written in typical Greg Mansfield style.
   (By the way, what happened to this excellent man?  Is he still alive?)  */

size_t
hash_string (const char *string, size_t num_buckets)
{
  size_t value = 0;
  unsigned char ch;

  for (; (ch = *string); string++)
    value = (value * 31 + ch) % num_buckets;
  return value;
}

#endif /* not USE_DIFF_HASH */

/* Return true if CANDIDATE is a prime number.  CANDIDATE should be an odd
   number at least equal to 11.  */

static bool _GL_ATTRIBUTE_CONST
is_prime (size_t candidate)
{
  size_t divisor = 3;
  size_t square = divisor * divisor;

  while (square < candidate && (candidate % divisor))
    {
      divisor++;
      square += 4 * divisor;
      divisor++;
    }

  return (candidate % divisor ? true : false);
}

/* Round a given CANDIDATE number up to the nearest prime, and return that
   prime.  Primes lower than 10 are merely skipped.  */

static size_t _GL_ATTRIBUTE_CONST
next_prime (size_t candidate)
{
  /* Skip small primes.  */
  if (candidate < 10)
    candidate = 10;

  /* Make it definitely odd.  */
  candidate |= 1;

  while (SIZE_MAX != candidate && !is_prime (candidate))
    candidate += 2;

  return candidate;
}

void
hash_reset_tuning (Hash_tuning *tuning)
{
  *tuning = default_tuning;
}

/* If the user passes a NULL hasher, we hash the raw pointer.  */
static size_t
raw_hasher (const void *data, size_t n)
{
  /* When hashing unique pointers, it is often the case that they were
     generated by malloc and thus have the property that the low-order
     bits are 0.  As this tends to give poorer performance with small
     tables, we rotate the pointer value before performing division,
     in an attempt to improve hash quality.  */
  size_t val = rotr_sz ((size_t) data, 3);
  return val % n;
}

/* If the user passes a NULL comparator, we use pointer comparison.  */
static bool
raw_comparator (const void *a, const void *b)
{
  return a == b;
}


/* For the given hash TABLE, check the user supplied tuning structure for
   reasonable values, and return true if there is no gross error with it.
   Otherwise, definitively reset the TUNING field to some acceptable default
   in the hash table (that is, the user loses the right of further modifying
   tuning arguments), and return false.  */

static bool
check_tuning (hash_table *table)
{
  const Hash_tuning *tuning = table->tuning;
  float epsilon;
  if (tuning == &default_tuning)
    return true;

  /* Be a bit stricter than mathematics would require, so that
     rounding errors in size calculations do not cause allocations to
     fail to grow or shrink as they should.  The smallest allocation
     is 11 (due to next_prime's algorithm), so an epsilon of 0.1
     should be good enough.  */
  epsilon = 0.1f;

  if (epsilon < tuning->growth_threshold
      && tuning->growth_threshold < 1 - epsilon
      && 1 + epsilon < tuning->growth_factor
      && 0 <= tuning->shrink_threshold
      && tuning->shrink_threshold + epsilon < tuning->shrink_factor
      && tuning->shrink_factor <= 1
      && tuning->shrink_threshold + epsilon < tuning->growth_threshold)
    return true;

  table->tuning = &default_tuning;
  return false;
}

/* Compute the size of the bucket array for the given CANDIDATE and
   TUNING, or return 0 if there is no possible way to allocate that
   many entries.  */

static size_t _GL_ATTRIBUTE_PURE
compute_bucket_size (size_t candidate, const Hash_tuning *tuning)
{
  if (!tuning->is_n_buckets)
    {
      float new_candidate = candidate / tuning->growth_threshold;
      if (SIZE_MAX <= new_candidate)
        return 0;
      candidate = new_candidate;
    }
  candidate = next_prime (candidate);
  if (xalloc_oversized (candidate, sizeof (struct hash_entry *)))
    return 0;
  return candidate;
}

/* Allocate and return a new hash table, or NULL upon failure.  The initial
   number of buckets is automatically selected so as to _guarantee_ that you
   may insert at least CANDIDATE different user entries before any growth of
   the hash table size occurs.  So, if have a reasonably tight a-priori upper
   bound on the number of entries you intend to insert in the hash table, you
   may save some table memory and insertion time, by specifying it here.  If
   the IS_N_BUCKETS field of the TUNING structure is true, the CANDIDATE
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
   arguments pointing to user data, it then returns true for a pair of entries
   that compare equal, or false otherwise.  This function is internally called
   on entries which are already known to hash to the same bucket index,
   but which are distinct pointers.

   The user-supplied DATA_FREER function, when not NULL, may be later called
   with the user data as an argument, just before the entry containing the
   data gets freed.  This happens from within 'hash_free' or 'hash_clear'.
   You should specify this function only if you want these functions to free
   all of your 'data' data.  This is typically the case when your data is
   simply an auxiliary struct that you have malloc'd to aggregate several
   values.  */

hash_table *
hash_initialize (size_t candidate, const Hash_tuning *tuning,
                 Hash_hasher hasher, Hash_comparator comparator,
                 Hash_data_freer data_freer)
{
  hash_table *table;

  if (hasher == NULL)
    hasher = raw_hasher;
  if (comparator == NULL)
    comparator = raw_comparator;

  table = xmalloc (sizeof *table);
  if (table == NULL)
    return NULL;

  if (!tuning)
    tuning = &default_tuning;
  table->tuning = tuning;
  if (!check_tuning (table))
    {
      /* Fail if the tuning options are invalid.  This is the only occasion
         when the user gets some feedback about it.  Once the table is created,
         if the user provides invalid tuning options, we silently revert to
         using the defaults, and ignore further request to change the tuning
         options.  */
      goto fail;
    }

  table->num_buckets = compute_bucket_size (candidate, tuning);
  if (!table->num_buckets)
    goto fail;

  table->bucket = calloc (table->num_buckets, sizeof *table->bucket);
  if (table->bucket == NULL)
    goto fail;
  table->bucket_limit = table->bucket + table->num_buckets;
  table->num_buckets_used = 0;
  table->n_entries = 0;

  table->hasher = hasher;
  table->comparator = comparator;
  table->data_freer = data_freer;

  table->free_entry_list = NULL;
#if USE_OBSTACK
  obstack_init (&table->entry_stack);
#endif
  return table;

 fail:
  free (table);
  return NULL;
}

/* Make all buckets empty, placing any chained entries on the free list.
   Apply the user-specified function data_freer (if any) to the datas of any
   affected entries.  */

void
hash_clear (hash_table *table)
{
  struct hash_entry *bucket;

  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++)
    {
      if (bucket->data)
        {
          struct hash_entry *cursor;
          struct hash_entry *next;

          /* Free the bucket overflow.  */
          for (cursor = bucket->next; cursor; cursor = next)
            {
              if (table->data_freer)
                table->data_freer (cursor->data);
              cursor->data = NULL;

              next = cursor->next;
              /* Relinking is done one entry at a time, as it is to be expected
                 that overflows are either rare or short.  */
              cursor->next = table->free_entry_list;
              table->free_entry_list = cursor;
            }

          /* Free the bucket head.  */
          if (table->data_freer)
            table->data_freer (bucket->data);
          bucket->data = NULL;
          bucket->next = NULL;
        }
    }

  table->num_buckets_used = 0;
  table->n_entries = 0;
}

/* Reclaim all storage associated with a hash table.  If a data_freer
   function has been supplied by the user when the hash table was created,
   this function applies it to the data of each entry before freeing that
   entry.  */

void
hash_free (hash_table *table)
{
  struct hash_entry *bucket;
  struct hash_entry *cursor;
  struct hash_entry *next;

  /* Call the user data_freer function.  */
  if (table->data_freer && table->n_entries)
    {
      for (bucket = table->bucket; bucket < table->bucket_limit; bucket++)
        {
          if (bucket->data)
            {
              for (cursor = bucket; cursor; cursor = cursor->next)
                table->data_freer (cursor->data);
            }
        }
    }

#if USE_OBSTACK

  obstack_free (&table->entry_stack, NULL);

#else

  /* Free all bucket overflowed entries.  */
  for (bucket = table->bucket; bucket < table->bucket_limit; bucket++)
    {
      for (cursor = bucket->next; cursor; cursor = next)
        {
          next = cursor->next;
          free (cursor);
        }
    }

  /* Also reclaim the internal list of previously freed entries.  */
  for (cursor = table->free_entry_list; cursor; cursor = next)
    {
      next = cursor->next;
      free (cursor);
    }

#endif

  /* Free the remainder of the hash table structure.  */
  free (table->bucket);
  free (table);
}

/* Insertion and deletion.  */

/* Get a new hash entry for a bucket overflow, possibly by recycling a
   previously freed one.  If this is not possible, allocate a new one.  */

static struct hash_entry *
allocate_entry (hash_table *table)
{
  struct hash_entry *new;

  if (table->free_entry_list)
    {
      new = table->free_entry_list;
      table->free_entry_list = new->next;
    }
  else
    {
#if USE_OBSTACK
      new = obstack_alloc (&table->entry_stack, sizeof *new);
#else
      new = xmalloc (sizeof *new);
#endif
    }

  return new;
}

/* Free a hash entry which was part of some bucket overflow,
   saving it for later recycling.  */

static void
free_entry (hash_table *table, struct hash_entry *entry)
{
  entry->data = NULL;
  entry->next = table->free_entry_list;
  table->free_entry_list = entry;
}

/* This private function is used to help with insertion and deletion.  When
   ENTRY matches an entry in the table, return a pointer to the corresponding
   user data and set *BUCKET_HEAD to the head of the selected bucket.
   Otherwise, return NULL.  When DELETE is true and ENTRY matches an entry in
   the table, unlink the matching entry.  */

static void *
hash_find_entry (hash_table *table, const void *entry,
                 struct hash_entry **bucket_head, bool delete)
{
  struct hash_entry *bucket = safe_hasher (table, entry);
  struct hash_entry *cursor;

  *bucket_head = bucket;

  /* Test for empty bucket.  */
  if (bucket->data == NULL)
    return NULL;

  /* See if the entry is the first in the bucket.  */
  if (entry == bucket->data || table->comparator (entry, bucket->data))
    {
      void *data = bucket->data;

      if (delete)
        {
          if (bucket->next)
            {
              struct hash_entry *next = bucket->next;

              /* Bump the first overflow entry into the bucket head, then save
                 the previous first overflow entry for later recycling.  */
              *bucket = *next;
              free_entry (table, next);
            }
          else
            {
              bucket->data = NULL;
            }
        }

      return data;
    }

  /* Scan the bucket overflow.  */
  for (cursor = bucket; cursor->next; cursor = cursor->next)
    {
      if (entry == cursor->next->data
          || table->comparator (entry, cursor->next->data))
        {
          void *data = cursor->next->data;

          if (delete)
            {
              struct hash_entry *next = cursor->next;

              /* Unlink the entry to delete, then save the freed entry for later
                 recycling.  */
              cursor->next = next->next;
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
   occur.  Return false if the free entry list is exhausted and an
   allocation fails.  */

static bool
transfer_entries (hash_table *dst, hash_table *src, bool safe)
{
  struct hash_entry *bucket;
  struct hash_entry *cursor;
  struct hash_entry *next;
  for (bucket = src->bucket; bucket < src->bucket_limit; bucket++)
    if (bucket->data)
      {
        void *data;
        struct hash_entry *new_bucket;

        /* Within each bucket, transfer overflow entries first and
           then the bucket head, to minimize memory pressure.  After
           all, the only time we might allocate is when moving the
           bucket head, but moving overflow entries first may create
           free entries that can be recycled by the time we finally
           get to the bucket head.  */
        for (cursor = bucket->next; cursor; cursor = next)
          {
            data = cursor->data;
            new_bucket = safe_hasher (dst, data);

            next = cursor->next;

            if (new_bucket->data)
              {
                /* Merely relink an existing entry, when moving from a
                   bucket overflow into a bucket overflow.  */
                cursor->next = new_bucket->next;
                new_bucket->next = cursor;
              }
            else
              {
                /* Free an existing entry, when moving from a bucket
                   overflow into a bucket header.  */
                new_bucket->data = data;
                dst->num_buckets_used++;
                free_entry (dst, cursor);
              }
          }
        /* Now move the bucket head.  Be sure that if we fail due to
           allocation failure that the src table is in a consistent
           state.  */
        data = bucket->data;
        bucket->next = NULL;
        if (safe)
          continue;
        new_bucket = safe_hasher (dst, data);

        if (new_bucket->data)
          {
            /* Allocate or recycle an entry, when moving from a bucket
               header into a bucket overflow.  */
            struct hash_entry *new_entry = allocate_entry (dst);

            if (new_entry == NULL)
              return false;

            new_entry->data = data;
            new_entry->next = new_bucket->next;
            new_bucket->next = new_entry;
          }
        else
          {
            /* Move from one bucket header to another.  */
            new_bucket->data = data;
            dst->num_buckets_used++;
          }
        bucket->data = NULL;
        src->num_buckets_used--;
      }
  return true;
}

/* For an already existing hash table, change the number of buckets through
   specifying CANDIDATE.  The contents of the hash table are preserved.  The
   new number of buckets is automatically selected so as to _guarantee_ that
   the table may receive at least CANDIDATE different user entries, including
   those already in the table, before any other growth of the hash table size
   occurs.  If TUNING->IS_N_BUCKETS is true, then CANDIDATE specifies the
   exact number of buckets desired.  Return true iff the rehash succeeded.  */

bool
hash_rehash (hash_table *table, size_t candidate)
{
  hash_table storage;
  hash_table *new_table;
  size_t new_size = compute_bucket_size (candidate, table->tuning);

  if (!new_size)
    return false;
  if (new_size == table->num_buckets)
    return true;
  new_table = &storage;
  new_table->bucket = calloc (new_size, sizeof *new_table->bucket);
  if (new_table->bucket == NULL)
    return false;
  new_table->num_buckets = new_size;
  new_table->bucket_limit = new_table->bucket + new_size;
  new_table->num_buckets_used = 0;
  new_table->n_entries = 0;
  new_table->tuning = table->tuning;
  new_table->hasher = table->hasher;
  new_table->comparator = table->comparator;
  new_table->data_freer = table->data_freer;

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

  /* Merely reuse the extra old space into the new table.  */
#if USE_OBSTACK
  new_table->entry_stack = table->entry_stack;
#endif
  new_table->free_entry_list = table->free_entry_list;

  if (transfer_entries (new_table, table, false))
    {
      /* Entries transferred successfully; tie up the loose ends.  */
      free (table->bucket);
      table->bucket = new_table->bucket;
      table->bucket_limit = new_table->bucket_limit;
      table->num_buckets = new_table->num_buckets;
      table->num_buckets_used = new_table->num_buckets_used;
      table->free_entry_list = new_table->free_entry_list;
      /* table->n_entries and table->entry_stack already hold their value.  */
      return true;
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
  table->free_entry_list = new_table->free_entry_list;
  if (! (transfer_entries (table, new_table, true)
         && transfer_entries (table, new_table, false)))
    abort ();
  /* table->n_entries already holds its value.  */
  free (new_table->bucket);
  return false;
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
int
hash_insert_if_absent (hash_table *table, void const *entry,
                       void const **matched_ent)
{
  void *data;
  struct hash_entry *bucket;

  /* The caller cannot insert a NULL entry, since hash_lookup returns NULL
     to indicate "not found", and hash_find_entry uses "bucket->data == NULL"
     to indicate an empty bucket.  */
  if (! entry)
    abort ();

  /* If there's a matching entry already in the table, return that.  */
  if ((data = hash_find_entry (table, entry, &bucket, false)) != NULL)
    {
      if (matched_ent)
        *matched_ent = data;
      return 0;
    }

  /* If the growth threshold of the buckets in use has been reached, increase
     the table size and rehash.  There's no point in checking the number of
     entries:  if the hashing function is ill-conditioned, rehashing is not
     likely to improve it.  */

  if (table->num_buckets_used
      > table->tuning->growth_threshold * table->num_buckets)
    {
      /* Check more fully, before starting real work.  If tuning arguments
         became invalid, the second check will rely on proper defaults.  */
      check_tuning (table);
      if (table->num_buckets_used
          > table->tuning->growth_threshold * table->num_buckets)
        {
          const Hash_tuning *tuning = table->tuning;
          float candidate =
            (tuning->is_n_buckets
             ? (table->num_buckets * tuning->growth_factor)
             : (table->num_buckets * tuning->growth_factor
                * tuning->growth_threshold));

          if (SIZE_MAX <= candidate)
            return -1;

          /* If the rehash fails, arrange to return NULL.  */
          if (!hash_rehash (table, candidate))
            return -1;

          /* Update the bucket we are interested in.  */
          if (hash_find_entry (table, entry, &bucket, false) != NULL)
            abort ();
        }
    }

  /* ENTRY is not matched, it should be inserted.  */

  if (bucket->data)
    {
      struct hash_entry *new_entry = allocate_entry (table);

      if (new_entry == NULL)
        return -1;

      /* Add ENTRY in the overflow of the bucket.  */

      new_entry->data = (void *) entry;
      new_entry->next = bucket->next;
      bucket->next = new_entry;
      table->n_entries++;
      return 1;
    }

  /* Add ENTRY right in the bucket head.  */

  bucket->data = (void *) entry;
  table->n_entries++;
  table->num_buckets_used++;

  return 1;
}

/* hash_insert0 is the deprecated name for hash_insert_if_absent.
   .  */
int
hash_insert0 (hash_table *table, void const *entry, void const **matched_ent)
{
  return hash_insert_if_absent (table, entry, matched_ent);
}

/* If ENTRY matches an entry already in the hash table, return the pointer
   to the entry from the table.  Otherwise, insert ENTRY and return ENTRY.
   Return NULL if the storage required for insertion cannot be allocated.
   This implementation does not support duplicate entries or insertion of
   NULL.  */

void *
hash_insert (hash_table *table, void const *entry)
{
  void const *matched_ent;
  int err = hash_insert_if_absent (table, entry, &matched_ent);
  return (err == -1
          ? NULL
          : (void *) (err == 0 ? matched_ent : entry));
}

/* If ENTRY is already in the table, remove it and return the just-deleted
   data (the user may want to deallocate its storage).  If ENTRY is not in the
   table, don't modify the table and return NULL.  */

void *
hash_delete (hash_table *table, const void *entry)
{
  void *data;
  struct hash_entry *bucket;

  data = hash_find_entry (table, entry, &bucket, true);
  if (!data){
    return NULL;
  }
  table->n_entries--;
  if (!bucket->data){
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
              size_t candidate =
                (tuning->is_n_buckets
                 ? table->num_buckets * tuning->shrink_factor
                 : (table->num_buckets * tuning->shrink_factor
                    * tuning->growth_threshold));
            }
        }
    }
  return data;
}

/* Testing.  */

#if TESTING

void
hash_print (const hash_table *table)
{
  struct hash_entry *bucket = (struct hash_entry *) table->bucket;

  for ( ; bucket < table->bucket_limit; bucket++)
    {
      struct hash_entry *cursor;

      if (bucket)
        printf ("%lu:\n", (unsigned long int) (bucket - table->bucket));

      for (cursor = bucket; cursor; cursor = cursor->next)
        {
          char const *s = cursor->data;
          /* FIXME */
          if (s)
            printf ("  %s\n", s);
        }
    }
}

#endif /* TESTING */
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

#define HASH_MUR(key,keylen,num_bkts,hashv,bkt)                        \
do {                                                                   \
  const uint8_t *_mur_data = (const uint8_t*)(key);                    \
  const int _mur_nblocks = (keylen) / 4;                               \
  uint32_t _mur_h1 = 0xf88D5353;                                       \
  uint32_t _mur_c1 = 0xcc9e2d51;                                       \
  uint32_t _mur_c2 = 0x1b873593;                                       \
  uint32_t _mur_k1 = 0;                                                \
  const uint8_t *_mur_tail;                                            \
  const uint32_t *_mur_blocks = (const uint32_t*)(_mur_data+_mur_nblocks*4); \
  int _mur_i;                                                          \
  for(_mur_i = -_mur_nblocks; _mur_i; _mur_i++) {                      \
    _mur_k1 = MUR_GETBLOCK(_mur_blocks,_mur_i);                        \
    _mur_k1 *= _mur_c1;                                                \
    _mur_k1 = MUR_ROTL32(_mur_k1,15);                                  \
    _mur_k1 *= _mur_c2;                                                \
                                                                       \
    _mur_h1 ^= _mur_k1;                                                \
    _mur_h1 = MUR_ROTL32(_mur_h1,13);                                  \
    _mur_h1 = _mur_h1*5+0xe6546b64;                                    \
  }                                                                    \
  _mur_tail = (const uint8_t*)(_mur_data + _mur_nblocks*4);            \
  _mur_k1=0;                                                           \
  switch((keylen) & 3) {                                               \
    case 3: _mur_k1 ^= _mur_tail[2] << 16;                             \
    case 2: _mur_k1 ^= _mur_tail[1] << 8;                              \
    case 1: _mur_k1 ^= _mur_tail[0];                                   \
    _mur_k1 *= _mur_c1;                                                \
    _mur_k1 = MUR_ROTL32(_mur_k1,15);                                  \
    _mur_k1 *= _mur_c2;                                                \
    _mur_h1 ^= _mur_k1;                                                \
  }                                                                    \
  _mur_h1 ^= (keylen);                                                 \
  MUR_FMIX(_mur_h1);                                                   \
  hashv = _mur_h1;                                                     \
  bkt = hashv & (num_bkts-1);                                          \
} while(0)
