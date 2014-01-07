/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "hash_fn.h"
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
#define MUR_PLUS0_ALIGNED(p) (((uint64_t)p & 0x3) == 0)
#define MUR_PLUS1_ALIGNED(p) (((uint64_t)p & 0x3) == 1)
#define MUR_PLUS2_ALIGNED(p) (((uint64_t)p & 0x3) == 2)
#define MUR_PLUS3_ALIGNED(p) (((uint64_t)p & 0x3) == 3)
#define WP(p) ((uint32_t*)((uint64_t)(p) & ~3UL))
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

uint32_t murmur_hash(const void *key,int keylen){
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
uint32_t fnv_hash32(const void *key, int keylen){
  const uint8_t *raw_data=(const uint8_t*)key;
  int i;
  uint64_t hash=offset_basis_32;
  for(i=0;i<keylen;i++){
    hash=(hash^raw_data[i])*fnv_prime_32;
  }
}
uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
mpz_t* fnv_hash_mpz(const void *key, int keylen,int keysize){
  mpz_t *hash,*fnv_prime,*cur_octet;
  mpz_init_set_str(*hash,offset_basis_512,0);
  mpz_init_set_str(*fnv_prime,fnv_prime_512,0);
  mpz_init(*cur_octet);
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  for(i=0;i<keylen;i++){
    mpz_set_ui(*cur_octet,(uint64_t)raw_data[i]);
    mpz_xor(*hash,*hash,*cur_octet);
    mpz_mul(*hash,*hash,*fnv_prime);
  }
  return hash;
}

uint64_t bernstein_hash(const void *key,int keylen){
  unsigned _hb_keylen=keylen;
  const uint8_t *_hb_key=(const uint8_t*)(key);
  uint64_t hashv=0;
  while (_hb_keylen--)  { (hashv) = ((hashv) * 33) + *_hb_key++; }
  return hashv;
}
unsigned oat_hash ( const void *key, int len ){
  const uint8_t *p = key;
  unsigned h = 0;
  int i;
  for ( i = 0; i < len; i++ ) {
    h += p[i];
    h += ( h << 10 );
    h ^= ( h >> 6 );
  }

  h += ( h << 3 );
  h ^= ( h >> 11 );
  h += ( h << 15 );
  return h;
}
uint64_t Pearson_hash16(const void *key, int keylen){
  int h, i, j, k;
  uint8_t *x=(uint8_t*)key,ch;
  union {
    char hh[8];
    uint64_t hash_val;
  } hash;
  ch=x[0]; // save first byte
  for (j=0; j<8; j++) {
    // standard Pearson hash (output is h)
    h=0;
    for (i=0; i<keylen; i++) {
      k=h^x[i];
      h=PEARSON_LOOKUP[k];
    }
    hash.hh[j]=h; // store result
    x[0]=x[0]+1; // increment first data byte by 1
  }
  x[0]=ch; // restore first byte
  // concatenate the 8 stored values of h;
  return hash.hash_val; // output 64-bit 16 hex bytes hash
}
pearson_hash_n(128)
pearson_hash_n(256)
pearson_hash_n(512)
pearson_hash_n(1024)
