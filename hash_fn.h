#ifndef __HASH_FN_H
#define __HASH_FN_H
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
uint32_t murmur_hash(char* key,int keylen){
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
long fnv_hash(char* key,int keylen){
  char *raw_data=key;
  int i;
  long hash=offset_basis;
  for(i=0; i < keylen; i++){
      hash = (hash * fnv_prime) ^ raw_data[i];
  }
  return hash;
}
#endif
