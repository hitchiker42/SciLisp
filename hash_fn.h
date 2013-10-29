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
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
#define fnv_prime_128 309485009821345068724781371
#define fnv_prime_256 374144419156711147060143317175368453031918731002211
#define fnv_prime_512 "35835915874844867368919076489095108449946327955754392558399825615420669938882575126094039892345713852759"
#define fnv_prime_1024 "5016456510113118655434598811035278955030765345404790744303017523831112055108147451509157692220295382716162651878526895249385292291816524375083746691371804094271873160484737966720260389217684476157468082573"
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
#define offset_basis_128 144066263297769815596495629667062367629
#define offset_basis_256 100029257958052580907070968620625704837092796014241193945225284501741471925557
#define offset_basis_512 "9659303129496669498009435400716310466090418745672637896108374329434462657994582932197716438449813051892206539805784495328239340083876191928701583869517785"
#define offset_basis_1024 "14197795064947621068722070641403218320880622795441933960878474914617582723252296732303717722150864096521202355549365628174669108571814760471015076148029755969804077320157692458563003215304957150157403644460363550505412711285966361610267868082893823963790439336411086884584107735010676915"
static unsigned int fnv_hash32(void* key, int keylen){
  char *raw_data=(char*)key;
  int i;
  unsigned long hash=offset_basis_32;
  for(i=0;i<keylen;i++){
    hash=(hash*fnv_prime_32)^raw_data[i];
  }
}
static unsigned long fnv_hash(void* key,int keylen){
  char *raw_data=(char*)key;
  int i;
  unsigned long hash=offset_basis_64;
  for(i=0; i < keylen; i++){
      hash = (hash * fnv_prime_64) ^ raw_data[i];
  }
  return hash;
}
static mpz_t fnv_hash_mpz(void* key, int keylen,int keysize){
  mpz_t hash,fnv_prime,cur_octet;
  mpz_init_set_str(hash,offset_basis_256,0);
  mpz_init_set_str(fnv_prime,fnv_prime_256,0);
  mpz_init(cur_octet);
  char* raw_data=(char*)key;
  int i;
  for(i=0;i<keylen;i++){
    mpz_set(cur_octet,(unsigned long)raw_data[i]);
    mpz_mul(hash,hash,fnv_prime);
    mpz_xor(hash,hash,cur_octet);
  }
  return hash;
}

static long bernstein_hash(void* key,int keylen){
  unsigned _hb_keylen=keylen;
  char *_hb_key=(char*)(key);
  (hashv) = 0;
  while (_hb_keylen--)  { (hashv) = ((hashv) * 33) + *_hb_key++; }
  return hashv;
}
unsigned oat_hash ( void *key, int len ){
  unsigned char *p = key;
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
#endif
