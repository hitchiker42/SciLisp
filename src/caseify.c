#include "common.h"
typedef union m128i {
  __m128i m128i;
  uint64_t m64i[2];
  uint32_t m32i[4];
  uint16_t m16i[8];
  uint8_t m8i[16];
} m128i;
static const m128i downcase_mask __attribute__((aligned(16)))=
  {.m32i={0x20202020,0x20202020,0x20202020,0x20202020}};
static const m128i upcase_mask __attribute__((aligned(16)))=
  {.m32i={0xdfdfdfdf,0xdfdfdfdf,0xdfdfdfdf,0xdfdfdfdf}};

static void upcase(char *str,int len){
  register __m128i mask=upcase_mask.m128i;
  __m128i chars;
  while(len>16){
    chars=_mm_loadu_si128((__m128i*)str);
    chars=_mm_and_si128(mask,chars);
    _mm_storeu_si128((__m128i*)str,chars);
    str+=16;
    len-=16;
  }
  if(len&8){
    *(uint64_t*)str&=upcase_mask.m64i[0];
    str+=8;
    len-=8;
  }
  if(len&4){
    *(uint32_t*)str&=upcase_mask.m32i[0];
    str+=4;
    len-=4;
  }
  if(len&2){
    *(uint16_t*)str&=upcase_mask.m16i[0];
    str+=2;
    len-=2;
  }
  if(len&1){
    *(uint8_t*)str&=upcase_mask.m8i[0];
  }
  return;
}
static inline void upcase_short(char *str,int len){
  while(len--){
    *str=*str&(0xdf);
    str++;
  }
}
static void downcase(char *str,int len){
  register __m128i mask=downcase_mask.m128i;
  __m128i chars;
  while(len>16){
    chars=_mm_loadu_si128((__m128i*)str);
    chars=_mm_or_si128(mask,chars);
    _mm_storeu_si128((__m128i*)str,chars);
    str+=16;
    len-=16;
  }
  if(len&8){
    *(uint64_t*)str|=downcase_mask.m64i[0];
    str+=8;
    len-=8;
  }
  if(len&4){
    *(uint32_t*)str|=downcase_mask.m32i[0];
    str+=4;
    len-=4;
  }
  if(len&2){
    *(uint16_t*)str|=downcase_mask.m16i[0];
    str+=2;
    len-=2;
  }
  if(len&1){
    *(uint8_t*)str|=downcase_mask.m8i[0];
  }
  return;
}
static inline void downcase_short(char *str,int len){
  while(len--){
    *str=*str|0xdf;
    str++;
  }
}
