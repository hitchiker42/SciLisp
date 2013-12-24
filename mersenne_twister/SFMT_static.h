#ifndef _SFMT_STATIC_H
#define _SFMT_STATIC_H
//simple wrapper functions around the sfmt functions
//using static internal arrays to hold state
//easy to use but not thread safe
#include "SFMT_lib.h"
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
static sfmt_t uint64_sfmt;
static sfmt_t uint32_sfmt;
void sfmt32_init_fast_static();
void sfmt64_init_fast_static();
void sfmt32_init_stable_static();
void sfmt64_init_stable_static();
uint32_t sfmt_lrand32_buf();
uint64_t sfmt_lrand64_buf();
//lets use naming conventions from the rand48 functions
static inline uint32_t sfmt_lrand32(){
  return sfmt_lrand32_buf();
}
static inline int32_t sfmt_mrand32(){
  return (int32_t) sfmt_lrand32();
}
//this probably shouldn't be used as it needs
//to generate two 32 bit integers as opposed to
//sfmt_drand64 which only needs one 64 bit one
static inline double sfmt_drand32(){
  return sfmt_to_res53_mix(sfmt_lrand32(),sfmt_lrand32());
}
static inline uint64_t sfmt_lrand64(){
  return sfmt_lrand64_buf();
}
static inline int64_t sfmt_mrand64(){
  return (int64_t) sfmt_lrand64();
}
static double sfmt_drand64(){
  return sfmt_to_res53(sfmt_lrand64());
}
#endif
