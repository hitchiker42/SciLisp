#ifndef _SFMT_STATIC_H
#define _SFMT_STATIC_H
//simple wrapper functions around the sfmt functions
//using static internal arrays to hold state
//easy to use but not thread safe
#include "SFMT_lib.h"
#include "SFMT_r.h"
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
static sfmt_t sfmt_static;
static sfmt_buf sfmt_static_buf;
static void sfmt_init_fast_static(){
  sfmt_init_fast_r(&sfmt_static);
  sfmt_init_buf_r(&sfmt_static,&sfmt_static_buf);
}
static void sfmt_init_explicit_static(uint32_t seed){
  sfmt_init_explicit_r(&sfmt_static,seed);
  sfmt_init_buf_r(&sfmt_static,&sfmt_static_buf);
}
static void sfmt_init_stable_static(){
  sfmt_init_stable_r(&sfmt_static);
  sfmt_init_buf_r(&sfmt_static,&sfmt_static_buf);
}
//lets use naming conventions from the rand48 functions
static inline uint32_t sfmt_lrand32(){
  return sfmt_nrand32(&sfmt_static,&sfmt_static_buf);
}
static inline int32_t sfmt_mrand32(){
  return (int32_t) sfmt_lrand32();
}
static inline uint64_t sfmt_lrand64(){
  return sfmt_nrand64(&sfmt_static,&sfmt_static_buf);
}
static inline int64_t sfmt_mrand64(){
  return (int64_t) sfmt_lrand64();
}
static double sfmt_drand64(){
  return sfmt_to_res53(sfmt_lrand64());
}
#endif
