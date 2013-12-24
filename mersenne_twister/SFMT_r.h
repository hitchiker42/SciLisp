#ifndef SFMT_R_H
#define SFMT_R_H
#include "SFMT_lib.h"
#include <time.h>
#include <stdlib.h>
//re-entrant wrapper functions around sfmt functions
//require the user to pass an sfmt_t value for each call
typedef struct sfmt_buf sfmt_buf;
void sfmt32_init_fast_r(sfmt_t *sfmt);
void sfmt64_init_fast_r(sfmt_t *sfmt);
void sfmt32_init_stable_r(sfmt_t *sfmt);
void sfmt64_init_stable_r(sfmt_t *sfmt);
uint32_t sfmt_nrand32_buf(sfmt_t *sfmt,sfmt_buf *buf);
uint64_t sfmt_nrand64_buf(sfmt_t *sfmt,sfmt_buf *buf);
int32_t sfmt_jrand32_buf(sfmt_t *sfmt,sfmt_buf *buf);
int64_t sfmt_jrand64_buf(sfmt_t *sfmt,sfmt_buf *buf);
double sfmt_erand32_buf(sfmt_t *sfmt,sfmt_buf *buf);
double sfmt_erand64_buf(sfmt_t *sfmt,sfmt_buf *buf);
void sfmt_init_buf32(sfmt_t *sfmt,sfmt_buf *buf);
void sfmt_init_buf64(sfmt_t *sfmt,sfmt_buf *buf);
//lets use naming conventions from the rand48 functions
static inline uint32_t sfmt_nrand32(sfmt_t *sfmt,sfmt_buf *buf){
  if(buf){
    return sfmt_nrand32_buf(sfmt,buf);
  } else {
    return sfmt_genrand_uint32(sfmt);
  }
}
static inline int32_t sfmt_jrand32(sfmt_t *sfmt,sfmt_buf *buf){
  return (int32_t) sfmt_nrand32(sfmt,buf);
}
//this probably shouldn't be used as it needs
//to generate two 32 bit integers as opposed to
//sfmt_drand64 which only needs one 64 bit one
static inline double sfmt_erand32(sfmt_t *sfmt,sfmt_buf *buf){
  return sfmt_to_res53_mix(sfmt_nrand32(sfmt,buf),sfmt_nrand32(sfmt,buf));
}
static inline uint64_t sfmt_nrand64(sfmt_t *sfmt,sfmt_buf *buf){
  if(buf){
    return sfmt_nrand64_buf(sfmt,buf);
  } else {
    return sfmt_genrand_uint64(sfmt);
  }
}
static inline int64_t sfmt_jrand64(sfmt_t *sfmt,sfmt_buf *buf){
  return (int64_t) sfmt_nrand64(sfmt,buf);
}
static double sfmt_erand64(sfmt_t *sfmt,sfmt_buf *buf){
  return sfmt_to_res53(sfmt_nrand64(sfmt,buf));
}
#endif

