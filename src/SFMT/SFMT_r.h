#ifndef SFMT_R_H
#define SFMT_R_H
#include "SFMT_lib.h"
#include <time.h>
#include <stdlib.h>
#ifndef BUF_LEN
#define BUF_LEN 4000
#endif
typedef struct sfmt_buf sfmt_buf;
static void *(*sfmt_malloc)(size_t)=malloc;
static void (*sfmt_free)(void*)=free;
//re-entrant wrapper functions around sfmt functions
//require the user to pass an sfmt_t value for each call
struct sfmt_buf {
  uint32_t buf[BUF_LEN] __attribute__((aligned(16)));
  uint32_t buf_index;
};
void sfmt_init_fast_r(sfmt_t *sfmt);
void sfmt_init_explicit_r(sfmt_t *sfmt,uint32_t seed);
void sfmt_init_stable_r(sfmt_t *sfmt);
uint32_t sfmt_nrand32_buf(sfmt_t *sfmt,sfmt_buf *buf);
uint64_t sfmt_nrand64_buf(sfmt_t *sfmt,sfmt_buf *buf);
void sfmt_init_buf_r(sfmt_t *sfmt,sfmt_buf *buf);
  
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
static inline uint64_t sfmt_nrand64(sfmt_t *sfmt,sfmt_buf *buf){
  if(buf){
    return sfmt_nrand64_buf(sfmt,buf);
  } else {
    if(!(sfmt->idx&1)){
      sfmt->idx++;
    }
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

