#include "SFMT.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
int main(){
  sfmt_t *sfmt=malloc(sizeof(sfmt_t));
  sfmt_buf *buf=malloc(1000*(sizeof(uint32_t))+sizeof(uint32_t)+sizeof(int));
  sfmt32_init_fast_static();
  uint32_t randu32_static=sfmt_lrand32();
  int32_t randi32_static=sfmt_mrand32();
  double randr32_static=sfmt_drand32();
  sfmt64_init_stable_static();
  uint64_t randu64_static=sfmt_lrand64();
  int64_t randi64_static=sfmt_mrand64();
  double randr64_static=sfmt_drand64();
  sfmt32_init_fast_r(sfmt);
  sfmt_init_buf32(sfmt,buf);
  uint32_t randu32_r=sfmt_nrand32(sfmt,buf);
  int32_t randi32_r=sfmt_jrand32(sfmt,NULL);
  double randr32_r=sfmt_erand32(sfmt,buf);
  sfmt64_init_fast_r(sfmt);
  sfmt_init_buf64(sfmt,buf);
  uint64_t randu64_r=sfmt_nrand64(sfmt,buf);
  int64_t randi64_r=sfmt_jrand64(sfmt,NULL);
  double randr64_r=sfmt_erand64(sfmt,buf);
  printf("SFMT_static tests:\n"
         "32 static,using init fast:\n"
         "u32=%u,i32=%d,r64=%f\n"
         "64 static,using init stable:\n"
         "u64=%lu,i64=%ld,r64=%f\n"
         "32 re-entrant,using init fast:\n"
         "u32=%u,i32=%d (without buf),r64=%f\n"
         "64 re-entrant,using init stable:\n"
         "u64=%lu,i64=%ld (without buf),r64=%f\n",
         randu32_static,randi32_static,randr32_static,
         randu64_static,randi64_static,randr64_static,
         randu32_r,randi32_r,randr32_r,
         randu64_r,randi64_r,randr64_r);
}
