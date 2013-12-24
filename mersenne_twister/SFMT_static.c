#include "SFMT_static.h"
#ifndef U32_BUF_LEN
#define U32_BUF_LEN 1000
#endif
#ifndef U64_BUF_LEN
#define U64_BUF_LEN 500
#endif
#define time_seed()                             \
  (gettimeofday(&tp,NULL),(uint32_t)(tp.tv_sec^tp.tv_usec))

static uint32_t sfmt_init_array[SFMT_N32];
static uint32_t sfmt_u32_buf[U32_BUF_LEN] __attribute__((aligned(16)));
static uint64_t sfmt_u64_buf[U64_BUF_LEN] __attribute__((aligned(16)));
//these are set to the lengths of the arrays so the arrays get
//initialized on the first call
static uint32_t sfmt_u32_buf_index=U32_BUF_LEN;
static uint32_t sfmt_u64_buf_index=U64_BUF_LEN;
static struct timeval tp;
//I could (and did at first) use macros here, but for just two different
//functions it's not worth it
void sfmt32_init_fast_static(){
  sfmt_init_gen_rand(&uint32_sfmt,time_seed());

}
void sfmt64_init_fast_static(){
  sfmt_init_gen_rand(&uint64_sfmt,time_seed());
}
void sfmt32_init_stable_static(){
  srand48(time_seed());
  int i;
  for(i=0;i<SFMT_N32;i++){
    sfmt_init_array[i]=(uint32_t)(lrand48());
  }
  sfmt_init_by_array(&uint32_sfmt,sfmt_init_array,SFMT_N32);
}
void sfmt64_init_stable_static(){
  srand48(time_seed());
  int i;
  for(i=0;i<SFMT_N32;i++){
    sfmt_init_array[i]=(uint32_t)(lrand48());
  }
  sfmt_init_by_array(&uint64_sfmt,sfmt_init_array,SFMT_N32);
}
static inline void sfmt_fill_uint32_buf(){
  sfmt_fill_array32(&uint32_sfmt,sfmt_u32_buf,U32_BUF_LEN);
  sfmt_u32_buf_index=0;
}
static inline void sfmt_fill_uint64_buf(){
  sfmt_fill_array64(&uint64_sfmt,sfmt_u64_buf,U64_BUF_LEN);
  sfmt_u64_buf_index=0;
}
uint32_t sfmt_lrand32_buf(){
  if(sfmt_u32_buf_index>=U32_BUF_LEN){
    sfmt_fill_uint32_buf();
  }
  return sfmt_u32_buf[sfmt_u32_buf_index++];
}
uint64_t sfmt_lrand64_buf(){
  if(sfmt_u64_buf_index>=U64_BUF_LEN){
    sfmt_fill_uint64_buf();
  }
  return sfmt_u64_buf[sfmt_u64_buf_index++];
}
