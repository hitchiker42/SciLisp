#include "SFMT_r.h"
#include <sys/time.h>
#ifndef U32_BUF_LEN
#define U32_BUF_LEN 1000
#endif
#ifndef U64_BUF_LEN
#define U64_BUF_LEN 500
#endif
#define time_seed()                             \
  (gettimeofday(&tp,NULL),(uint32_t)(tp.tv_sec^tp.tv_usec))
struct sfmt_buf {
  union {
    uint32_t sfmt_u32_buf[U32_BUF_LEN] __attribute__((aligned(16)));
    uint64_t sfmt_u64_buf[U64_BUF_LEN] __attribute__((aligned(16)));
  };
  union {
    uint32_t u32_buf_index;
    uint32_t u64_buf_index;
  };
  enum {
    _32,
    _64,
  } element_size;
};
union lcg_state {
  long X_i;
  unsigned short state[3];
};
void sfmt32_init_fast_r(sfmt_t *sfmt){
  struct timeval tp;
  sfmt_init_gen_rand(sfmt,time_seed());
}
void sfmt64_init_fast_r(sfmt_t *sfmt){
  struct timeval tp;
  sfmt_init_gen_rand(sfmt,time_seed());
}
void sfmt_init_stable_r(sfmt_t *sfmt){
  uint32_t init_array[SFMT_N32];
  struct timeval tp;
  union lcg_state lcg_init;
  lcg_init.X_i=time_seed();
  int i;
  for(i=0;i<SFMT_N32;i++){
    init_array[i]=nrand48(lcg_init.state);
  }
  sfmt_init_by_array(sfmt,init_array,SFMT_N32);
}
void sfmt32_init_stable_r(sfmt_t *sfmt) 
  __attribute__((alias("sfmt_init_stable_r")));
void sfmt64_init_stable_r(sfmt_t *sfmt) 
  __attribute__((alias("sfmt_init_stable_r")));
static inline void sfmt_fill_u32_buf(sfmt_t *sfmt,sfmt_buf *buf){
  sfmt_fill_array32(sfmt,buf->sfmt_u32_buf,U32_BUF_LEN);
  buf->u32_buf_index=0;
};
static inline void sfmt_fill_u64_buf(sfmt_t *sfmt,sfmt_buf *buf){
  sfmt_fill_array64(sfmt,buf->sfmt_u64_buf,U64_BUF_LEN);
  buf->u64_buf_index=0;
};
void sfmt_init_buf32(sfmt_t *sfmt,sfmt_buf *buf){
  sfmt_fill_u32_buf(sfmt,buf);
  buf->element_size=_32;
}
void sfmt_init_buf64(sfmt_t *sfmt,sfmt_buf *buf){
  sfmt_fill_u64_buf(sfmt,buf);
  buf->element_size=_64;
}
uint32_t sfmt_nrand32_buf(sfmt_t *sfmt,sfmt_buf *buf){
  if(buf->u32_buf_index>=U32_BUF_LEN){
    sfmt_fill_u32_buf(sfmt,buf);
  }
  return buf->sfmt_u32_buf[buf->u32_buf_index++];
}
uint64_t sfmt_nrand64_buf(sfmt_t *sfmt,sfmt_buf *buf){
  if(buf->u64_buf_index>=U64_BUF_LEN){
    sfmt_fill_u64_buf(sfmt,buf);
  }
  return buf->sfmt_u64_buf[buf->u64_buf_index++];
}
