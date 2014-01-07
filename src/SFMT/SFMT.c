#include "SFMT_r.h"
#include "SFMT_static.h"
#include <sys/time.h>
union uintN_t {
  uint32_t uint32[2];
  uint64_t uint64;
};
#define time_seed()                             \
  (gettimeofday(&tp,NULL),(uint32_t)(tp.tv_sec^tp.tv_usec))
union lcg_state {
  long X_i;
  unsigned short state[3];
};
void sfmt_init_fast_r(sfmt_t *sfmt){
  struct timeval tp;
  sfmt_init_gen_rand(sfmt,time_seed());
}
void sfmt_init_explicit_r(sfmt_t *sfmt,uint32_t seed){
  sfmt_init_gen_rand(sfmt,seed);
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
void sfmt_init_buf_r(sfmt_t *sfmt,sfmt_buf *buf){
  sfmt_fill_array32(sfmt,buf->buf,BUF_LEN);
  buf->buf_index=0;

}
uint32_t sfmt_nrand32_buf(sfmt_t *sfmt,sfmt_buf *buf){
  if(buf->buf_index>=BUF_LEN){
    sfmt_init_buf_r(sfmt,buf);
  }
  return buf->buf[buf->buf_index++];
}
uint64_t sfmt_nrand64_buf(sfmt_t *sfmt,sfmt_buf *buf){
  if(buf->buf_index+1>=BUF_LEN){
    sfmt_init_buf_r(sfmt,buf);
  }
  union uintN_t retval;
  retval.uint32[0]=buf->buf[buf->buf_index++];
  retval.uint32[1]=buf->buf[buf->buf_index++];
  return retval.uint64;
}
