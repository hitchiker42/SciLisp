/* 48 bit linear congrguential random number generator */
#define LCG_a 25214903917
#define LCG_c 11
#define LCG_m (1<<48)
uint16_t LCG_state48[3]={0,0,0};
void LCG_rand_seed(uint64_t seed){
  union {
    uint64_t int64;
    uint16_t int16[4];
  } seed16v;
  if(!seed){
    seed=time(NULL);
  }
  seed16v.int64=seed;
  LCG_state48[2]=seed16v.int16[0];
  LCG_state48[1]=seed16v.int16[1];
  LCG_state48[0]=seed16v.int16[2];
  return;
}
void LCG_rand48_iter(){
  //copy what libc does
  uint64_t X,result;
  X = (uint64_t) LCG_state48[2] << 32 | (uint32_t) LCG_state48[1] << 16 |
    LCG_state48[0];
  result = X * LCG_a + LCG_c;
  LCG_state48[0] = result & 0xffff;
  LCG_state48[1] = (result >> 16) & 0xffff;
  LCG_state48[2] = (result >> 32) & 0xffff;
  return;
}
#ifndef IEEE754_DOUBLE_BIAS
#define IEEE754_DOUBLE_BIAS 0x3ff
#endif
union ieee754_double {
  double d;
  struct {
    unsigned int mantissa1:32;
    unsigned int mantissa0:20;
    unsigned int exponent:11;
    unsigned int negative:1;
  } ieee;
};
double LCG_drand48(){
//literally just copied this from glibc
  union ieee754_double temp;
  /* Construct a positive double with the 48 random bits distributed over
     its fractional part so the resulting FP number is [0.0,1.0).  */
  LCG_rand48_iter();
  temp.ieee.negative = 0;
  temp.ieee.exponent = IEEE754_DOUBLE_BIAS;
  temp.ieee.mantissa0 = (LCG_state48[2] << 4) | (LCG_state48[1] >> 12);
  temp.ieee.mantissa1 = ((LCG_state48[1] & 0xfff) << 20) | (LCG_state48[0] << 4);

  /* Please note the lower 4 bits of mantissa1 are always 0.  */
  return temp.d - 1.0;
}
uint64_t LCG_lrand48(){
  LCG_rand48_iter();
  return (uint64_t) (LCG_state48[2]<<15 | LCG_state48[1] >> 1);
}
int64_t LCG_mrand48(){
  LCG_rand48_iter();
  return (int64_t) ((LCG_state48[2]<<16)|LCG_state48[1]);
}
