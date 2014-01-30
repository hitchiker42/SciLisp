#ifndef _SCILISP_SIMD_H
#define _SCILISP_SIMD_H
#ifndef __x86_64__
union simd128_real64 {
  double real64_vec[2];
};
union simd128_real32 {
  float real32_vec[4];
};
union simd128_int_n {
  uint8_t int8_vec[16];
  uint16_t int16_vec[8];
  uint32_t int32_vec[4];
  uint64_t int64_vec[2];
};
typedef union simd128_int_n simd128_int8;
typedef union simd128_int_n simd128_int16;
typedef union simd128_int_n simd128_int32;
typedef union simd128_int_n simd128_int64;
#else
#include "x86intrin.h"
union simd128_real64 {
  double real64_vec[2];
  __m128d m128d;
};
union simd128_real32 {
  float real32_vec[4];
  __m128 m128f;
};
union simd128_int_n {
  uint8_t int8_vec[16];
  uint16_t int16_vec[8];
  uint32_t int32_vec[4];
  uint64_t int64_vec[2];
  __m128i m128i
};
typedef struct simd128_int_n simd128_int8;
typedef struct simd128_int_n simd128_int16;
typedef struct simd128_int_n simd128_int32;
typedef struct simd128_int_n simd128_int64;
#ifdef __AVX__
union simd256_real64 {
  double real64_vec[4];
  __m256d m256d;
};
union simd256_real32 {
  float real32_vec[8];
  __m256 m256f;
};
#ifdef __AVX2__
union simd256_int_n {
  uint8_t int8_vec[32];
  uint16_t int16_vec[16];
  uint32_t int32_vec[8];
  uint64_t int64_vec[4];
  __m256i m256i
};
typedef struct simd256_int_n simd256_int8;
typedef struct simd256_int_n simd256_int16;
typedef struct simd256_int_n simd256_int32;
typedef struct simd256_int_n simd256_int64;
#else
union simd256_int_n {
  uint8_t int8_vec[32];
  uint16_t int16_vec[16];
  uint32_t int32_vec[8];
  uint64_t int64_vec[4];
};
typedef struct simd256_int_n simd256_int8;
typedef struct simd256_int_n simd256_int16;
typedef struct simd256_int_n simd256_int32;
typedef struct simd256_int_n simd256_int64;
#endif /*ifdef __AVX2__*/
#else
union simd256_real64 {
  double real64_vec[4];
  __m256d m256d;
};
union simd256_real32 {
  float real32_vec[8];
  __m256 m256f;
};
#endif /*ifdef __AVX__*/
#endif /*ifdef __x86_64__*/
union simd128 {
  simd128_real32 simd_real32;
  simd128_real64 simd_real64;
  simd128_int_n simd_int_n;
};
union simd256 {
  simd256_real32 simd_real32;
  simd256_real64 simd_real64;
  simd256_int_n simd_int_n;
};
#endif /*_SCILISP_SIMD_H*/
