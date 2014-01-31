/* Define simd types, using arrays to emulate unsuported types

   Copyright (C) 2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
#ifndef _SCILISP_SIMD_TYPES_H
#define _SCILISP_SIMD_TYPES_H
#define SIMD128P(obj) (obj.tag>=sexp_simd128_real32 && obj.tag <=sexp_simd128_int64)
#define SIMD256P(obj) (obj.tag>=sexp_simd256_real32 && obj.tag <=sexp_simd256_int64)
#define SIMD128_REAL32P(obj) (obj.tag == sexp_simd128_real32)
#define SIMD128_REAL64P(obj) (obj.tag == sexp_simd128_real64)
#define SIMD128_INT8P(obj) (obj.tag == sexp_simd128_int8)
#define SIMD128_INT16P(obj) (obj.tag == sexp_simd128_int16)
#define SIMD128_INT32P(obj) (obj.tag == sexp_simd128_int32)
#define SIMD128_INT64P(obj) (obj.tag == sexp_simd128_int64)
#define SIMD256_REAL32P(obj) (obj.tag == sexp_simd256_real32)
#define SIMD256_REAL64P(obj) (obj.tag == sexp_simd256_real64)
#define SIMD256_INT8P(obj) (obj.tag == sexp_simd256_int8)
#define SIMD256_INT16P(obj) (obj.tag == sexp_simd256_int16)
#define SIMD256_INT32P(obj) (obj.tag == sexp_simd256_int32)
#define SIMD256_INT64P(obj) (obj.tag == sexp_simd256_int64)
typedef union simd128_real64 simd128_real64;
typedef union simd128_real32 simd128_real32;
typedef union simd128_int_n simd128_int8;
typedef union simd128_int_n simd128_int16;
typedef union simd128_int_n simd128_int32;
typedef union simd128_int_n simd128_int64;
typedef union simd256_real64 simd256_real64;
typedef union simd256_real32 simd256_real32;
typedef union simd256_int_n simd256_int8;
typedef union simd256_int_n simd256_int16;
typedef union simd256_int_n simd256_int32;
typedef union simd256_int_n simd256_int64;
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
union simd256_int_n {
  uint8_t int8_vec[32];
  uint16_t int16_vec[16];
  uint32_t int32_vec[8];
  uint64_t int64_vec[4];
};
union simd256_real64 {
  double real64_vec[4];
};
union simd256_real32 {
  float real32_vec[8];
};
#else
#include "x86intrin.h"
union simd128_real64 {
  double real64_vec[2];
  __m128d m128d;
};
typedef union simd128_real64 m128d;
union simd128_real32 {
  float real32_vec[4];
  __m128 m128f;
};
typedef union simd128_real32 m128f;
union simd128_int_n {
  uint8_t int8_vec[16];
  uint16_t int16_vec[8];
  uint32_t int32_vec[4];
  uint64_t int64_vec[2];
  __m128i m128i
};
typedef union simd128_int_n m128i;
#ifdef __AVX__
union simd256_real64 {
  double real64_vec[4];
  __m256d m256d;
};
typedef union simd256_real64 m256d;
union simd256_real32 {
  float real32_vec[8];
  __m256 m256f;
};
typedef union simd256_real32 m256f;
#ifdef __AVX2__
union simd256_int_n {
  uint8_t int8_vec[32];
  uint16_t int16_vec[16];
  uint32_t int32_vec[8];
  uint64_t int64_vec[4];
  __m256i m256i
};
typedef union simd256_int_n m256i;
#else
union simd256_int_n {
  uint8_t int8_vec[32];
  uint16_t int16_vec[16];
  uint32_t int32_vec[8];
  uint64_t int64_vec[4];
};
#endif /*ifdef __AVX2__*/
#else
union simd256_real64 {
  double real64_vec[4];
};
union simd256_real32 {
  float real32_vec[8];
};
#endif /*ifdef __AVX__*/
#endif /*ifdef __x86_64__*/
union simd128 {
  simd128_real32 real32;
  simd128_real64 real64;
  simd128_int_n int_n;
};
union simd256 {
  simd256_real32 real32;
  simd256_real64 real64;
  simd256_int_n int_n;
};
typedef union simd128 simd128;
typedef union simd256 simd256;
#endif /*_SCILISP_SIMD_H*/
