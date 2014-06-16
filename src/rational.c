/* Rational types and functions (a rational is a nonreducable integer fraction)

   Copyright (C) 2013-2014 Tucker DiNapoli

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
#include "common.h"
typedef union rational rational_t;
typedef struct rational64 rational64_t;
//the tag will need to be kept in the sexp tag field
union rational {
  struct {
    int32_t num;//numerator
    uint32_t denom;//denominator
  };
  mpq_t *mpq;
  rational64 *rat64;
};
struct rational64 {
  int64_t numerator;
  uint64_t denominator;
};
const rational rational_0 = {.num=0,.denom=1};
const rational rational_1 = {.num=1,.denom=1};
const rational rational_inf = {.num=1,.denom=0};
#define ctz(a,b) __builtin_ctzl(a,b)
#define rational_literal(a,b) (rational){.num=a,.denom=b}
uint64_t binary_gcd(uint64_t a,uint64_t b){
  //assume a > 0 and b > 0
  //find the common factors of two (using ctz)
  uint64_t common_factor_of_2 = ctz(a|b);
  a >>= ctz(a);//shift away common factors of 2
  //now a is always odd
  //loop until a == b or a == 1, insuring a is always the min of a and b
  while(1){
    b>>=ctz(b);//now b is odd
    if(a == b){//we're done
      break;
    }
    if(a > b){//make sure a is always the lesser of the two
      SWAP(a,b);
    }
    if(a == 1){
      break;
    }
    b-=a;
  }
  return a << common_factor_of_2;
}
//assumes tail recursion
uint64_t euclid_gcd(uint64_t a,uint64_t b){
  if(b=0){
    return a;
  } else {
    euclid_gcd_acc(b,a%b);
  }
}
          
rational make_rational(int32_t a,int32_t b){
  if(a==0){
    return rational_0;
  }
  if(b==0){//not sure what to do about n/0 rationals;
    return rational_inf;
  }
  //get the sign of the resulting rational
  //the sign is always stored in the numerator
  uint8_t sign_a=(a>>31);
  uint8_t sign_b=(b>>31);
  uint8_t sign=sign_a^sign_b;
  uint32_t x = ((a+sign_a) ^ sign_a);
  uint32_t y = ((b+sign_b) ^ sign_b);
  if(x>y){
    SWAP(x,y);
  }
  uint32_t z = binary_gdc(x,y);
  if(z>1){
    x/=z;
    y/=z;
  }
  if(sign){
    x=(x&-1)+1;
  }
  return rational_literal(x,y);
}
rational_t rational_neg(rational x){
  return rational_literal(-x.num,x.denom);
}