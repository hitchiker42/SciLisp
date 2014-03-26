/*Bitwise and logical operations
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
//bitwise primitives
#include "common.h"
#define lop_to_fun(op,op_name)                                          \
  sexp lisp_unchecked_##op_name (sexp x,sexp y){                        \
    return long_sexp(x.val.int64 op y.val.int64);                       \
  }                                                                     \
  sexp lisp_##op_name(sexp x,sexp y){                                   \
  if(!INT_ANYP(x) || !INT_ANYP(y)){                                     \
    raise_simple_error                                                  \
      (Etype,format_type_error2(#op_name,"integer",x.tag,"integer",y.tag)); \
  } else {                                                              \
    return long_sexp(x.val.uint64 op y.val.uint64);                     \
  }                                                                     \
  }
lop_to_fun(>>,rshift);
lop_to_fun(<<,lshift);
lop_to_fun(^,xor);
lop_to_fun(&,logand);
lop_to_fun(&~,logandn);
lop_to_fun(|,logior);
sexp ash(sexp x,sexp y){
  if(!INT_ANYP(x) || !INT_ANYP(y)){
    raise_simple_error(Etype,format_type_error2("ash","integer",
                                                x.tag,"integer",y.tag));
  } else if(y.val.int64>=0){
    return int64_sexp(x.val.int64>>y.val.int64);
  } else {
    return int64_sexp(x.val.int64<<(y.val.int64&(labs(x.val.int64))));
  }
}
sexp lsh(sexp x,sexp y){
  if(!INT_ANYP(x) || !INT_ANYP(y)){
    raise_simple_error(Etype,format_type_error2("ash","integer",
                                                x.tag,"integer",y.tag));
  } else if(y.val.int64>=0){
    return uint64_sexp(x.val.uint64<<y.val.uint64);
  } else {
    return uint64_sexp(x.val.uint64>>labs(y.val.uint64));
  }
}
sexp lisp_lognot(sexp x){
  if(!INTP(x)){
    raise_simple_error(Etype,format_type_error("lognot","integer",x.tag));
  } else {
    return long_sexp(~x.val.uint64);
  }
}
static const uint8_t bitmasks[8]={0x1,0x2,0x4,0x8,0x10,0x20,0x40,0x80};

/* Assume for boolean operations we have two operands A and B and each is either true (1) or false (0)
   there are thus 4 possible conditons, forming the following table
   |B=1|B=0|
A=1| 1 | 2 |
   ---------
A=0| 4 | 8 |
   ---------
   where 1 2 3 4 refer to the value in that box, this forms 16 operations
   which can be represented by a hexadecimal digit who's bits are set
   based on the values of each cell
*/
//I might use different names in lisp ,but I want to be as descriptive as possible in c
enum bool_ops {
  bool_false=0x0,//0 regardless of A and B
  bool_and=0x1,//A&B
  bool_A_only=0x2,//A&(!B)
  bool_A=0x3,//A
  bool_B_only=0x4,//!A&B
  bool_B=0x5,//B
  bool_xor=0x6,//A^B
  bool_ior=0x7,//A|B
  bool_nor=0x8,//!(A&B)
  bool_eqv=0x9,//A==B
  bool_not_B=0xa,//!B
  bool_not_A_or_B=0xb,//A&B|!B
  bool_not_A=0xc,//!A
  bool_A_or_not_B=0xd,//A&B|!A
  bool_nand=0xe,//!(A&B)
  bool_true=0xf,//1 regardless of A and B
};
//functions which take 2 integers and return lisp_true or lisp_false
#define c_bool(name,conditional)                                        \
  uint64_t __attribute__((const))c_bool_##name(uint64_t A,uint64_t B){  \
    return conditional;                                                 \
  } 
c_bool(false,0)//damnit emacs, false isn't a keyword
c_bool(and,(A&B))
c_bool(A_only,(A&(~B)))
c_bool(A,(A))
c_bool(B_only,(B&(~A)))
c_bool(B,(B))
c_bool(xor,(A^B))
c_bool(ior,(A|B))
c_bool(nor,(~A&~B))
c_bool(eqv,(A&B|(~A&~B)))
c_bool(not_B,(~B))
c_bool(not_A_or_B,(~A|B))
c_bool(not_A,(~A))
c_bool(A_or_not_B,(A|~B))
c_bool(nand,(~(A&B)))
c_bool(true,(uint64_t)-1)//easer than writing 0xf...f

static uint64_t(*bool_dispatch_table[16])(uint64_t,uint64_t) = 
{c_bool_false,c_bool_and,c_bool_A_only,c_bool_A,c_bool_B_only,c_bool_B,
 c_bool_xor,c_bool_ior,c_bool_nor,c_bool_eqv,c_bool_not_B,c_bool_not_A_or_B,
 c_bool_not_A,c_bool_A_or_not_B,c_bool_nand,c_bool_true};
#define lisp_bool(name)                                                 \
  sexp lisp_bool_##name(sexp A,sexp B){                                 \
    if(!INT_ANYP(A) || !INT_ANYP(B)){                                   \
      raise_simple_error(Etype,format_type_error2("ash","integer",      \
                                                  x.tag,"integer",y.tag)); \
    }                                                                   \
    return uint64_sexp(c_bool_##name(A.val.uint64,B.val.uint64));       \
  }

sexp bit_scan_forward(sexp x){
  //typecheck
  return int64_sexp((int64_t)(__builtin_ffsl(x.val.int64)));
}
sexp bit_scan_reverse(sexp x){
  //typecheck
  return int64_sexp((int64_t)(__builtin_clzl(x.val.int64)));
}
sexp population_count(sexp x){
  //typecheck
  return int64_sexp((int64_t)__builtin_popcountl(x.val.int64));
}
#ifdef __x86_64__
sexp bit_rotate(sexp x,sexp y){
  //typecheck
  int64_t result;
  if(y.val.int64>=0){
    asm("rorq %1,%0,%2":"=g"(result):"g"(x.val.int64),
        "c"((uint8_t)(y.val.int64%64)));
  } else {
    asm("rolq %1,%0,%2":"=g"(result):"g"(x.val.int64),
        "c"((uint8_t)(labs(y.val.int64)%64)));
  }
  return uint64_sexp(result);
}
sexp bit_test(sexp x,sexp y){
  //typecheck
  uint8_t offset=y.val.uint8;
  asm("btq %[bits],%[offset]\n\tsetc %[offset]"
      : [offset] "+g" (offset) : [bits] "g" (x.val.uint64));
  return uint64_sexp(offset);
}
#else
sexp bit_test(sexp x,sexp y){
  uint8_t bit=y.val.uint8;
  uint64_t val=x.val.uint64;
  //I don't really know a better way to do this
  //I mean (val&(1<<bit)?1:0) would work too, but this is cooler
  return uint64_sexp(!!(val & (1<<bit)));
}
sexp bit_rotate(sexp a,sexp b){
  //this is a bit tricky
  union {
    uint32_t int32[2];
    uint64_t int64;
  } x,y,z;
  x.int64=y.int64=z.int64=0;
  int rot_cnt=b.val.int64%64;
  //need to put bigendian code here
  if(rot_cnt>=0){
    if(rot_cnt>=32){
      //swap low and high 32 bit chunks
      x.int32[1]=a.val.uint64<<32;//low bits(initally)
      y.int32[1]=a.val.uint64>>32;//high bits(initally)
      rot_cnt-=32;
    } else {
      x.int32[1]=a.val.uint64>>32;//high bits
      y.int32[1]=a.val.uint64<<32;//low bits
    }
    x.int64>>rot_cnt;
    y.int64>>rot_cnt;
    z.int32[1]=x.int32[1]|y.int32[0];//high
    z.int32[0]=x.int32[0]|y.int32[1];//low
  } else {
    rot_cnt=labs(rot_cnt);
    if(rot_cnt>=32){
      //swap low and high 32 bit chunks
      x.int32[0]=a.val.uint64>>32;//high bits(initally)
      y.int32[0]=a.val.uint64<<32;//low bits(initally)
      rot_cnt-=32;
    } else {
      x.int32[0]=a.val.uint64<<32;//low bits
      y.int32[0]=a.val.uint64>>32;//high bits
    }
    x.int64<<rot_cnt;
    y.int64<<rot_cnt;
    z.int32[1]=x.int32[1]|y.int32[0];//high
    z.int32[0]=x.int32[0]|y.int32[1];//low
  }
  return uint64_sexp(z.int64);
}
    
#endif
