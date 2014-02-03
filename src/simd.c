/* Simd intrinsics for x86_64 and emulated versions for everything else

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

/*
  simd floating point map-reduce, with sequential reduce
  xmm1 accumulator;
  xmm2 working_register
  
  if we have and initial value
    load it into xmm1
  else 
    load the first value in array into xmm1
    set array to array+1
  endif
  int width=N,index,i;//where N=2/4/8 depending on floatingpt width & vector width
  for(index=0;index<length;index+=width){
    load(array,xmm2);//load should only use the next width elements
    perform map operation on xmm2
    for(i=0;i<with;i++){
    perform reduce operation on xmm1,xmm2;
    shift xmm1 right by 32/64 bits depending;
    }
    shift xmm1 left by width-(32/64 bits);
   }
   //for excess elements
   load single element into xmm2
   perform sigular version of reduce op on xmm2,xmm1

   zero out leftmost bits of xmm1 before returning;

   core loop is effectively(assuming 4x32bit elements)
   
      op xmm2 # whatever op is used for mapping
      mov $4,rax;
LABEL op2 xmm2,xmm1;
      psrldq $4,xmm1;
      dec rax;
      jnz LABEL

*/
/*so many macros, I did this in sml using a lot of parameterized functions
  shame I can't really do that in c, well not as eaisly anyway. (i mean I can 
  paramterize things by function pointers, but then I'm bound to specific types)*/
//return a pointer to the first aligned location in the array
//if the return value != arr then the initial unaligned bits need to be
//delt with seperately
#define AVX_ALIGNED(arr)                        \
  (arr+(arr%32))
#define SSE_ALIGNED(arr)                        \
  (arr+(arr%16))
#define MK_SHUFFLE_CONST_32(a,b,c,d)            \
  (a|b<<2|c<<4|d<<6)
#define MK_SHUFFLE_CONST_64_AVX(a,b,c,d)            \
  (a|b<<1|c<<2|d<<3)
#define MK_SHUFFLE_CONST_64(a,b)            \
  (a|b<<1)
//input of |128a|128b| output |000...|128a| (or ifdef AVX2 |128b|128a|
//needed when reducing a 256 bit simd vector
__m256 AVX_rsh_128(__m256 simd){
#ifdef __AVX2__
  return _mm256_permute2f128ps(simd,simd,0b00000001);
#else
  uint64_t temp[6];//2 extra bytes to insure loading temp+2 works
  //assume statck storage isn't 32 byte aligned(it generally wouldn't be)
  _mm256_storeu_si256((__m256i*)temp,(__m256i)simd);
  //I'm pretty sure a cast from __m256 to __m256i is just a bitwise cast
  return (__m256)_mm256_loadu_si256(temp+2);
}
//just an example using doubles & avx
void avx_double_arr_map_destructive(double *arr,size_t len,
                                    double(*f_scalar)(double),
                                    __m256d(*f_simd)(__m256d)){
  size_t index=0;
  size_t offset=arr%32;
  register __m256d simd;
  if(!offset % 8){//it would be hard to do this but you could
    goto UNALIGNED;
  }  
  while(index<(offset/8)){//this could be done probably be done better
    arr[index]=f_scalar(arr[index]);
    index++;
  }
 ALIGNED:
  for(;index<len;index+=4){
    simd=_mm256_load_pd(arr+index);
    simd=f_simd(simd);
    _mm256_store_pd(arr+index,simd); 
  }
  goto EXCESS;
 UNALIGNED:
  for(;index<len;index+=4){
    simd=_mm256_loadu_pd(arr+index);
    simd=f_simd(simd);
    _mm256_storeu_pd((_m256d*)arr+index,simd);
  }
 EXCESS:
  if(index>len){
    index-=4;
    for(index<len;index++){
      arr[index]=f_scalar(arr[index]);
    }
  }
  return;
}
//macros to translate infix operations to prefix operations
#define add(a,b) (a+b)
#define sub(a,b) (a-b)
#define mul(a,b) (a*b)
#define div(a,b) (a/b)
#define lsh(a,b) (a<<b)
#define rsh(a,b) (a>>b)
#define and(a,b) (a&b)
#define ior(a,b) (a|b)
#define xor(a,b) (a^b)
#define cmp_eq(a,b) (a==b)
#define cmp_ne(a,b) (a!=b)
#define cmp_gt(a,b) (a>b)
#define cmp_lt(a,b) (a<b)
#define cmp_ge(a,b) (a>=b)
#define cmp_le(a,b) (a<=b)
#define cmp_ngt(a,b) (a<=b)
#define cmp_nlt(a,b) (a>=b)
#define cmp_nge(a,b) (a<b)
#define cmp_nle(a,b) (a>b)

#define simd_n_emulated_horozintal_op(name,fun,type,elements,size)      \
  simd1##size##_##type simd##size##_##type##_##name(                    \
    simd##size##_##type a,simd##size##_##type b){                       \
    simd##size##_##type c;                                              \
    int i,j;                                                            \
    int midpt=elements/2;                                               \
    for(i=0,j=0;i<elemnts;i+=2,j++){                                    \
      c.type##_vec[j]=fun(a.type##_vec[i],a.type##_vec[i+1]);           \
      c.type##_vec[j+midpt]=fun(b.type##_vec[i],b.type##_vec[i+1]);     \
    }                                                                   \
    return c;                                                           \
  }

#define simd_n_emulated_binop(name,fun,type,elements,size)              \
  simd1##size##_##type simd##size##_##type##_##name(                    \
    simd##size##_##type a,simd##size##_##type b){                       \
    simd##size##_##type c;                                              \
    int i;                                                              \
    for(i=0;i<elements;i++){                                            \
      c.type##_vec[i]=fun(a.type##_vec[i],b.type##_vec[i]);             \
    }                                                                   \
    return c;                                                           \
  }
#define simd128_emulated_binop(name,type,elements)      \
  simd_n_emulated_binop(name,type,elements,128)
#define simd256_emulated_binop(name,type,elements)      \
  simd_n_emulated_binop(name,type,elements,256)

#define do_simd_types(macro,name,fun,size)                          \
  macro(name,fun,real32,(size/sizeof(real32_t));                    \
  macro(name,fun,real64,(size/sizeof(real64_t));                    \
  macro(name,fun,int8,(size/sizeof(int8_t));                        \
  macro(name,fun,int16,(size/sizeof(int16_t));                      \
  macro(name,fun,int32,(size/sizeof(int32_t));                      \
  macro(name,fun,int64,(size/sizeof(int64_t))
#define do_simd_types_emulated_binop(name,size)         \
  do_simd_types(simd_n_emulated_binop,name,name,size)
#define simd128_emulated_unop(name,fun,type,size)                       \
  simd128_##type simd128_##type##_##name(simd128_##type a){             \
    simd128_##type b;                                                   \
    int i;                                                              \
    for(i=0;i<size;i++){                                                \
      b[i]=fun(a[i]);                                                   \
    }                                                                   \
    return c;                                                           \
  }
#define simd_n_binop(name,fun,type,size)                                \
  simd_##size##_##type simd_##size##_##type##_##name(                   \
    simd_##size##_##type a,simd_##size##_##type b){                     \
    return fun(a,b);                                                    \
  }
#define simd128_binop(name,fun,type)            \
  simd_n_binop(name,fun,type,128)
#define simd256_binop(name,fun,type)            \
  simd_n_binop(name,fun,type,128)
#define simd_n_int_binops(name,fun,size,byte,word,dblword,quadword)     \
  simd_n_binop_##byte(name,fun##8,int8,size);                           \
  simd_n_binop_##word(name,fun##16,int16,size);                         \
  simd_n_binop_##dblword(name,fun##32,int32,size);                      \
  simd_n_binop_##quadword(name,fun##64,int64,size)
#define simd_n_binop_t(name,fun,type,size)              \
  simd_n_binop(name,fun,type,size)
#define simd_n_binop_f(name,fun,type,size)

#define simd_n_real_binops(name,fun,size)       \
  simd_n_binop(name,fun##_ps,real32,size);      \
  simd_n_binop(name,fun##_pd,real64,size)
#ifndef __x86_64__
do_simd128_types_emulated_binop(add);
do_simd128_types_emulated_binop(sub);
do_simd128_types_emulated_binop(mul);
do_simd128_types_emulated_binop(div);
#else
simd_n_real_binops(add,_mm_add,128)
simd_n_real_binops(sub,_mm_sub,128)
simd_n_real_binops(mul,_mm_mul,128)
simd_n_real_binops(div,_mm_div,128)
simd_n_int_binops(add,_mm_add_epi,128,t,t,t,f);
simd_n_int_binops(adds,_mm_adds_epi,128,t,t,f,f);
simd_n_int_binops(adds,_mm_adds_epu,128,t,t,f,f);
#ifdef __SSE3__
#ifdef __AVX__
#ifdef __AVX2__
#endif/*__SSE3__*/
#endif/*__AVX__*/
#endif/*__AVX2__*/
#endif/*__x86_64__*/
