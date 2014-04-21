#ifndef _SCILISP_ATOMICS_H
#define _SCILISP_ATOMICS_H
/*
  SciLisp assumes a 64 bit architecture so all atomic operations are defined
  for 64 bit unsigned integers, which should corrspond to the type of pointers
  
  hopefully I'll add support for atomic operations on doubles as well
  granted this is much more complicated since there is no hardware support
  for atomic operations on floats
 */
//C11 atomic operations are unsupported as they require special types and 
//so can't be eaisly intergated into existing code
#include <stdint.h>
//I don't know where else to put these
//prefetch0-2 do similar things but the number denotes the 
//importance of the data, prefetch_0 is more likely to 
//fetch data into the 1st level cache then prefetch_2
#define prefetch_0(addr)                        \
  __builtin_prefetch(addr,0,3)
#define prefetch_1(addr)                        \
  __builtin_prefetch(addr,0,2)
#define prefetch_2(addr)                        \
  __builtin_prefetch(addr,0,1)
//fetch data that you know won't need to be used more then once
#define prefetch_ntq(addr)                      \
  __builtin_prefetch(addr,0,0)
static uint64_t atomic_inc(uint64_t *addr);
static uint64_t atomic_dec(uint64_t *addr);

#define ATOMIC_FULL __ATOMIC_SEQ_CST
#define ATOMIC_STORE __ATOMIC_ACQUIRE
#define ATOMIC_LOAD __ATOMIC_RELEASE
#define ATOMIC_LOAD_STORE __ATOMIC_ACQ_REL
//use macros instead of inline functions since the gcc bultins
//work on any types
//Give the simplest names to the macros with the strongest memory
//semantics, because it's probably what I'll use everytime
#define atomic_add(addr,val)                                \
  __atomic_add_fetch(addr,val,ATOMIC_FULL)
#define atomic_sub(addr,val)
  __atomic_sub_fetch(addr,val,ATOMIC_FULL)
#define atomic_or(addr,val)
  __atomic_or_fetch(addr,val,ATOMIC_FULL)
#define atomic_xor(addr,val)
  __atomic_xor_fetch(addr,val,ATOMIC_FULL)
#define atomic_and(addr,val)
  __atomic_and_fetch(addr,val,ATOMIC_FULL)
#define atomic_nand(addr,val)
  __atomic_nand_fetch(addr,val,ATOMIC_FULL)
#define atomic_fetch_add(addr,val)                      \
  __atomic_fetch_add(addr,val,ATOMIC_FULL)
//gcc is smart enough to use xadd for atomic_fetch_sub
#define atomic_fetch_sub(addr,val)                      \
  __atomic_fetch_sub(addr,val,ATOMIC_FULL)

#define atomic_store_ptr(addr1,addr2)               \
  __atomic_store(addr1,addr2,ATOMIC_FULL)
#define atomic_store(addr1,val)               \
  __atomic_store_n(addr1,val,ATOMIC_FULL)
#define atomic_load_ptr(addr1,addr2)               \
  __atomic_load(addr1,addr2,ATOMIC_FULL)
#define atomic_load(addr1,val)               \
  __atomic_load_n(addr1,val,ATOMIC_FULL)
#define atomic_compare_exchange(addr,expected,desired)                  \
  __atomic_compare_exchange(addr,expected,desired,ATOMIC_FULL,ATOMIC_FULL)
#define atomic_compare_exchange_n(addr,expected,desired)                \
  __atomic_compare_exchange_n(addr,expected,desired,ATOMIC_FULL,ATOMIC_FULL)
#define atomic_swap(addr1,addr2)                        \
  __atomic_exchange(addr1,addr2,addr2,ATOMIC_FULL)
#define atomic_exchange(addr,val,ret)           \
  __atomic_exchange(addr,val,ret,ATOMIC_FULL)
#define atomic_exchange_n(addr,val)           \
    ({__atomic_exchange_n(addr,val,ATOMIC_FULL);})
//shouldn't need to used this
#define atomic_mfence()                         \
  __atomic_thread_fence(ATOMIC_FULL)

//these are special since gcc doesn't actually provide 
//atomic inc/dec operatinos
#ifdef __x86_64__
//stolen from glibc, generics in c, lol
#define atomic_val_bysize(pre, post, ...)                        \
  ({__typeof (*mem) __atg1_result;                               \
    if (sizeof (*mem) == 1){                                     \
      __atg1_result = pre##_8##post (mem, __VA_ARGS__);          \
    } else if (sizeof (*mem) == 2){                              \
      __atg1_result = pre##_16##post (mem, __VA_ARGS__);         \
    } else if (sizeof (*mem) == 4){                              \
      __atg1_result = pre##_32##post (mem, __VA_ARGS__);         \
    } else if (sizeof (*mem) == 8){                              \
      __atg1_result = pre##_64##post (mem, __VA_ARGS__);         \
    } else {                                                     \
      abort ();                                                  \
    }                                                            \
    __atg1_result;})
static inline uint64_t atomic_inc_64(uint64_t *addr){
  __asm__ ("lock incq %0,$1" : "+m" (addr));
  return *addr;
  }
static inline uint32_t atomic_inc_32(uint32_t *addr){
  __asm__ ("lock incl %0,$1" : "+m" (addr));
  return *addr;
}
static inline uint32_t atomic_inc_16(uint32_t *addr){
  __asm__ ("lock inc %0,$1" : "+m" (addr));
  return *addr;
}
static inline uint32_t atomic_inc_8(uint32_t *addr){
  __asm__ ("lock incb %0,$1" : "+m" (addr));
  return *addr;
}
static inline uint64_t atomic_dec_64(uint64_t *addr){
  __asm__ ("lock decq %0,$1" : "+m" (addr));
  return *addr;
}
static inline uint32_t atomic_dec_32(uint32_t *addr){
  __asm__ ("lock decl %0,$1" : "+m" (addr));
  return *addr;
}
static inline uint32_t atomic_dec_16(uint32_t *addr){
  __asm__ ("lock dec %0,$1" : "+m" (addr));
  return *addr;
}
static inline uint32_t atomic_dec_8(uint32_t *addr){
  __asm__ ("lock decb %0,$1" : "+m" (addr));
  return *addr;
}
#define atomic_inc(addr)                        \
  atomic_val_bysize(atomic_inc,,addr)
#define atomic_dec(addr)                        \
  atomic_val_bysize(atomic_dec,,addr)

#define PAUSE volatile __asm__("pause\n")
#else
#define atomic_inc(addr)                        \
  __atomic_add_fetch(addr,1,ATOMIC_FULL)
#define atomic_dec(addr)                        \
  __atomic_add_fetch(addr,-1,ATOMIC_FULL)
#define PAUSE 
#endif

static inline void atomic_store(uint64_t *addr,uint64_t val){
  __atomic_store_n(addr,val,ATOMIC_FULL);
}
static inline void atomic_store_ptr(uint64_t *addr,uint64_t *val){
  __atomic_store(addr,val,ATOMIC_FULL);
}
//incase I decide to use the libatomic ops later
#if 0
#if (defined USE_GCC_ATOMICS)
#else
//using libatomic_opts
#include "atomic_opts.h"
static inline uint64_t atomic_inc(uint64_t *addr){
  return (uint64_t)AO_fetch_and_add1_full(addr):
}
static inline uint64_t atomic_dec(uint64_t *addr){
  return (uint64_t)AO_fetch_and_sub1_full(addr):
}
//static void atomic_store(uint64_t *addr,uint64_t val) __attribute__((alias("AO_store_full")))
static inline void atomic_store(uint64_t *addr,uint64_t val){
  AO_store_full(addr,val);
}
static inline void atomic_store_ptr(uint64_t *addr,uint64_t *val){
  AO_store_full(addr,*val);
}
static inline int compare_and_swap(uint64_t *addr,uint64_t old_val,uint64_t new_val){
  return AO_compare_and_swap_full(addr,old_val,new_val);
}
#endif
#ifdef 0
//this is probably eaiser to write as straight assembly
//i.e
/*
  fetch_and_add_double:
  movq %rax,%rdx #save address in rdx
  movq (%rax),%rax
  movl $3,%ecx;
  L(loop);
  addsd %(rdx),%xmm0
  movsd %xmmo,%rbx
  lock; cmpxchg %rbx,(%rdx)
  loopne %L(loop)
  setz %rax
  ret
*/

#ifndef __x86_64__
static inline double fetch_and_add_double(double *addr_,register double inc){
  double initial_val[1] = {*addr},new_val=0;
  do {
    new_val=inc+initial_val[0];
  } while {
    !atomic_compare_exchange_n((uint64_t*)addr,(uint64_t*)initial_val,
                               (uint64_t*)&new_val);
  }
    
    
#else
static inline double fetch_and_add_double(double *addr,register double val){
  uint8_t result,i;
  __asm__ volatile("lea %[old],%%rax\n\t"//copy initial value to rax
                   "movl $3,%%rcx\n\t"//clear index register
                   "L(loop): "
                   "addsd %[add_src],%[add_dest]\n\t"//floating point add
                   "movsd %[add_dest] %%rbx\n\t"//copy new value to some register new
                   "lock; cmpxchg %[old],%%rbx\n\t"//cmpxchg uses rax implicitly
                   "loopne $L(loop)\n\t"//loop if xchg failed or ecx >0
                   "L(end):\n\t setz %[result]"//set if xchg successful
                   : [result] "=g" (result),[add_dest] "+x" (val)
                   : [old] "m" (addr)
                   : "rax","rbx","rcx");
}
#endif
#endif
