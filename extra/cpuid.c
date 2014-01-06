#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdint.h>
#if defined(__x86_64__)
typedef uint64_t word;
#else
typedef uint32_t word;
#endif
typedef union reg reg;
struct flags {
  uint8_t mmx;
  uint8_t fxsave;
  uint8_t sse;
  uint8_t sse2;
  uint8_t sse3;
  uint8_t ssse3;
  uint8_t sse4_1;
  uint8_t sse4_2;
  uint8_t aes;
  uint8_t osxsave;
  uint8_t avx;
  uint8_t fma;
  uint8_t avx2;
};
union reg{
  word name;
  uint8_t bytes[sizeof(word)];
};
void cpuid(volatile word eax_input,volatile word ecx_input,
           volatile word *eax,volatile word *ebx,
           volatile word *ecx,volatile word *edx){
    __asm__ volatile (
        "cpuid;"
        :"=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
        :"0" (eax_input), "2" (ecx_input)
    );
}
struct flags *get_flags(){
  //unsigned int eax,ebx,ecx,edx;
  volatile reg eax,ebx,ecx,edx;
  cpuid(0x1,0x0,&eax.name,&ebx.name,&ecx.name,&edx.name);
  //w/lsb=31->msb=0
  //in  ecx:
  /*    unsigned int 
    fma=0x1000,//bit 12=fma
    avx=0x10000000,//bit 28=avx
    // if avx supported; cpuid eax=07h,ecx=0h check bit 5 of ebx
    osxsave=0xc000000,//  bit 27=osxsave, operating system ymm support
p    xsave=0x4000000,// bit 26=xsave, hardware ymm support
    aes=0x2000000,//bit 25=aes
    sse4_2=0x100000,//bit 20=sse4_2
    sse4_1=0xc0000,//bit 19=sse4_1
    ssse3=0x200,//bit 9=ssse3
    sse3=0x1,//bit 0=sse3
    //edx:
    sse2=0x4000000,//bit 26=sse2
    sse=0x2000000,//bit 25=sse
    fxsave=0x1000000,//bit 24=fxsave(check before any sse stuff)
    mmx=0x800000;//bit 23=mmx
  unsigned int simd[13]={mmx,fxsave,sse,sse2,sse3,ssse3,sse4_1,sse4_2,
  aes,osxsave,avx,fma};*/
  //flags = mmx,fxsave,sse,sse2,sse3,ssse3,sse4.1,sse4.2,aes,osxsave,avx,fma,avx2
  volatile struct flags *flags=malloc(sizeof(struct flags));
  *flags=(struct flags)
    {.mmx=0x80,.fxsave=0x1,.sse=0x2,.sse2=0x4,.sse3=0x1,
     .ssse3=0x2,.sse4_1=0x8,.sse4_2=0x16,.aes=0x2,.osxsave=0x8,
     .avx=0x10,.fma=0x10,.avx2=0x20};
  flags->mmx&=edx.bytes[2];
  flags->fxsave&=edx.bytes[3];
  flags->sse&=edx.bytes[3];
  flags->sse2&=edx.bytes[3];
  flags->sse3&=ecx.bytes[0];
  flags->ssse3&=ecx.bytes[1];
  flags->sse4_1&=ecx.bytes[2];
  flags->sse4_2&=ecx.bytes[2];
  flags->aes&=ecx.bytes[3];
  flags->osxsave&=ecx.bytes[3];
  flags->avx&=ecx.bytes[3];
  flags->fma&=ecx.bytes[1];  
  cpuid(0x7,0x0,&eax.name,&ebx.name,&ecx.name,&edx.name);
  flags->avx2&=ebx.bytes[0];
  return (struct flags*)flags;
}
void print_features(struct flags *flags){
  printf("mmx: %hhx, fxsave: %hhx, sse: %hhx, sse2:%hhx\n"
         "sse3: %hhx, ssse3: %hhx, sse4.1: %hhx, sse4.2: %hhx\n"
         "aes: %hhx, osxsave: %hhx, avx: %hhx, fma: %hhx, avx2: %hhx\n",
         flags->mmx,flags->fxsave,flags->sse,flags->sse2,flags->sse3,
         flags->ssse3,flags->sse4_1,flags->sse4_2,flags->aes,flags->osxsave,
         flags->avx,flags->fma,flags->avx2);
}
void print_cpuid(){
  volatile word eax, ebx, ecx, edx;
  int i;
  for (i = 0; i < 8; ++i){
    cpuid(i,0x0, &eax, &ebx, &ecx, &edx);
    printf("eax=%i: %#010x %#010x %#010x %#010x\n", i, eax, ebx, ecx, edx);
  }
}
char* latest_simd(struct flags *flags){
  if(flags->avx2){
    return "avx2";
  } else if (flags->avx){
    return "avx";
  } else if (flags->sse4_2){
    return "sse4.2";
  } else if (flags->sse4_1){
    return "sse4.1";
  } else if (flags->ssse3){
    return "ssse3";
  } else if (flags->sse3){
    return "sse3";
  } else {
    //sse2 and below avialible on all 64 bit processors
    return "sse2";
  }
}
int main(int argc,char *argv[]){
  struct flags *flags=malloc(sizeof(struct flags));
  flags=get_flags();
  if(argv[1] && argv[1][0] == '-'){
    if (argv[1][1]  == 'l'){
      char *retval=latest_simd(flags);
      printf("%s",retval);
      return(EXIT_SUCCESS);
    }
  }
  print_cpuid();
  print_features(flags);
  return 0;
}
