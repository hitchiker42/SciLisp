#include <stdint.h>
typedef uint64_t size_t;
uint32_t *__attribute__((leaf))
  memcpy_stride_32(register uint32_t *dest,register const uint32_t *src,size_t size,size_t stride){
  //save return value
  uint32_t *retval=dest;
  if(stride==2){//optimize the most common case;
    //make size a multiple of 4
    if(size&1){//if the first bit is set
      *dest++=*src;
      src+=2;
    }
    if(size&2){//if the second bit is set
      *dest++=*src;
      src+=2;
      *dest++=*src;
      src+=2;
    }
    size&=~3;
    register uint32_t a,b,c,d;
    while(size){
      a=*src;
      src+=2;
      b=*src;
      src+=2;
      c=*src;
      src+=2;
      d=*src++;
      src+=2

      *dest++=a;
      *dest++=b;
      *dest++=c;
      *dest++=d;
    
      if(!(size-=4)){
        break;
      }
      a=*src;
      src+=2;
      b=*src;
      src+=2;
      c=*src;
      src+=2;
      d=*src++;
      src+=2

      *dest++=a;
      *dest++=b;
      *dest++=c;
      *dest++=d;
      
      size-=4;
    }
    return retval;
  }
  //make size a multiple of 4
  if(size&1){//if the first bit is set
    *dest++=*src;
    src+=stride;
  }
  if(size&2){//if the second bit is set
    *dest++=*src;
    src+=stride;
    *dest++=*src;
    src+=stride;
  }
  size&=~3;
  register uint32_t a,b,c,d;
  while(size){
    a=*src;
    src+=stride;
    b=*src++;
    src+=stride;
    c=*src++;
    src+=stride;
    d=*src++;

    *dest++=a;
    *dest++=b;
    *dest++=c;
    *dest++=d;
    
    if(!(size-=4)){
      break;
    }

    a=*src++;
    src+=stride;
    b=*src++;
    src+=stride;
    c=*src++;
    src+=stride;
    d=*src++;
    src+=stride;

    *dest++=a;
    *dest++=b;
    *dest++=c;
    *dest++=d;

    size-=4;
  }
  return retval;
}
