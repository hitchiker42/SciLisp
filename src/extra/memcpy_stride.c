#include <stdint.h>
#include <stdlib.h> //for size_t
uint64_t *__attribute__((leaf))
  memcpy_stride(register uint64_t *dest,register const uint64_t *src,size_t size,size_t stride){
  register uint64_t a,b,c,d;
  uint64_t *retval=dest;
  uint32_t i=0,j=0;
  if(stride==2){
    goto stride2;
  }
  if(size&1){
    a=*src;
    *dest++=a;
    src+=stride;
    
  }
  if(size&2){
    a=*src;
    src+=stride;
    b=*src;
    src+=stride;
    
    *dest++=a;
    *dest++=b;
  }
  size&=(size_t)-4;
  size>>=2;
  while(size){
    a=*src;
    src+=stride;
    b=*src;
    src+=stride;
    c=*src;
    src+=stride;

    *dest++=a;
    *dest++=b;
    *dest++=c;
    *dest++=d;
    size--;
  }
  return retval;

 stride2:
  if(size&1){
    a=*src;
    *dest++=a;
    src+=2;
  }
  if(size&2){
    a=*src;
    b=*(src+2);    
    *dest++=a;
    *dest++=b;
    src+=4;
  }
  size&=(size_t)-4;
  size>>=4;
  while(size){
    a=*src;
    b=*(src+2);
    c=*(src+4);
    d=*(src+6);

    *dest++=a;
    *dest++=b;
    *dest++=c;
    *dest++=d;
    
    size--;
  }
  return retval;
}
