#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define offset_basis_64 14695981039346656037UL
#define fnv_prime_64 1099511628211UL
uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
int main(int argc,char* argv[]){
  int interactive=0;
  if(!argv[1]){
    return 1;
  } else if (argv[1][0] == '-'){
    if (!(strcmp(argv[1],"-i"))){
      interactive=1;
      argv=argv+1;
    }
  }
  char* data=argv[1];
  int i=1,len;
  uint64_t hashv;
  while(data){
    len=strlen(data);
    hashv=fnv_hash(data,len);
    printf("%#0lx ",hashv);
    if(interactive){
      printf("\n%d\n",hashv%128);
    }
    data=argv[++i];
  }
  return 0;
}
