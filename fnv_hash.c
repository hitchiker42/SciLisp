#include <stdint.h>
#include <stdio.h>
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
  if(!argv[1]){
    return 1;
  }
  char* data=argv[1];
  int i=1,len;
  uint64_t hashv;
  while(data){
    len=strlen(data);
    hashv=fnv_hash(data,len);
    printf("%#0lx ",hashv);
    data=argv[++i];
  }
  return 0;
}
