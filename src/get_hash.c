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
#if defined(__x86_64__) && defined(CITY_HASH)
#include "city_hash.h"
#include "city_hash.c"
#define hash_fn CityHash64
const char *hash_name="City Hash";
#else
#define hash_fn fnv_hash
const char *hash_name="FNV Hash";
#endif
#define print_help()                                             \
  printf("get_hash -ihv [string]*\n"                             \
         "\t Using hash function %s\n"                           \
         "\t-h help, print this help and exit\n"                 \
         "\t-i interactive, read lines from stdin\n"             \
         "\t-v verbose, print extra information\n", hash_name)
int main(int argc,char* argv[]){
  int interactive=0,verbose=0;  
  if(!argv[1]){
    print_help();
    return 1;
  } else if (argv[1][0] == '-'){
    if (!(strcmp(argv[1],"-i"))){
      interactive=1;
      argv=argv+1;
    } else if (!strcmp(argv[1],"-v")){
      verbose=1;
      argv=argv+1;
    } else if (!strcmp(argv[1],"-h")){
      return(EXIT_SUCCESS);
    }  
  }
  uint64_t hashv;
  char *data;
  int len;
  if(interactive){
    size_t n=0;
    while((getline(&data,&n,stdin))){
      len=strlen(data);
      hashv=hash_fn(data,len);
      printf("%#.16lx\n",hashv);
    }
    return 0;
  } else {
    int i=1;
    while((data=argv[i++])){
      len=strlen(data);
      hashv=hash_fn(data,len);
      printf("%#.16lx ",hashv);
      if(verbose){
        puts(data);
        printf("bucket number %d/128\n",hashv%128);
      }
  }
  return 0;
  }
}
