#include "common.h"
//gmp functions work by side effect, so hide that fact in a macro
#define gmp_wrapper(function,args...)           \
  mpz_ptr gmp_temp=xmalloc(sizeof(mpz_t));      \
  mpz_init(*gmp_temp);                          \
  function(*gmp_temp,##args);                   \
  return gmp_temp
