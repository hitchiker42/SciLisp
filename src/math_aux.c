#include "common.h"
uint8_t max_num_type(sexp *args, int numargs){
  uint8_t max_type=0;
  int i=0;
  while (i<numargs){
    max_type=MAX(max_type,args[i++].tag);
  }
  return max_type;
}
