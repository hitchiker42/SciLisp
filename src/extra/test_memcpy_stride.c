#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
extern void* memcpy_stride(void*,void*,size_t,size_t);
#define HERE() (fprintf(stderr,"here at line %d\n",__LINE__))

int main(){
  double *dbl_array=malloc(1000*sizeof(double));
  uint64_t *uint64_array=malloc(1000*sizeof(double));
  int i;
  for(i=0;i<1000;i++){
    dbl_array[i]=i*i;
    uint64_array[i]=i;
  }
  double *dbl_dest_1=malloc(500*sizeof(double));
  double *dbl_dest_2=malloc(250*sizeof(double));
  uint64_t *uint64_dest_1=malloc(500*sizeof(uint64_t));
  uint64_t *uint64_dest_2=malloc(250*sizeof(uint64_t));
  HERE();
  memcpy_stride(dbl_dest_1,dbl_array,500,2);
  HERE();
  memcpy_stride(dbl_dest_2,dbl_array,250,4);
  HERE();
  uint64_dest_1=memcpy_stride(uint64_dest_1,uint64_array,500,2);
  HERE();
  uint64_dest_2=memcpy_stride(uint64_dest_2,uint64_array,250,4);
  for(i=2;i<250;i*=2){
    printf("dbl_array[%d] = %lf\nuint64_array[%d] = %lu\n",
           i,dbl_array[i],i,uint64_array[i]);
    printf("dbl_dest_1[%d] = %lf\ndbl_dest_2[%d] = %lf\n",
           i,dbl_dest_1[i],i,dbl_dest_2[i]);
    printf("uint64_dest_1[%d] = %lu\nuint64_dest_2[%d] = %lu\n",
           i,uint64_dest_1[i],i,uint64_dest_2[i]);
  }
  return 0;
}
