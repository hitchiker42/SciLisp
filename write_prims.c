#include "common.h"
#include "prim.h"
int main(){
  FILE* primsFile=fopen("prims.dat","w");
  obarray* ob=obarray_init(64,0.75);
  env ob_env={.enclosing=NULL,.head={.ob=ob},.tag=_obEnv};
  initPrimsObarray(ob,&ob_env);
  int i,bsize=0;
  obarray_entry* ent;
  for(i=0;i<ob->size;i++){
    printf("bucket %d:",i);
    if(!ob->buckets[i]){
      printf("\n");
    } else {
      ent=ob->buckets[i];
      while(ent){
        bsize++;
        ent=ent->next;
        printf("-");
      }
      //      fprintf(stderr,"bucket %d has %d entries\n",i,bsize);
      printf("\n");
      bsize=0;
    }
  }
  printf("Obarray statistics:\nsize:%d\nused:%d\nentries:%d\ncapacity:%f\n",
         ob->size,ob->used,ob->entries,ob->capacity);
}
