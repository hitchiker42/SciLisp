#include "common.h"
#include "prim.h"
#include "env.h"
/*#include <sys/mman.h>
#define SIZE 128
#define GTHRESH 0.75
obarray* init_prim_obarray(){
  obarray *ob=xmalloc(sizeof(obarray));
  ob->buckets=xmalloc(sizeof(obarray_entry*)*SIZE);
  int i,j;
  for(i=0;i<size;i++){
    ob->buckets=xmalloc(10*sizeof(obarray_entry));
    for(j=0;j<10;j++){
      ob->buckets[j]->ob_symbol=xmalloc(sizeof(symbol));
    }
  }
  *ob={.buckets=ob->buckets,.size=SIZE,.used=0,.entries=0,.capacity=0.0,
       .capacity_inc=(1.0/(SIZE*10)),.gthresh=GTHRESH,.gfactor=2,
       .is_weak_hash=0,.hash_fn=fnv_hash};
       }*/
static int bucket_indices[SIZE]={0};
void add_entry_prim_obarray(obarray* ob,symref entry){
  uint64_t hashv=ob->hash_fn(CORD_as_cstring(entry->name),CORD_len(entry->name));
  uint64_t index=hashv%SIZE;
  //this only works because I have the bucket indices in an array
  //and all memory is already statically allocated, this is expensive but 
  //since I write this to afile and read it back it's worth it
  uint64_t prev=(index==0 ? 0 : ob->buckets[index][bucket_indices[index-1]]);
  *ob->buckets[index][bucket_indices[index]]=
    (obarray_entry){.prev=prev,.next=ob->buckets[index][bucket_indices[index+1]],
                    .ob_symbol=entry,.hashv=hashv};
  bucket_indices[index]++;
  }
/*
  much more complicated than I thought
  void write_obarray_prims(){
  int primFile=open("prim.dat",O_RDWR | O_CREAT);
  if(primFile == -1){
    perror("errror opening file using open");
    exit(1);
  }
  size_t page_size=(size_t) sysconf(_SC_PAGESIZE);
  size_t needed_size=(sizeof(obarray)+
                      (SIZE*sizeof(obarray_entry*))+
                      (SIZE*10*sizeof(obarray_entry))+(SIZE*10*sizeof(symbol)));
  needed_size+=(page_size-(needed_size%page_size));//align to page boundry  
  obarray *ob=mmap(NULL,needed_size,PROT_READ|PROT_WRITE,MAP_SHARED,primFile,0)
  }*/
int main(){
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
  sexp hello=cord_sexp("Hello, World!");
  symref print_sym=getSym(&ob_env,"print");
  if(print_sym){
    if(print_sym->val.tag == _fun){
      print_sym->val.val.fun->fun.comp.f1(hello);
    }
  }
}
