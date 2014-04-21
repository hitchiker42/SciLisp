typedef struct binary_heap priority_queue;
typedef struct binary_heap binary_heap;
//the only thing the heap cares about is the priority
struct heap_obj {
  uint64_t priority;
  uint8_t obj[];
};
/*for priority use something akin to linux priority,
  the top bit of priority is only set in tasks that have explicit
  priority, thus any task with explicit priority has a higher
  priority that any task with implicit prioritp. other tasks have
  priority set based on the time they were added.
*/
struct heap {
  heap_obj **heap;
  int len;
};
/*optimal heap sizes:
   1 3 7 15 31 63 ...maybe it'd be best to use 1 indexed heaps...   
*/
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
#define heap_start 0
#define heap_end(len) len-1
binary_heap* heap_sort(binary_heap *heap);
void sift_down(binary_heap *heap,int start,int end);
void sift_up(binary_heap *heap,int index);
void heapify(binary_heap *heap);
void heap_insert(binary_heap *heap,void *new_element,int heap_index);
void* heap_pop(binary_heap *heap);
binary_heap *heap_sort(binary_heap *heap);
//use >=, because generally sift_down gets called after popping the
//heap, and it first swaps the last element to the top, using >=
//means that items that were initially above the last element
//and equal to it will stay above it
void sift_down(binary_heap *heap,int start,int end){
  int child,swap,root;
  root=start;
  while((child=heap_left_child(root)) <= end){
    swap=root;
    //is left child bigger?
    if(heap->heap[child]->priority >= heap->heap[swap]->priority)){
      swap=child;
    }
    //is right child bigger?
    if(child+1<=end && 
       heap->heap[child+1]->priority>heap->heap[swap]->priority){
      swap=child+1;
    }
    if(swap!=root){
      SWAP(heap->heap[root],heap->heap[swap]);
      root=swap;
    } else {
      return;
    }
  }
}
void sift_up(binary_heap *heap,int index){
  int parent=heap_parent(index);
  while(heap->compare_fn(heap->heap[index],heap->heap[parent])){
    SWAP(heap->heap[index],heap->heap[parent]);
    index=parent;
    parent=heap_parent(index);
  }
}
void heapify(binary_heap *heap){
  int start=heap_parent(heap_end(heap->len));
  for(;start>heap_start;start--){
    sift_down(heap,start,heap_end(heap->len));
  }
}
void heap_insert(binary_heap *heap,void *new_element,int heap_index){
  heap[heap_index]=new_element;
  sift_up(heap,heap_index);
}
void* heap_pop(binary_heap *heap){
  void *retval=*heap->heap;
  heap->heap[heap_start]=heap->heap[heap_end(heap->len)];
  sift_down(heap,heap_start,heap_end(heap->len)-1);
  return retval;
}
//works by modifying memory, but returns a value for convience
binary_heap *heap_sort(binary_heap *heap){
  int end=heap_end(heap->len);
  heapify(heap);
  while(end>0){
    SWAP(heap->heap[end],heap->heap[heap_start]);
    end--;
    sift_down(heap,heap_start,end);
  }
  return heap;
}

/* one based indexing
void* heap_pop(binary_heap *heap){
  void *retval=*heap->heap;
  heap->heap[1]=heap->heap[heap->len];
  sift_down(heap,1,heap->len-1);
  return retval;
}
//works by modifying memory, but returns a value for convience
binary_heap *heap_sort(binary_heap *heap){
  int end=heap->len;
  heapify(heap);
  while(end>1){
    SWAP(heap->heap[end],heap->heap[1]);
    end--;
    sift_down(heap,1,end);
  }
  return heap;
}

*/
//this is just here temporarly, but it kinda fits since it is a queue
//rings can either siently overwrite data, 
//or return an error(in c)/raise an error(in lisp) if data would be overwritten
//rings only exist as mutable structures, there wouldn't be much of a point
//otherwise
typedef struct ring ring_buffer;
struct ring {
  sexp *ring;
  uint32_t head;
  uint32_t tail;
  uint32_t size;
  int32_t state;//empty=-1/full=1/default=0
};
#define pow_of_2_mod(x,y) x & (y-1)
#define ring_full(ring) (ring->head==ring->tail && ring->state=1)
#define ring_empty(ring) (ring->head==ring->tail && ring->state=-1)
#define ring_empty_or_full(ring) (ring->head==ring->tail)
//assumes that ring will never be full when passed to it
//which is not really a good assumption
void ring_push(ring_buffer *ring,sexp elem){
  if(ring_empty(ring)){//if ring was empty indicate it's not anymore
    ring->state=0;
  }
  ring->ring[ring->tail++]=elem;//set element and increment tail
  ring->tail%=ring->size;//insure tail is always < size
  if(ring->head==ring->tail){//ring is full
    ring->state=1;
  }
}
sexp ring_pop(ring_buffer *ring){
  if(ring_full(ring)){
    ring->state=0;
  }
  sexp retval=ring->ring[ring->head++];
  ring->head%=ring->size;
  if(ring->head==ring->tail){//ring is either empty or full
    ring->state=-1;
  }
} 


struct queue_node {
  sexp *data;
  queue_node *next;
};
struct queue {
  queue_node *head;//(data . next)
  queue_node *tail;//(data . nil)
};
sexp atomic_queue_pop(struct queue *queue){
  queue_node *old_head;
  //exchanges pointers, which is why it can be atomic
  atomic_exchange(&queue->head,&queue->head->next,&old_head);
  return old_head->data;
}
sexp queue_pop(struct queue *queue){
  queue_node *old_head=queue->head;
  queue->head=queue->head->next;
  return old_head;
}
void atomic_queue_append_by_value(struct queue *queue, sexp val){
  sexp *val_ptr=xmalloc(sizeof(sexp));
  *val_ptr=val;
  atomic_queue_append(queue,val_ptr);
}
void atomic_queue_append(struct queue *queue,sexp *val){
  queue_node *tail_copy=xmalloc(sizeof(queue_node));
  queue_node *new_node=xmalloc(sizeof(queue_node));
  new_node={.data=val,.next=NULL};
  do {
    queue_node *old_tail =queue->tail;
    *tail_copy = *old_tail;     
    tail_copy->next=new_node;
  } while(!compare_exchange(&queue->tail,&old_tail,&tail_copy));
}
void queue_append_by_value(struct queue *queue,sexp val){
  sexp *val_ptr=xmalloc(sizeof(sexp));
  *val_ptr=val;
  queue_append(queue,val_ptr);
}
void queue_append(struct queue *queue,sexp *val){
  queue_node *new_node=xmalloc(sizeof(queue_node));
  new_node={.data=val,.next=NULL};
  queue->tail->next=new_node;
  queue->tail=queue->tail->next;
  return;
}
