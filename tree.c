#include "tree.h"
/*trees:
 * tree sexp = [pointer to tree,pointer to tree functions]
 * tree head = [(data,pointer to next cell),
 (pointer to left child, pointer to right child)]
 * tree branch = same format as tree head
 * tree leaf [(data,pointer to next cell), NIL]
 * branch w/1 child [(data,pointer to next cell),(nil|pointer,nil|pointer)]
 *
 * Trees w/Parent pointers:
 * tree node w/2 children=
 [(pointer to parent(nil for head),pointer to next cell),(data,pointer to next),
 (pointer to left child, pointer te right child)]
 * tree = [(parent pointer?),(data,pointer to next),
 [pointer to child1,...,pointer to child n]
 */
/* recursive helper function to build a tree
   the arguments are:
   mem, the location of the tree in memory
   vals, an array contaning the initial values of the tree
   start, starting index of the current subtree in mem
   end, ending index of current subtree in mem
   ind, stating index in vals of current subtree
   ind_max, ending index in vals of current subtree
   called from make tree with mem an array with stride 2
   (it's an array of lists of the form (x . (y . z)))
   and vals just a normal arary, start is 0 end is
   the length of mem, ind is len/2 and ind_max is len
 */
static inline void make_tree_acc
(cons *mem,sexp *vals,int start,int end,int ind,int ind_max){
  mem[start].car=vals[ind];//set the value of the current node
  /*if the index is 0 and the max index is < 1 we're at a leaf,
    so set the children to nil and return*/
  if(ind<1 && ind+1>=ind_max){
    mem[start].cdr=tree_NIL;
    return;
  } else {
    mem[start].cdr=cons_sexp(mem+start+1);
    //next cell holds previous value in list
    if(ind<1){//do we have any more left children
      mem[start+1].car=tree_NIL;//if not set left child to nil
    } else {//otherwise recursively build up left child
      mem[start+1].car=cons_sexp(mem+start+2);
      make_tree_acc(mem,vals,start+2,end/2,ind-1,ind);
    }
    if(ind+1>=ind_max){//do we have any more right children
      mem[start+1].cdr=tree_NIL;//if not set right to nil
    } else {//otherwise recursively build up right child
      mem[start+1].cdr=cons_sexp(mem+(end/2));
      make_tree_acc(mem,vals,start+(end/2),end,ind+1,ind_max);
    }
  }
}
//(defun make-tree (predicate &key type initial-contents))
sexp make_tree(sexp comp_fun,sexp tree_type,sexp contents){
  if(!FUN2P(comp_fun)){
    return format_type_error("make-tree","function",comp_fun.tag);
  }
  lisp_tree *new_tree=xmalloc(sizeof(lisp_tree));
  new_tree->comp_fn=comp_fun.val.fun->comp.f2;
  if(!NILP(tree_type)){
    //lookup keyword and match to tree type
  }// else {
  new_tree->tree.meta=_basic_tree;
  if(NILP(contents)){
    new_tree->tree=cons_sexp(xmalloc(sizeof(cons)*2));
    XCAR(new_tree->tree)=tree_NIL;
    XCDR(new_tree->tree)=tree_NIL;
    return tree_sexp(new_tree);
  } else {
    contents=array_from_list(contents);
    contents=array_qsort(contents,comp_fun,long_sexp(1));
    int len=contents.len;
    //this is an array of sets of 2 conses(i.e [(x . (y . z))*])
    cons *new_tree_mem=xmalloc(sizeof(cons)*2*len);
    new_tree->tree=cons_sexp(new_tree_mem);
    make_tree_acc(new_tree_mem,contents.val.array,0,len*2,len/2,len);
    return tree_sexp(new_tree);
  }
}
//access_fn is either XCADR or XCDDR, unused side is XCDDR or XCADR respectively
//this inserts new_node into either the left or right child of tree
//based on the values of access_fn and unused side
#define TREE_INSERT_GENERIC(tree,new_node,access_fn,unused_side)      \
  if(NILP(XCDR(tree))){                                               \
    XCDR(tree)=cons_sexp((cons*)(((uint8_t*)&tree)+sizeof(cons)));    \
    tree_add_node(tree,new_node,access_fn);                           \
    unused_side(tree)=tree_NIL;                                       \
  } else if (NILP(access_fn(tree))){                                  \
    tree_add_node(tree,new_node,access_fn);                           \
  } else {                                                            \
    tree=access_fn(tree);                                             \
  }
sexp basic_tree_insert(sexp tree,sexp new_node){
  sexp(*f)(sexp,sexp)=tree.val.tree->comp_fn;
  sexp node=tree.val.tree->tree;
  while(CONSP(tree)){
    if(isTrue(f(XCAR(node),new_node))){
      TREE_INSERT_GENERIC(node,new_node,XCADR,XCDDR);
    } else {
      TREE_INSERT_GENERIC(node,new_node,XCDDR,XCADR);
    }
  }
}
//(defun tree-insert (tree new))
sexp lisp_tree_insert(sexp tree,sexp new_node){
  if(!LISP_TREEP(tree)){
    return format_type_error("tree-insert","tree",tree.tag);
  }
  switch(tree.val.tree->tree.meta){
    case _basic_tree:
      return basic_tree_insert(tree,new_node);
    default:
      return error_sexp("invalid or unimplemented tree type");
  }
}
void inorder_walk(sexp node,sexp(*f)(sexp)){
  if(!NILP(node)){
    if(has_left_child(node)){
      inorder_walk(XCADR(node),f);
    }
    f(node);
    if(has_right_child(node)){
      inorder_walk(XCDDR(node),f);
    }
  }
}
static void map_tree_acc(sexp old_tree,sexp new_tree,sexp(*f)(sexp)){
  if(!NILP(old_tree)){
    if(has_child(old_tree)){
      XCDR(new_tree)=cons_sexp(new_tree.val.cons+1);
    }
    if(has_left_child(old_tree)){      
      XCADR(new_tree)=cons_sexp(xmalloc(sizeof(cons)*2));
      map_tree_acc(XCADR(old_tree),XCADR(new_tree),f);
    }
    XCAR(new_tree)=f(XCAR(old_tree));
    if(has_right_child(old_tree)){
      XCDDR(new_tree)=cons_sexp(xmalloc(sizeof(cons)*2));
      map_tree_acc(XCDDR(old_tree),XCDDR(new_tree),f);
    }
  }
}
sexp map_tree(sexp tree,sexp (*f)(sexp)){
  lisp_tree *new_tree=xmalloc(sizeof(lisp_tree));
  new_tree->comp_fn=tree.val.tree->comp_fn;
  new_tree->tree=cons_sexp(xmalloc(sizeof(cons)*2));
  XCDR(new_tree->tree)=NIL;
  map_tree_acc(tree.val.tree->tree,new_tree->tree,f);
  return tree_sexp(new_tree);
}
void preorder_walk(sexp node,sexp(*f)(sexp)){
  if(!NILP(node)){
    f(node);
    if(has_left_child(node)){
      preorder_walk(XCDAR(node),f);
    }
    if(has_right_child(node)){
      preorder_walk(XCDDR(node),f);
    }
  }
}
void postorder_walk(sexp node,sexp(*f)(sexp)){
  if(!NILP(node)){
    if(has_left_child(node)){
      preorder_walk(XCDAR(node),f);
    }
    if(has_right_child(node)){
      preorder_walk(XCDDR(node),f);
    }
    f(node);
  }
}
void levelorder_walk(sexp node,sexp(*f)(sexp)){
  sexp queue=cons_sexp(xmalloc(sizeof(cons)));
  sexp next;
  XCAR(queue)=XCDR(queue)=NIL;
  enqueue(node,queue);
  while(!C_QUEUE_EMPTY(queue)){
    next=dequeue(queue);
    f(XCAR(next));
    if(has_left_child(next)){
      enqueue(XCADR(next),queue);
    }
    if(has_right_child(next)){
      enqueue(XCDDR(next),queue);
    }
  }
}
enum walk_order{
  _inorder,_preorder,_postorder,_levelorder,
};
sexp lisp_tree_walk(sexp node,sexp fun,sexp order){
  enum walk_order walk_order;
  if(!LISP_TREEP(node) || !FUNP(fun)){
    return format_type_error2("walk-tree","tree",node.tag,"function",fun.tag);
  } 
  sexp(*f)(sexp)=fun.val.fun->comp.f1;
  if (NILP(order)){
    inorder_walk(node.val.tree->tree,f);
    return node;
  } else if(!KEYWORDP(order)){
    return format_type_error_key("walk-tree","order",":inorder,:preorder,:postorder or :levelorder",
                                 order.tag);
  } else {
    //lame way to emulate a switch 
    if (KEYWORD_COMPARE(":inorder",order)){
      inorder_walk(node.val.tree->tree,f);
      return node;
    }
    if (KEYWORD_COMPARE(":preorder",order)){
      preorder_walk(node.val.tree->tree,f);
      return node;
    }
    if (KEYWORD_COMPARE(":postorder",order)){
      postorder_walk(node.val.tree->tree,f);
      return node;
    }    
    if (KEYWORD_COMPARE(":levelorder",order)){
      levelorder_walk(node.val.tree->tree,f);
      return node;
    }
    return error_sexp("invalid keyword passes to walk-tree");
 }
}      
sexp tree_lookup(sexp tree,sexp val){
  if(!LISP_TREEP(tree)){
    return format_type_error("tree-lookup","tree",tree.tag);
  }
  sexp(*f)(sexp,sexp)=tree.val.tree->comp_fn;
  sexp node=tree.val.tree->tree;
  while(CONSP(node)){
    if(isTrue(lisp_eq(val,XCAR(node)))){
      return LISP_TRUE;
    } else if (NILP(XCDR(node))){
      return LISP_FALSE;
    } else {
      node=next_node(tree,val,f);
    }
  }
  return LISP_FALSE;
}
/* Red-Black tree
 * 1. All nodes are either BLACK or RED
 * 2. Leafs are BLACK
 * 3. A RED node has BLACK children only
 * 4. Path from a node to any leafs has the same number of BLACK nodes.
 */



/* Heap
   -tree with compairson function f
   -the tree satisfies the headp property
   -that is, f(node,left-child) and f(node,right-child) are both true
   -each subtree also satisfies the heap property
   -implemented as an array.
   f(node)=[left-child,right-child];
   [root,f(root),f(root-left-child),f(root-right-child),...f(root-nth-child)]
   -i.e the array holds the elements of the tree as if they were being reads
   from left-right, top-bottom
   -for any index i, the left child is at index 2i+1, right child at 2i+2
     and parent at floor((i-1)/2)
*/
#define HEAP_LEFT_CHILD(_heap,index) (_heap->arr[(index<<1)+1])
#define HEAP_RIGHT_CHILD(_heap,index) (_heap->arr[(index<<1)+2])
#define HEAP_PARENT(_heap,index) (_heap->arr[(index-1)>>1])
#define HEAP_PARENT_INDEX(index) ((index-1)>>1))
#define HEAP_TOP(_heap) (_heap->arr[0])
sexp heap_left_child(sexp heap,sexp index){
  uint64_t ind=index.val.uint64;
  return heap.val.heap->arr[(ind<<1)+1];//ind*2+1
}
sexp lisp_heap_left_child(sexp heap,sexp index){
  if(!HEAPP(heap) || !INTP(index)){
    return format_type_error2("heap-left-child","heap",
                              heap.tag,"integer",index.tag);
  } else {
    HEAP_LEFT_CHILD(heap.val.heap,index.val.int64);
  }
}
sexp lisp_heap_right_child(sexp heap,sexp index){
  if(!HEAPP(heap) || !INTP(index)){
    return format_type_error2("heap-right-child","heap",
                              heap.tag,"integer",index.tag);
  } else {
    HEAP_RIGHT_CHILD(heap.val.heap,index.val.int64);
  }
}
sexp heap_right_child(sexp heap,sexp index){
  uint64_t ind=index.val.uint64;
  return heap.val.heap->arr[(ind<<1)+2];//ind*2+2
}
sexp heap_parent(sexp heap,sexp index){
  uint64_t ind=index.val.uint64;
  return heap.val.heap->arr[((ind-1)>>1)];
}
void heap_swap(sexp heap,sexp index1,sexp index2){
  uint64_t ind1=index1.val.uint64,ind2=index2.val.int64;
  register sexp temp=heap.val.heap->arr[ind1];
  heap.val.heap->arr[ind1]=heap.val.heap->arr[ind2];
  heap.val.heap->arr[ind2]=temp;
  return;
}
static inline void c_heap_swap(lisp_heap *heap,uint64_t ind1,uint64_t ind2){
  register sexp temp=heap->arr[ind1];
  heap->arr[ind1]=heap->arr[ind2];
  heap->arr[ind2]=temp;
  return;
}
//these are very c-esq funcitons, are are probably best left in c
static void heapify(sexp heap,uint64_t index){
  uint64_t ind=index;
  uint64_t cur_max;
  sexp left,right;
  lisp_heap *cur_heap=heap.val.heap;
  while(ind<cur_heap->len){
    left=HEAP_LEFT_CHILD(cur_heap,ind);
    right=HEAP_RIGHT_CHILD(cur_heap,ind);
    cur_max=ind;
    if(isTrue(cur_heap->comp_fn(left,cur_heap->arr[cur_max]))){
      cur_max=((ind<<1)+1);
    }
    if(isTrue(cur_heap->comp_fn(right,cur_heap->arr[cur_max]))){
      cur_max=((ind<<1)+2);
    }
    if(cur_max==ind){
      return;
    }    
    c_heap_swap(cur_heap,ind,cur_max);
    ind=cur_max;
  }
  return;
}
static void heapify_array(lisp_heap* heap,uint64_t len){
  uint64_t i;
  for(i=((len>>1)+(len&1));i<=0;i--){
    heapify(heap_sexp(heap),i);
  }
}

sexp heap_peek(sexp heap){
  if(!HEAPP(heap)){
    return format_type_error("heap-peek","heap",heap.tag);
  }
  if(heap.val.heap->len>0){
    return HEAP_TOP(heap.val.heap);
  }
}
sexp c_heap_pop(sexp heap){
  sexp retval=HEAP_TOP(heap.val.heap);//get retval
  //put last value at heap top
  //this is actually the best way to do this(suprising, at least to me)
  heap.val.heap->arr[0]=heap.val.heap->arr[heap.val.heap->len];
  heapify(heap,0);
  return retval;
} 
sexp heap_pop(sexp heap){
  if(!HEAPP(heap)){
    return format_type_error("heap-pop!","heap",heap.tag);
  }
  return c_heap_pop(heap);
}
static void heap_insert_helper(lisp_heap *cur_heap,uint64_t cur_index){
  if(isTrue(cur_heap->comp_fn
            (cur_heap->arr[cur_index],HEAP_PARENT(cur_heap,cur_index)))){
    c_heap_swap(cur_heap,cur_index,((cur_index-1)>>1));
    heap_insert_helper(cur_heap,((cur_index-1)>>1));
  } else {
    return;
  }
}
sexp c_heap_insert(sexp heap,sexp new_val){
  lisp_heap *cur_heap=heap.val.heap;
  if(cur_heap->len>cur_heap->size){
    cur_heap->size*=2;
    cur_heap->arr=xrealloc(cur_heap->arr,cur_heap->size);
  }
  cur_heap->arr[cur_heap->len]=new_val;
  uint64_t cur_index=cur_heap->len;
  cur_heap->len++;
  heap_insert_helper(cur_heap,cur_index);
  return heap;
}
sexp heap_insert(sexp heap,sexp new_val){
  if(!HEAPP(heap)){
    return format_type_error("heap-insert","heap",heap.tag);
  } else {
    return c_heap_insert(heap,new_val);
  }
}

//(defun make-heap (predicate &key size initial-contents
sexp make_heap(sexp comp_fun,sexp arr,sexp size){
  if(!FUN2P(comp_fun)){
    return format_type_error("make-heap","function of two arguments",comp_fun.tag);
  }
  sexp(*comp_fn)(sexp,sexp)=comp_fun.val.fun->comp.f2;
  sexp *heap_arr;
  uint64_t heap_size;
  if(NILP(size)){
    heap_size=0;
  } else if (!INTP(size)){
    return format_type_error_key("make-heap","size","integer",size.tag);
  } else {
    heap_size=size.val.int64;
  }
  if(NILP(arr)){
    heap_arr=xmalloc(sizeof(sexp)*(heap_size?heap_size:16));
    lisp_heap *retval=xmalloc(sizeof(lisp_heap));
    *retval=(lisp_heap){.arr=heap_arr,.size=16,.len=0,.comp_fn=comp_fn};
    return heap_sexp(retval);
  } else if(!ARRAYP(arr)){
    return format_type_error_key("make-heap","arr","array",arr.tag);
  } else {
    if(heap_size<arr.len){
      if(heap_size){//size is 0 if size was 
        fprintf(stderr,";Warning, size parameter of make-array shorter"
                "\n;than initial array, using size of array as initial size");
      }
      heap_size=arr.len;
    }
    heap_arr=arr.val.array;
  }
  //we already returned if we didn't get an initial array, so we know
  //that we need to heapify heap_arr, but first we need a heap
  lisp_heap *retval=xmalloc(sizeof(lisp_heap));  
  *retval=(lisp_heap){.arr=heap_arr,.size=heap_size,
                      .len=arr.len,.comp_fn=comp_fn};
  heapify_array(retval,arr.len);
  return heap_sexp(retval);
}
