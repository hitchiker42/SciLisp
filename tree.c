#include "tree.h"
#include "cons.h"
sexp tree_NIL;
#define tree_add_node(_tree_,node,access_fn)      \
  access_fn(_tree_)=xmalloc(2*sizeof(cons));      \
  XCAR(access_fn(_tree_))=node;                   \
  XCDR(access_fn(_tree_))=tree_NIL;               \
  access_fn(_tree_).tag=_tree_node
//start and end should always be powers of 2
static inline void make_btree_acc
(cons *mem,sexp *vals,int start,int end,int ind,int ind_max){
  mem[start]->car=vals[ind];
  if(ind-1<0 && ind+1>=ind_max){
    mem[start]->cdr=tree_NIL;
  } else {
    mem[start]->cdr=cons_sexp(mem+start+1);
    //next cell holds previous value in list
    if(ind-1<0){
      mem[start+1]->car=tree_NIL;
    } else {
      mem[start+1]->car=cons_sexp(mem+start+2);
      make_btree_acc(mem,vals,start+2,end/2,ind-1);
    }
    if(ind+1>=ind_max){
      mem[start+1]->cdr=tree_NIL;
    } else {
      mem[start+1]->cdr=cons_sexp(mem+(end/2));
      make_btree_acc(mem,vals,start+(end/2),end,ind+1);
    }
  }
}
sexp make_btree(sexp comp_fun,sexp tree_type,sexp contents){
  if(!FUNCTIONP(comp_fun)){
    return format_type_error("make-tree","function",comp_fun.tag);
  }
  lisp_btree *new_tree=xmalloc(sizeof(lisp_btree));
  new_tree->comp_fn=comp_fun.val.fun->comp.f2;
  if(!NILP(tree_type)){
    //lookup keyword and match to tree type
  }// else {
  new_tree.val.tree->btree.meta=_basic_tree;
  if(NILP(contents)){
    new_tree->btree=xmalloc(sizeof(cons)*2);
    XCAR(new_tree->btree)=tree_NIL;
    XCDR(new_tree->btree)=tree_NIL;
    return tree_sexp(new_tree);
  } else {
    contents=sarray_from_list(contents);
    contentps=sqsort_array(contents,comp_fun,long_sexp(1));
    int len=contents.len;
    //this is an array of sets of 2 conses
    cons *new_tree_mem=xmalloc(sizeof(cons)*2*len);
    new_tree->btree=cons_sexp(new_tree_mem);
    make_btree_acc(new_tree_mem,contents.val.sarray,0,len*2,len/2);
    return tree_sexp(new_tree);
  }
}
#define insert_tree_generic(btree,access_fn,unused_side)   \
  if(NILP(XCDR(btree))){                                \
    XCDR(btree)=cons_sexp((cons*)(((uint8_t*)&btree)+sizeof(cons)))     \
    tree_add_node(btree,new_node,access_fn);            \
    unused_side(btree)=tree_NIL;                        \
  } else if (NILP(access_fn(btree))){                   \
    tree_add_node(btree,new_node,access_fn);            \
  } else {                                              \
    btree=access_fn(btree);                             \
  }
sexp basic_tree_insert(sexp tree,sexp new_node){
  if(!TREEP(tree)){
    return format_type_error("tree-insert","tree",tree.tag);
  }
  sexp(*f)(sexp,sexp)=tree.val.tree->comp_fn;
  sexp btree=tree.val.tree->btree;
  while(CONSP(btree)){
    if(isTrue(f(XCAR(btree),new_node))){
      insert_tree_generic(btree,XCADR,XCDDR);
    } else {
      insert_tree_generic(btree,XCDDR,XCADR);
    }
  }
