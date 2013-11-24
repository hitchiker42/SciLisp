#include "common.h"
#include "cons.h"
sexp tree_NIL;
#define AS_TREE(_tree_) _tree_.val.tree
/* there is no formal structure for a binary tree, but
   each node is a cons cell consisting of a car containing
   the actual data and a cdr which is a dotted pair of the
   left and right children, or nil if this is a leaf*/
struct lisp_btree {
  sexp btree;//.meta values == tree type
  sexp (*comp_fn)(sexp,sexp);//how to keep the btree sorted
};
struct lisp_btree_type {
  sexp (*insert)(sexp,sexp);//how to insert an element
  sexp (*delete)(sexp,sexp);//how to delete and element
  sexp (*sort)(sexp);//how to sort a stree
  //walk tree given by first arg, calling second arg on each node
  sexp (*walk)(sexp,sexp);
};
struct lisp_btree_type avl_tree;
struct lisp_btree_type rb_tree;
struct lisp_btree_type splay_tree;
struct lisp_btree_type basic_tree;
#define tree_add_node(_tree_,node,access_fn)      \
  access_fn(_tree_)=xmalloc(sizeof(cons));        \
  XCAR(access_fn(_tree_))=leaf;                   \
  XCDR(access_fn(_tree_))=tree_NIL;               \
  access_fn(_tree_).tag=_tree_node
static inline make_btree_acc(
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
    contents=sqsort_array(contents,comp_fun,long_sexp(1));
    int len=contents.len;
    sexp *new_tree_mem=xmalloc(sizeof(cons)*2*len);
    new_tree->btree=new_tree_mem[0];
    XCDR(btree->new_tree)=tree_mem[1];
    
    

    
    
sexp basic_tree_insert
