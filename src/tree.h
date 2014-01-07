#ifndef _TREE_H
#define _TREE_H
#include "cons.h"
#include "common.h"
#include "array.h"
#include "prim.h"
//trees
//structurally trees are just recursive lists
//and a sorting function
#define AS_TREE(_tree_) _tree_.val.tree
struct lisp_tree {
  sexp tree;//.meta values == tree type
  sexp (*comp_fn)(sexp,sexp);//how to keep the tree sorted
};
struct lisp_tree_type {
  sexp (*insert)(sexp,sexp);//how to insert an element
  sexp (*delete)(sexp,sexp);//how to delete and element
  sexp (*sort)(sexp);//how to sort a stree
  //walk tree given by first arg, calling second arg on each node
  sexp (*walk)(sexp,sexp);
};
struct lisp_heap{
  sexp *arr;
  uint32_t size;//i.e memory allocated for the heap
  uint32_t len;//what index does the heap end at
  sexp(*comp_fn)(sexp,sexp);
};
struct lisp_tree_type avl_tree;
struct lisp_tree_type rb_tree;
struct lisp_tree_type splay_tree;
struct lisp_tree_type basic_tree; 
static const sexp tree_NIL={.tag=_nil,.val={.meta=_nil},.meta=_leaf};
#define next_node(tree,val,f)                   \
  (isTrue(f(XCAR(tree),val))?                   \
   XCADR(tree):XCDDR(tree))
#define has_left_child(tree_node)                                       \
  (!NILP(XCDR(tree_node))&& !NILP(XCADR(tree_node)))
#define has_left_child_unsafe(tree_node)                                       \
  (!NILP(XCADR(tree_nod)))
#define has_right_child(tree_node)                                       \
  (!NILP(XCDR(tree_node)) && !NILP(XCDDR(tree_node)))
#define has_right_child_unsafe(tree_node)       \
  (!NILP(XCDDR(tree_node)))
#define has_child(tree_node)                                           \
  (!NILP(XCDR(tree_node)))
#define get_right_child_safely(tree_node)                              \
  (NILP(XCDR(tree_node)) ? tree_NIL : XCDDR(tree_node))
#define get_left_child_safely(tree_node)                              \
  (NILP(XCDR(tree_node)) ? tree_NIL : XCADR(tree_node))
#define tree_add_node(_tree_,node,access_fn)      \
  access_fn(_tree_)=cons_sexp(xmalloc(2*sizeof(cons))); \
  XCAR(access_fn(_tree_))=node;                   \
  XCDR(access_fn(_tree_))=tree_NIL;               \
  access_fn(_tree_).tag=_tree_node
sexp make_tree(sexp comp_fun,sexp tree_type,sexp contents);
sexp basic_tree_insert(sexp tree,sexp new_node);
void inorder_walk(sexp node,sexp(*f)(sexp));
void preorder_walk(sexp node,sexp(*f)(sexp));
void postorder_walk(sexp node,sexp(*f)(sexp));
void levelorder_walk(sexp node,sexp(*f)(sexp));
sexp map_tree(sexp tree,sexp (*f)(sexp));
sexp lisp_tree_walk(sexp node,sexp fun,sexp order);
sexp tree_lookup(sexp tree,sexp val);
sexp lisp_tree_insert(sexp tree,sexp new_node);
#endif
