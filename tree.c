#include "common.h"
#include "cons.h"
typedef struct lisp_btree lisp_btree;
typedef enum  btree_type btree_type;
/* there is no format structure for a binary tree, but
   each node is a cons cell consisting of a car containing
   the actual data and a cdr which is a dotted pair of the
   left and right children, or nil if this is a leaf*/
struct lisp_btree {
  sexp btree;
  sexp (*comp_fn)(sexp,sexp);//how to keep the btree sorted
  btree_type tree_type;
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
