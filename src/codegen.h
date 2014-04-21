/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//Idea
//make a cfg, identify basic blocks and assign an sexp value to
//each basic block
static CORD codegen(sexp ast,enum backend backend){
  fprintf(stderr,"codegen unimplemented exiting\n");
  exit(0);
}
#if 0
#include "common.h"
#include "cons.h"
#include "prim.h"
#include <assert.h>
CORD lisp_name_to_c_name
CORD c_codegen(sexp ast);
CORD llvm_codegen(sexp ast);
static CORD codegen(sexp ast,enum backend backend){
  switch(backend){
    case 0:{
      CORD c_code=c_codegen(ast);
      return c_code;
    }
    default:
      return "";
  }
}
#endif
