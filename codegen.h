/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//Idea
//make a cfg, identify basic blocks and assign an sexp value to
//each basic block

#include "common.h"
#include "cons.h"
#include "prim.h"
#include <assert.h>
CORD c_codegen(sexp ast);
CORD llvm_codegen(sexp ast);
//lazy fillin untill I add better error handling
static void handle_error(){abort();}
static CORD codegen(FILE* outfile,sexp ast,enum backend backend){
  switch(backend){
    case 0:{
      CORD c_code=c_codegen(ast);
      return c_code;
    }
    default:
      return "";
  }
}
