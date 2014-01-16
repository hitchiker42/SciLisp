 /*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/* TODO: Figure out how to pass environments to macros in a somewhat consistent
   and elegant way*/
//KINDA obvious thing to do which I haven't done,
//rest arguments to c functions should be arrays not lists
//so that something like addition (obviously it's a bit more compilicated)
//would be defined as sexp lisp_add(int numargs,sexp *args)
//well... this is how emacs does it, but that'll require
//quite a bit of tinkering, and I should probably start
//focusing on the compiler(s) rather than that
#include "common.h"
#include "cons.h"
sexp eval(sexp expr,environment env);
