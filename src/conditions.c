/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/*Scilisp conditions, i.e error handling and recovery*/
#include "common.h"
enum condition_type{};
/*
  need to find a means of catching a subset of conditions
  (throw TAG VAL) will be caught by a handler whos member tag
  is eq to tag, ne
*/
struct lisp_handler {
  sexp tag;
  sexp val;
  //type that's just a cord
  jmp_buf jmp;//where the conditon was raised
  int cond_type;
  int sig;
  struct lisp_handler *next;//next handler in the handler stack
  void *user_data;//just in case
};
#define raise_condition(cond)                   \
  if(!sigsetjmp(cond->restart_loc)){                \
  raise(cond->sig);                                 \
  }

  
