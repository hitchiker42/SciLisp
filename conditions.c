/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/*Scilisp conditions, i.e error handling and recovery*/
#include "common.h"
struct lisp_condition {
  CORD condition_str;//for, if nothing else, compatibily with the basic error 
  //type that's just a cord
