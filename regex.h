/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/

/*SciLisp interface to emacs regex (also possibly pcre if I add in 
 *support for that)*/
#ifndef _REGEX_H_
#define _REGEX_H_
#include "common.h"
#include "emacs_regex.h"
struct re_match_data{
  CORD re_string;
  struct re_registers *match_data;
  regex_t *matched_re;
  int matched_len;
  int total_len;
};
sexp lisp_re_compile(sexp regex);
sexp lisp_re_match(sexp regex,sexp string,sexp start,
                   sexp dont_return_matches,sexp only_true_or_false);
sexp lisp_get_re_backref(sexp match_data,sexp ref_num);
/*
struct re_registers
{
  unsigned num_regs;
  regoff_t *start;
  regoff_t *end;
  };*/
#endif
