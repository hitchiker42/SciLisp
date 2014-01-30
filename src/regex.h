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
#ifdef HAVE_PCRE
#include <pcre.h>
pcre_malloc=xmalloc;
pcre_free=xfree
#define pcre_num_refs(pcre_regex) (pcre_regex->top_bracket)
typedef struct {
  uint32_t num_regs;
  union {
    int *pcre_start;
    regoff_t *start;
  };
  union {
    int *pcre_end;
    regoff_t *end;
  };
} pcre_registers;
#endif
struct re_match_data {
  const char *re_string;
  struct re_registers *match_data;    
  regex_t *matched_re;
  int matched_len;
  int total_len;
};
sexp lisp_re_compile(sexp regex,sexp opts);
sexp lisp_re_optimize(sexp regex);
sexp lisp_re_match(sexp regex,sexp string,sexp start,sexp opts);
sexp lisp_get_re_backref(sexp match_data,sexp ref_num);
sexp lisp_re_replace(sexp regex,sexp replacement,sexp string);
/*
struct re_registers
{
  unsigned num_regs;
  regoff_t *start;
  regoff_t *end;
  };*/
#endif
