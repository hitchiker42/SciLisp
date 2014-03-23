/* Regular expression header file

   Copyright (C) 2013-2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
/*SciLisp interface to emacs regex (also possibly pcre if I add in 
 *support for that)*/
#ifndef _REGEX_H_
#define _REGEX_H_
#include "common.h"
#include "emacs_regex.h"
#ifdef HAVE_PCRE
#include <pcre.h>
//define this as a macro because it needs to use the internal structure
//and we want to make sure as little as possible depends on that
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
//Either make a struct  re_search_data or modify this and/or modify the re functions
//so that it's possible get the next match in a string by passing the match/search data
//back to the match/search function
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
/*pcre regex struct
  typedef struct real_pcre8_or_16 {
  pcre_uint32 magic_number;
  pcre_uint32 size;               // Total that was malloced 
  pcre_uint32 options;            // Public options 
  pcre_uint32 flags;              // Private flags 
  pcre_uint32 limit_match;        // Limit set from regex 
  pcre_uint32 limit_recursion;    // Limit set from regex 
  pcre_uint16 first_char;         // Starting character 
  pcre_uint16 req_char;           // This character must be seen 
  pcre_uint16 max_lookbehind;     // Longest lookbehind (characters) 
  pcre_uint16 top_bracket;        // Highest numbered group 
  pcre_uint16 top_backref;        // Highest numbered back reference 
  pcre_uint16 name_table_offset;  // Offset to name table that follows 
  pcre_uint16 name_entry_size;    // Size of any name items 
  pcre_uint16 name_count;         // Number of name items 
  pcre_uint16 ref_count;          // Reference count 
  pcre_uint16 dummy1;             // To ensure size is a multiple of 8 
  pcre_uint16 dummy2;             // To ensure size is a multiple of 8 
  pcre_uint16 dummy3;             // To ensure size is a multiple of 8 
  const pcre_uint8 *tables;       // Pointer to tables or NULL for std 
  void             *nullpad;      // NULL padding 
} real_pcre8_or_16;*/
/*
struct re_pattern_buffer {
  unsigned char *buffer;//Space that holds the compiled pattern.
  size_t allocated;//Number of bytes to which `buffer' points.
  size_t used;  //Number of bytes actually used in `buffer'.
  reg_syntax_t syntax;  //Syntax setting with which the pattern was compiled.
  char *fastmap;  // Pointer to a fastmap, if any, otherwise zero.
  // Either a translate table to apply to all characters before
  //   comparing them, or zero for no translation.  The translation
  //   is applied to a pattern when it is compiled and to a string
  //   when it is matched.
  RE_TRANSLATE_TYPE translate;
  size_t re_nsub; //Number of subexpressions found by the compiler.
  unsigned can_be_null : 1;//Zero if this pattern cannot match the empty string, one else.
  // If REGS_UNALLOCATED, allocate space in the `regs' structure
  // for `max (RE_NREGS, re_nsub + 1)' groups.
  //If REGS_REALLOCATE, reallocate space if necessary.
  //If REGS_FIXED, use what's there.
#define REGS_UNALLOCATED 0
#define REGS_REALLOCATE 1
#define REGS_FIXED 2
  unsigned regs_allocated : 2;
  //Set to zero when `regex_compile' compiles a pattern; set to one
  //by `re_compile_fastmap' if it updates the fastmap.
  unsigned fastmap_accurate : 1;
  unsigned no_sub : 1; //If set, don't return information about subexpressions.
  //If set, a beginning-of-line anchor doesn't match at the
  //beginning of the string.  
  unsigned not_bol : 1;
  unsigned not_eol : 1;//Similarly for an end-of-line anchor.
  //If true, the compilation of the pattern had to look up the syntax table,
  //so the compiled pattern is only valid for the current syntax table.
  unsigned used_syntax : 1;
};*/
