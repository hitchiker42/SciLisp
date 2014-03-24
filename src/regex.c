/* Regular expressions, searching, matching and replacing

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
#include "regex.h"
#include "cons.h"
#ifdef HAVE_PCRE
void*(*pcre_malloc)(size_t)=GC_malloc;
void(*pcre_free)(void*)=GC_free;
#endif

#ifdef HAVE_PCRE
sexp pcre_compile_pattern(char *pattern,size_t len){
  pcre *re_buffer=xmalloc(sizeof(pcre));
  int err_offset;
  char* error_string;
  re_buffer=pcre_compile(pattern,0,&error_string,&err_offset,NULL);
  if(!error_string){
    void **pcre_pattern=xmalloc(2*sizeof(void*));
    pcre_pattern[0]=re_buffer;
    return regex_sexp(pcre_pattern);
  } else {
    raise_simple_error_fmt(Einternal,
                           "Internal error in regex-compile.\n%s",error_string);
  }
}
#endif
sexp emacs_compile_pattern(char *pattern,size_t len){
  char* error_string;
  regex_t* re_buffer=xmalloc(sizeof(regex_t));
  //re_compile_pattern returns an error string on failure
  if((error_string = (char*)re_compile_pattern(pattern,length,re_buffer))){
    raise_simple_error_fmt(Einternal,
                           "Internal error in regex-compile.\n%s",error_string);
  } else {
    return regex_sexp(re_buffer);
  }
}
/*should run before any regex stuff
  re_set_syntax(RE_SYNTAX_EMACS);*/
//(defun re-compile (regex &optional opts))
sexp lisp_re_compile(sexp regex,sexp opts){
  if(!STRINGP(regex)){
    raise_simple_error(Etype,
                       format_type_error("re-compile","string",regex.tag));
  }
  const char* pattern=CORD_to_const_char_star(regex.val.string->cord);
  size_t length=regex.val.string->len;
  //if pcre syntax
  //return pcre_compile_pattech(pattern,length);
  return emacs_compile_pattern(pattern,length);
}
#if HAVE_PCRE
sexp pcre_optimize(void **regex){
  char *pcre_error;
  pcre* pcre_regex=(pcre*)regex[0];
  regex[1]=pcre_study(pcre_regex,0,pcre_error);
  if(!pcre_error){
    return regex_sexp(regex);
  } else {
    raise_simple_error(Einternal,"Internal error in pcre_study");
  }
}
#endif
sexp emacs_regex_optimize(sexp regex){
  if(re_compile_fastmap(regex.val.regex)){
    raise_simple_error(Einternal,"Internal error in re_compile_fastmap");
  } else {
    return regex;
  }
}
sexp lisp_re_optimize(sexp regex){
  if(!REGEXP(regex)){
    raise_simple_error(Etype,
                       format_type_error("re-optimize","regex",regex.tag));
  }
  //if pcre_syntax: pcre_optimize(regex.val.opaque);
  return emacs_regex_optimize(regex);
}
#ifdef HAVE_PCRE
sexp pcre_match(char *str,uint32_t len,int32_t start,void **re){
  pcre *regex=(pcre*)re[0];
  pcre_extra *extra=(pcre_extra*)re[1];
  int *pcre_regs=alloca(3*(prce_num_refs(regex)+1)*sizeof(int));
  /*pcre_exec
    Returns:    > 0 => success; value is the number of elements filled in
    = 0 => success, but offsets is not big enough
    -1 => failed to match
    < -1 => some kind of unexpected problem*/
  int num_matches=pcre_exec(pcre_regexp,pcre_studied,str,
                            len,start,PCRE_ANCHORED,
                            pcre_regs,pcre_num_refs(regexp));
  if(num_matches<=0){//0 shouldn't happen because we always allocate enough space
    if(num_matches==-1){
      return LISP_FALSE;//just failed to match, not an error
    } else {
      raise_simple_error(Einternal,"Internal error in pcre_exec");
    }
  }
  re_registers *regs=xmalloc(sizeof(re_registers) +
                             num_matches*(sizeof(int)*2));
  regs->num_regs=num_matches;
  //casts to void is because I do pointer arithmatic by bytes here
  regs->pcre_start=(void*)regs+sizeof(re_registers);
  regs->pcre_end=((void*)regs+sizeof(re_registers)+num_matches*sizeof(int));
  regs->pcre_start=
    memcpy_stride_32(regs->prce_start,pcre_ovector,num_matches,2);
  regs->pcre_end=
    memcpy_stride_32(regs->prce_end,pcre_ovector+1,num_matches,2);
  re_match_data *retval=xmalloc(sizeof(re_match_data));
  *retval=(re_match_data)
    {.re_string=str,.match_data=regs,.matched_re=regex,
     .matched_len=regs->pcre_end[0],.total_len=len};
  return re_match_sexp(retval);
}
#endif
sexp emacs_re_match(char *str,uint32_t len,int32_t start,regex_t *re){
  re_registers *match_data=xmalloc(sizeof(re_registers));
  int32_t match_len=re_match(re,str,len,0,match_data);
  if(match_len<=-1){
    return LISP_FALSE;
  } else {
    match_data->num_regs=re.val.regex->re_nsub;
    re_match_data *retval=xmalloc(sizeof(re_match_data));
    *retval=(re_match_data)
      {.re_string=string.val.cord,.match_data=match_data,
       .matched_re=re.val.regex,.matched_len=match_len,.total_len=len};
    return re_match_sexp(retval);
  }
}
//(defun re-match (re string &optional start no-subexprs t-or-f-only))
sexp lisp_re_match(sexp re,sexp string,sexp start,sexp opts){
  //typecheck and compile re if necessary
  if(!REGEXP(re) && !STRINGP(re)){
    raise_simple_error(Etype,format_type_error_opt2("re-match","string","regex",re.tag));
  } if(!STRINGP(string)){
    raise_simple_error(Etype,format_type_error_named("re-match","string","string",string.tag));
  }
  if(STRINGP(re)){
    re=lisp_re_compile(re,NIL);
  }
  //get starting index
  uint64_t int_start;
  if(NILP(start)){
    int_start=0;
  } else if(!INTP(start)){
    raise_simple_error(Etype,format_type_error_opt("re-match","integer",start.tag));
  } else {
    int_start=start.val.int64;
  }
  //get string and length
  const char *str_to_match=CORD_to_const_char_star(string.val.string->cord);
  int len=string.val.string->len;
  //test if we want registers, not yet implemented;
  if(0){//isTrue(dont_return_matches) || isTrue(only_true_or_false)){
    match_len=re_match(re.val.regex,str_to_match,len,0,0);
    if(isTrue(only_true_or_false)){
      return ((match_len<=0) ? LISP_TRUE : LISP_FALSE);
    } else {
      return ((match_len<=0) ? long_sexp(match_len) : LISP_FALSE);
    }
  }
  //if pcre_syntax ...
  return emacs_re_match(str_to_match,len,int_start,re.val.regex);
}

//(defun re-subexpr (match-data ref-num))
sexp lisp_get_re_backref(sexp match_data,sexp ref_num){
  if(!RE_MATCHP(match_data) || !INTP(ref_num)){
    return format_type_error2 ("re-subexpr","re-match-data",
                               match_data.tag,"integer",ref_num.tag);
  }
  int64_t reg_num=ref_num.val.int64;
  re_match_data *re_match=match_data.val.re_data;
  if(re_match->match_data->num_regs<reg_num){
    return error_sexp("re-subsexpr, subexpression index out of bounds");
  }
  return string_sexp
    (CORD_substr
     (re_match->re_string,re_match->match_data->start[reg_num],
      re_match->match_data->end[reg_num]-re_match->match_data->start[reg_num]));
}

#if 0
//(defun re-replace (regex string))
sexp lisp_re_replace(sexp regex,sexp string){
  if(STRINGP(regex)){
    regex=lisp_re_compile(regex);
  } else if(!REGEXP(regex)){
    raise_simple_error(Etype,format_type_error_opt2("re-replace","regex","string",regex.tag));
  }
  if(!STRINGP(string)){
    raise_simple_error(Etype,format_type_error_named("re-replace","string","string",string.tag));
  }
  int length = CORD_len(string.val.cord);
  struct re_registers *back_refs;



//snippet from emacs replace match
  if (!NILP (string))
    {
      Lisp_Object before, after;

      before = Fsubstring (string, make_number (0),
                           make_number (search_regs.start[sub]));
      after = Fsubstring (string, make_number (search_regs.end[sub]), Qnil);

      /* Substitute parts of the match into NEWTEXT
         if desired.  */
      if (NILP (literal))
        {
          ptrdiff_t lastpos = 0;
          ptrdiff_t lastpos_byte = 0;
          /* We build up the substituted string in ACCUM.  */
          Lisp_Object accum;
          Lisp_Object middle;
          ptrdiff_t length = SBYTES (newtext);

          accum = Qnil;

          for (pos_byte = 0, pos = 0; pos_byte < length;)
            {
              ptrdiff_t substart = -1;
              ptrdiff_t subend = 0;
              bool delbackslash = 0;

              FETCH_STRING_CHAR_ADVANCE (c, newtext, pos, pos_byte);

              if (c == '\\')
                {
                  FETCH_STRING_CHAR_ADVANCE (c, newtext, pos, pos_byte);

                  if (c == '&')
                    {
                      substart = search_regs.start[sub];
                      subend = search_regs.end[sub];
                    }
                  else if (c >= '1' && c <= '9')
                    {
                      if (c - '0' < search_regs.num_regs
                          && search_regs.start[c - '0'] >= 0)
                        {
                          substart = search_regs.start[c - '0'];
                          subend = search_regs.end[c - '0'];
                        }
                      else
                        {
                          /* If that subexp did not match,
                             replace \\N with nothing.  */
                          substart = 0;
                          subend = 0;
                        }
                    }
                  else if (c == '\\')
                    delbackslash = 1;
                  else if (c != '?')
                    error ("Invalid use of `\\' in replacement text");
                }
              if (substart >= 0)
                {
                  if (pos - 2 != lastpos)
                    middle = substring_both (newtext, lastpos,
                                             lastpos_byte,
                                             pos - 2, pos_byte - 2);
                  else
                    middle = Qnil;
                  accum = concat3 (accum, middle,
                                   Fsubstring (string,
                                               make_number (substart),
                                               make_number (subend)));
                  lastpos = pos;
                  lastpos_byte = pos_byte;
                }
              else if (delbackslash)
                {
                  middle = substring_both (newtext, lastpos,
                                           lastpos_byte,
                                           pos - 1, pos_byte - 1);

                  accum = concat2 (accum, middle);
                  lastpos = pos;
                  lastpos_byte = pos_byte;
                }
            }

          if (pos != lastpos)
            middle = substring_both (newtext, lastpos,
                                     lastpos_byte,
                                     pos, pos_byte);
          else
            middle = Qnil;

          newtext = concat2 (accum, middle);
        }
    }
}
#endif
/* Search in the string STRING (with length LENGTH) for the pattern
   compiled into BUFFER.  Start searching at position START, for RANGE
   characters.  Return the starting position of the match, -1 for no
   match, or -2 for an internal error.  Also return register
   information in REGS (if REGS and BUFFER->no_sub are nonzero).  /
extern regoff_t re_search (struct re_pattern_buffer *__buffer,
			   const char *__string, size_t __length,
			   ssize_t __start, ssize_t __range,
			   struct re_registers *__regs);*/
