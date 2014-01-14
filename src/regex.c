#include "regex.h"
#include "cons.h"
/*should run before any regex stuff
  re_set_syntax(RE_SYNTAX_EMACS);*/
//(defun re-compile (regex &optional opts))
sexp lisp_re_compile(sexp regex,sexp opts){
  if(!STRINGP(regex)){
    return format_type_error("re-compile","string",regex.tag);
  }  
  char* error_string;
  const char* pattern=CORD_to_const_char_star(regex.val.cord);
  size_t length=CORD_len(regex.val.cord);

#ifdef HAVE_PCRE
  pcre *re_buffer=xmalloc(sizeof(pcre));
  int err_offset;
  pcre_compile(pattern,0,&error_string,&err_offset,NULL);
  if(!error_string){
    void **pcre_pattern=xmalloc(2*sizeof(void*));
    pcre_pattern[0]=re_buffer;
    return regex_sexp(pcre_pattern);
  } else {
    return error_sexp(error_string);
  }
#else
  regex_t* re_buffer=xmalloc(sizeof(regex_t));
  //re_compile_pattern returns an error string on failure
  if((error_string = (char*)re_compile_pattern(pattern,length,re_buffer))){
    return error_sexp(error_string);
  } else {
    return regex_sexp(re_buffer);
  }
#endif
}
sexp lisp_re_optimize(sexp regex){
  if(!REGEXP(regex)){
    return format_type_error("re-optimize","regex",regex.tag);
  }
#if HAVE_PCRE
  void** pcre_pattern=(void**)regex.val.regex;
  pcre* pcre_regex=(pcre*)pcre_pattern[0];
  char *pcre_error;
  pcre_pattern[1]=pcre_study(pcre_regex,0,pcre_error);
  if(!pcre_error){
    return regex_sexp(pcre_pattern);
  } else {
    return error_sexp(pcre_error);
  }
#else
  if(re_compile_fastmap(regex.val.regex)){
    return error_sexp("error optimizing regexp");
  } else {
    return regex;
  }
}
//(defun re-match (re string &optional start no-subexprs t-or-f-only))
sexp lisp_re_match(sexp re,sexp string,sexp start,sexp opts){
  if((!REGEXP(re) && !STRINGP(re))){
    return format_type_error_opt2("re-match","string","regex",re.tag);
  } if(!STRINGP(string)){
    return format_type_error_named("re-match","string","string",string.tag);
  }
  if(STRINGP(re)){
    re=lisp_re_compile(re,NIL);
  }
  uint64_t int_start;
  if(NILP(start)){
    int_start=0;
  } else if(!INTP(start)){
    return format_type_error_opt("re-match","integer",start.tag);
  } else {
    int_start=start.val.int64;
  }
  const char *str_to_match=CORD_to_const_char_star(string.val.cord);
  int len=CORD_len(string.val.cord);
  int64_t match_len;
  //test if we want registers, not yet implemented;
  if(0){//isTrue(dont_return_matches) || isTrue(only_true_or_false)){
    match_len=re_match(re.val.regex,str_to_match,len,0,0);
    if(isTrue(only_true_or_false)){
      return ((match_len<=0) ? LISP_TRUE : LISP_FALSE);
    } else {
      return ((match_len<=0) ? long_sexp(match_len) : LISP_FALSE);
    }
  } else {
    struct re_registers *match_data=xmalloc(sizeof(struct re_registers));
#if HAVE_PCRE
    pcre* pcre_regexp=(pcre*)re.val.opaque[0];
    pcre_extra *pcre_studied=(pcre_extra*)re.val.opaque[1];
    int *pcre_regs=xmalloc(3*prce_num_refs(pcre_regexp)*sizeof(int));
    pcre_
    match_len=pcre_exec(pcre_regexp,pcre_studied,str_to_match,
                        len,start.val.uint64,pcre_regs,pcre_num_refs(pcre_regexp));
    
#else
    match_len=re_match(re.val.regex,str_to_match,len,0,match_data);
#endif
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
    return format_type_error_opt2("re-replace","regex","string",regex.tag);
  }
  if(!STRINGP(string)){
    return format_type_error_named("re-replace","string","string",string.tag);
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
#endif
