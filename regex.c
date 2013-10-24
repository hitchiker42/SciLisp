#include "common.h"
#include "regex.h"
/*should run before any regex stuff
  re_set_syntax(RE_SYNTAX_EMACS);*/
sexp lisp_re_compile(sexp regex){
  if(!STRINGP(regex)){
    return error_sexp("argument to regex-compile must be a string");
  }
  char* error_string;
  const char* pattern=CORD_to_const_char_star(regex.val.cord);
  size_t length=CORD_len(regex.val.cord);
  regex_t* re_buffer=xmalloc(sizeof(regex_t));
  //re_compile_pattern returns an error string on failure
  if(error_string = re_compile_pattern(pattern,length,re_buffer)){
    return error_sexp(error_string);
  } else {
    return (sexp){.tag=_regex,.val={.regex=re_buffer}};
  }
}
#if 0
sexp lisp_re_replace(sexp regex,sexp string){
  if(STRINGP(regex)){
    regex=lisp_re_compile(regex);
  } else if(!REGEXP(regex)){
    return error_sexp 
      ("1st argument to replace-regexp must be a string or a compiled regexp");
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
