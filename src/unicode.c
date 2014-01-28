/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/* A note about SciLisp strings and characters,
   SciLisp characters are represented by a single wchar_t value, regardless of
   the value of the character, while SciLisp strings are represented by CORDs, so
   SciLisp characters are wide characters, while SciLisp strings are multibyte
   strings, This may change at some point, but that's how it is for now*/
#include "unicode.h"
#include <endian.h>
#include <ctype.h>

sexp lisp_char_to_string(sexp lisp_char){
  if(!CHARP(lisp_char)){
    return format_type_error("char->string","character",lisp_char.tag);
  } else {
    c_string retval=c_wchar_to_string(lisp_char.val.uchar);
    if(retval[0] == 'c' && retval[1] != '\0'){
      //I think this should work, because A null character can only be itself
      //and so {'c',!'\0'} has to be an error string
      return error_sexp(retval);
    } else {
      return cord_sexp(retval);
    }
  }
}
//pretty much a simplified wrapper to wcrtomb (but returns a vaild c string)
c_string c_wchar_to_string(wchar_t lisp_char){
  mbstate_t state;
  size_t nbytes;
  memset(&state,'\0',sizeof(state));
  nbytes=wcrtomb(temp_ustring,lisp_char,&state);
  if(nbytes==(size_t)-1){
    return "conversion error in char->string";
  } else {
    char *retval=xmalloc(nbytes+sizeof(char));
    memcpy(retval,temp_ustring,nbytes);
    retval[nbytes]='\0';
    return retval;
  }
}
sexp lisp_string_to_char(sexp lisp_str){
  if(!STRINGP(lisp_str)){
    return format_type_error("string->char","string",lisp_str.tag);
  }
  //most common cases:
  lisp_string *str=lisp_str.val.string;
  if(str->string_type==str_wstring){
    return uchar_sexp(str->wstring[0]);
  } else if (str->string[0]>0 && str->string<=127){
    //test if first char is a valid ascii char
    return uchar_sexp((wchar_t)str->string[0]);
  }
  wchar_t retval;
  mbstate_t state;
  size_t nbytes;
  memset(&state,'\0',sizeof(state));
  if(str->string[0]==0){
    char[4] mb_str;
    CORD_pos pos;
    CORD_set_pos(pos,str->cord,0);
    int i=0;
    while(i<4 && CORD_pos_valid(pos)){
      mb_str[i]=CORD_pos_fetch(pos);
      CORD_next(pos);
    }
    if(i != 4){return error_sexp("CORD error");}
    nbytes=mbrtowc(&retval,mb_str,4,&state);
  } else {
    nbytes=mbrtowc(&retval,str->str,4,&state);
  }
  if(nbytes==(size_t)-1){
    return_errno("string->char");
  } else if (nbytes==(size_t)-2){
    //shouldn't happen,this only happens if n is too small
    //and 4 should be the max, so
    return error_sexp("Shouldn't get here");
  } else {
    return uchar_sexp(retval);
  }
}
wchar_t* lisp_string_to_wcs(lisp_string *string,int *out_len){
  mbstate_t state;
  size_t nbytes;
  memset(&state,'\0',sizeof(state));
  //make a naieve assumption that almost every byte in the input is a character
  //and hope that the memory wasted is worth only having to allocate
  //memory once (we'd only call this if the string was a multibyte string
  //meaning the number of bytes must be > the number of characters
  wchar_t *output=xmalloc_atomic((string->len-1)*sizeof(wchar_t));
  const char *input=CORD_to_const_char_star(string->cord);
  int output_index,input_index,len=string->len;
  while((nbytes=mbrtowc(output+output_index,input+input_index,
                        len-input_index,state))){
    if(nbytes==(size_t)-1){
      return NULL;//needs a better value
    }
    if(nbytes==(size_t)-2){
      return NULL;//n was too small, my math was off
      //or the  input string was an invalid multibyte string
    }
    input_index+=nbytes;
    output_index++;
  }
  if(out_len){
    *out_len=output_index;
  }
  return output;
}  
wchar_t* lisp_mbsrtowcs(char *restrict str,mbstate_t *restrict state){
  if(!state){
    state=alloca(sizeof(mbstate_t));
    memset(state,'\0',sizeof(mbstate_t));
  }
  char *restrict strptr=str;
  uint32_t len=16;
  wchar_t *wstr=xmalloc_atomic(len*sizeof(wchar_t));
  //loop converting the given string to a wchar string,
  //allocating more memory to hold the wchar string, if necessary
  while(mbstrowcs(wstr,&strptr,len,state) == len && strptr){
    wstr=xrealloc(wstr,(len*=2));
  }
  return wstr;
}
/*
 * convert a wide character string into a multi-byte string re using the input
 * butter as the output buffer, thus overwriting the given input.
 * This assumes that the maximum size of a multibyte sequence is <= sizeof(wchar_t)
 * which for UTF-8 (on the basic multilingual plane) as the multibyte encoding
 * and a 32 bit wchar should hold. This could change and render this funciton
 * unuseable.
 */
char *wcsrtombs_destructive(wchar *restrict input,mbstate *restrict state){
  char *output=(char *)input;
  int output_index=0,input_index=0;
  size_t nbytes;
  while(input[input_index]){
    nbytes=wcrtomb(output+output_index,input[input_index],state);
    if(nbytes=(size_t)-1){
      return NULL;//I dunno what to do here, it'd be weird to use lisp errors
    }
    output_index+=nbytes;
    input_index++;
  }
  //this `should` zero the rest of the input buffer
  //assunming my math is right (which it might not be)
  memset(output+output_index,'\0',
         (input_index*(sizeof(wchar_t)/sizeof(char)))-output_index);
  return output;
}
sexp *c_lisp_strcat(sexp *args,int numargs){
  if(numargs <=1){
    return *args;
  }
  CORD retval=0;
  uint32_t len=0;
  int i;
  for(i=0;i<numargs;i++){
    len+=args[i].val.string->len;
    retval=CORD_cat_char_star(retval,args[i].val.string->string,
                              args[i].val.string->len);
  };
  return
