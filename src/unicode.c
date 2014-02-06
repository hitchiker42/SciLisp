/* Unicode support and general string routines

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
/* A note about SciLisp strings and characters,
   SciLisp characters are represented by a single wchar_t value, regardless of
   the value of the character, while SciLisp strings are represented by CORDs, so
   SciLisp characters are wide characters, while SciLisp strings are multibyte
   strings, This may change at some point, but that's how it is for now*/
#include "unicode.h"
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
 * which for UTF-8 on the basic multilingual plane and a 32 bit wide character
 * is true. If there are characters outside the BMP this might work depending on how
 * many and where they are in the string, but really don't use this if that's the case.
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
  return;
}
//from the linux kernel (the assembly bit)
//not sure why I couldn't think to do bsf !val but eh
static inline uint64_t ffz(uint64_t word){
#ifdef __x86_64__ 
  asm("bsf %1,%0"
      : "=r" (word)
      : "r" (~word));
  return word;
#else
  return ffsl(~word);
}
int utf8_char_len(char *mb_char){  
  if(mb_char<0x80){
    return 1;
  } else {
    return 1-ffz(*mb_char);
  }
}
