/* Support routines for the reader (mostly parsing escape sequences)

   Copyright (C) 2014 Tucker DiNapoli

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
#define IN_LEXER
#include "read.h"
//For actual unicode compilance this should be 16, but llke this it filts in an sexp
#define MAX_MULTIBYTE_LEN 8
//for functions that return characters I return -1 to indicate an error,
//this should work in any instance, since the sign bit of a simple ascii char is generally
//unimportant so something like if(value==(char)-1); is safe, i.e -1 is never valid.
//for utf8 no (currently) valid sequence starts with 0xff, so again, it won't be valid
char parse_hex_escape(char *input);//sets input to the first character after the escape
wchar_t parse_unicode_escape(char *input);
//just for convience, just calls wcrtomb on parse_unicode_escape
char* parse_unicode_escape_to_utf8(char *input);
#define HEXVALUE(c) \
  (((c) >= 'a' && (c) <= 'f') \
        ? (c)-'a'+10 \
        : (c) >= 'A' && (c) <= 'F' ? (c)-'A'+10 : (c)-'0')
//large ammounts of this taken from the bash printf builtin
static inline char parse_simple_escape(char escape_char){
  //shamelessly stolen from emacs out of shear lazyness
  switch(escape_char){
    case 'a':
      return 0x7;
    case 'b':
      return '\b';
    case 'd':
      return 0x7f;
    case 'e':
      return 0x1b;
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\\':
      return '\\';
    //this is because lisp uses question marks to express characters
    case '?':
      return '?';
    case '0':
      return '\0';
    default:
      return (char)-1;
  }
}
int parse_escape_internal(char *input,wchar_t* output){//input is the text immediately following a backslash
  *output=(wchar_t)parse_simple_escape(*input);
  if(*output != ((wchar_t)(char)-1)){
    return 1;
  }
  int temp=0;
  wint_t uvalue;
  char *p=input+1;
  switch(*input++){
    case 'x':{
      //note to self: --  (prefix or postfix) has higer precidence than &&
      uvalue=parse_hex_escape(p);
      if(uvalue == (char)-1 || p == input+1){
        return 0;//parse hex escape will print an error message
      }
      *output=uvalue;
      return p-input;
    }
    case 'u':
      uvalue=parse_unicode_escape(p);
      if (uvalue == (wchar_t)-1 || p == input + 1){
        return 0;//parse unicode will print an error message
      }
      *output=uvalue;
      return p-input;
    default:
      return 0;
  }
}
wchar_t parse_escape(char *input){
  wchar_t output;
  if(parse_escape_internal(input,&output)){
    return output;
  } else {
    return (wchar_t)-1;
  }
}
char *parse_escape_mb(char *input){
  //I really don't know a better way to do this
  char simple_retval[1];
  char *mb_retval;
  if((simple_retval[0]=parse_simple_escape(*input) != (char)-1)){
    return simple_retval;
  } else if((simple_retval[0]=parse_hex_escape(input)) != (char)-1){
    return simple_retval;
  } else if((mb_retval==parse_unicode_escape_to_utf8)){
    return mb_retval;
  }
}
char parse_hex_escape(char *input){//sets input to the first character after the escape
  if(!(isxdigit(*input))){
    fprintf(stderr,"error lexing char, expected hex digit after \\x\n");
    return (char)-1;//incase char is unsigned
  }
  if(isxdigit(*input+1)){
    //pretty sure I can't just use strtol because \x010 is 1, not 10
    //sets input to input+2 and returns the hexvalue of *input,*input+1
    return((16*HEXVALUE(*input++))+HEXVALUE(*input++));
  } else {
    return (HEXVALUE(*input++));
  }
}
wchar_t parse_unicode_escape(char *input){
  int temp=4;
  wchar_t uvalue;
  for (uvalue = 0; isxdigit ((unsigned char)*input) && temp--; input++){
    uvalue <<= 4;
    uvalue += HEXVALUE(*input);
  }
  if (temp == 4){
    fprintf(stderr,"error lexing char, expected hex digit after \\u\n");
    return (wchar_t)-1;
  }
  return uvalue;
}
char* parse_unicode_escape_to_utf8(char *input){
  wchar_t u32_char=parse_unicode_escape(input);
  if(u32_char == (wchar_t)-1){
    return NULL;
  } else {
    size_t nbytes;
    char retval[utf8_len_max]={0};
    nbytes=utf8_encode_char(retval,u32_char);
    if(nbytes == (size_t)-1){
      raise_simple_error(Etype,"Invalid unicode code point");
    }
    return retval;
  }
}   

//returns the index of the first backslash in input
//or -1 if there isn't one
inline int string_has_escape(lisp_string *input){
  //I would use CORD_chr for both types of string
  //but it looks like CORD_chr doesn't optimize things
  //when a CORD is a c_string so I need to do that myself
  if(input->string[0]=='\0'){//cord
    size_t cordind=CORD_chr(input->cord,0,'\\');
    if(cordint==CORD_NOT_FOUND){
      return -1;
    } else {
      return cordint;
    }
  } else {//cstring(will most likely return a cord however)
    char *strptr=memchr(input->string,'\\',input->len);
    if(!strptr){
      return -1;
    } else {
      return input->string-strptr;
    }
  }
}
inline int strchr_index(const char *input){
  //I would use CORD_chr for both types of string
  //but it looks like CORD_chr doesn't optimize things
  //when a CORD is a c_string so I need to do that myself
  if(input->string[0]=='\0'){//cord
    size_t cordind=CORD_chr(input->cord,0,'\\');
    if(cordint==CORD_NOT_FOUND){
      return -1;
    } else {
      return cordint;
    }
  } else {//cstring(will most likely return a cord however)
    char *strptr=strchr(input->string,'\\');
    if(!strptr){
      return -1;
    } else {
      return input->string-strptr;
    }
  }
}
lisp_string *parse_string_escapes(lisp_string *input){
  int ind;
  if((int=string_has_escape(input))<0){
    return input;
  }
  //get the begining of the string, before any escapes
  CORD parsed_string=CORD_substr(input->cord,0,ind);
}
wchar_t parse_char(char *input){
  if(*input=='\\'){
    return parse_escape(input);
  } else {
    wchar_t result[1];
    mbstate_t state;
    size_t len;
    int64_t nbytes;
    memset(&state,'\0',sizeof(state));
    int i;
    nbytes=strlen(input);
    if(0<=(nbytes=mbrtowc(result,input,strlen(input),&state))){
      return (wchar_t)result[0];
    } else {
      fprintf(stderr,"error lexing char\n");
      return L'\0';
    }
  }
}
int lex_char(char* cur_yytext,wint_t *new_char){
  char *p=cur_yytext;
  if(*p=='\\'){
    int retval=parse_escape_internal(p++,new_char);
    return (retval?retval:-1);
  }
  wchar_t result[1];
  mbstate_t state;
  size_t len;
  int64_t nbytes;
  memset(&state,'\0',sizeof(state));
  int i;
  nbytes=strlen(p);//mbrlen(cur_yytext+1,4,&state);
  /*  for(i=0;i<4;i++){
    fprintf(stderr,"%#0hhx",(p+i)[0]);
    }*/
  if(0<=(nbytes=mbrtowc(result,p,strlen(p),&state))){
    PRINT_FMT("%lc",*result);
    *new_char = (wchar_t)result[0];
    return strlen(p);
  } else {
    fprintf(stderr,"error lexing char\n");
    return -1;
  }
}
