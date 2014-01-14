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
#define HEXVALUE(c) \
  (((c) >= 'a' && (c) <= 'f') \
        ? (c)-'a'+10 \
        : (c) >= 'A' && (c) <= 'F' ? (c)-'A'+10 : (c)-'0')
static char temp_ustring[MB_LEN_MAX];
struct lisp_ustring {
  wchar_t *restrict str;
  uint32_t len;
};
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
  wchar_t retval;
  mbstate_t state;
  size_t nbytes;
  memset(&state,'\0',sizeof(state));
  //we need at most 4 bytes, if the cord in lisp_str is say 100 bytes
  //CORD_to_const_char_star would need to process 96 excess bytes
  //presumably running CORD_substr doesn't take too long, so this
  //should be much more efficient
  const char *mb_str=CORD_to_const_char_star
    (CORD_substr(lisp_str.val.cord,0,4));
  nbytes=mbrtowc(&retval,mb_str,4,&state);
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
//large ammounts of this taken from the bash printf builtin
union utf8_hack utf8_escape;
static inline wchar_t parse_simple_escape(char escape_char){
  //shamelessly stolen from emacs out of shear lazyness
  switch(escape_char){
    case 'a':
      return (wchar_t)'\007';
    case 'b':
      return (wchar_t)'\b';
    case 'd':
      return (wchar_t)0177;
    case 'e':
      return (wchar_t)033;
    case 'f':
      return (wchar_t)'\f';
    case 'n':
      return (wchar_t)'\n';
    case 'r':
      return (wchar_t)'\r';
    case 't':
      return (wchar_t)'\t';
    case 'v':
      return (wchar_t)'\v';
    case '\\'
      return (wchar_t)'\\';
    default:
      return L'\0';
  }
}

int lex_char(char* cur_yytext,wint_t *new_char){
  utf8_escape.wchar=L'\0';
  int temp=0;
  char *p=cur_yytext;
  wint_t uvalue;
  if(*p=='\\'){
    p++;
    switch(*p++){
      case '?':{
        *new_char = '?';
        return 2;
      }
      case 'x':{
        //note to self: --  (prefix or postfix) has higer precidence than &&
        for(temp=2,uvalue=0;isxdigit(*p) && temp--;p++){
          uvalue <<= 4;
          uvalue += HEXVALUE(*p);
        }
        if(p==cur_yytext+2){
          fprintf(stderr,"error lexing char, expected hex digit after \\x\n");
          return -1;
        }
        *new_char=uvalue;
        return p-cur_yytext;
      }
      case 'U':
        temp=8;
      case 'u':
        temp = (temp) ? 4 : 8;      /* \uNNNN \UNNNNNNNN */
        wint_t uvalue;
        for (uvalue = 0; isxdigit ((unsigned char)*p) && temp--; p++){
            uvalue <<= 4;
            uvalue += HEXVALUE(*p);
        }
        if (p == cur_yytext + 2){
          fprintf(stderr,"error lexing char, expected hex digit after \\u\n");
          return -1;
        }
        *new_char=uvalue;
        return p-cur_yytext;
      default:
        //we need to decrement p to get the character we're switching on
        //if new char isn't 0 we can just return 2, and if it is 0 we'd
        //need to decrement p anyway
        *new_char=parse_simple_escape(*--p);
        if(*new_char){
          return 2;
        } else {
          break;
        }
    }
  }
  wchar_t result[1];
  mbstate_t state;
  size_t len;
  int64_t nbytes;
  memset(&state,'\0',sizeof(state));
  int i;
  HERE();
  PRINT_FMT("%lc",*p);
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
  
