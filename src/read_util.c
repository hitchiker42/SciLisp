#include "read.h"
#define HEXVALUE(c) \
  (((c) >= 'a' && (c) <= 'f') \
        ? (c)-'a'+10 \
        : (c) >= 'A' && (c) <= 'F' ? (c)-'A'+10 : (c)-'0')
//large ammounts of this taken from the bash printf builtin
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
    case '\\':
      return (wchar_t)'\\';
    //this is because lisp uses question marks to express characters
    case '?':
      return (wchar_t)'?';
    default:
      return L'\0';
  }
}
int parse_escape_internal(char *input,wchar_t* output){//input is the text immediately following a backslash
  *output=parse_simple_escape(*input);
  if(*output){
    return 1;
  }
  int temp=0;
  wint_t uvalue;
  char *p=input+1;
  switch(*input++){
    case 'x':{
      //note to self: --  (prefix or postfix) has higer precidence than &&
      for(temp=2,uvalue=0;isxdigit(*p) && temp--;p++){
        uvalue <<= 4;
        uvalue += HEXVALUE(*p);
      }
      if(p==input+1){
        fprintf(stderr,"error lexing char, expected hex digit after \\x\n");
        return 0;
      }
      *output=uvalue;
      return p-input;
    }
    case 'u':
      temp=4;
      for (uvalue = 0; isxdigit ((unsigned char)*p) && temp--; p++){
        uvalue <<= 4;
        uvalue += HEXVALUE(*p);
      }
      if (p == input + 1){
        fprintf(stderr,"error lexing char, expected hex digit after \\u\n");
        return 0;
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
    return L'\0';
  }
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
