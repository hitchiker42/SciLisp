#include <string.h>
#include <wchar.h>
#include <stdio.h>
#include <stdlib.h>
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);fputs("\n",stderr)
union utf8_hack{
  char bytes[2];
  wchar_t wchar;
};
union utf8_hack utf8_escape={.wchar=L'\0'};
static wchar_t lex_char(char* cur_yytext){
  wchar_t result[1];
  mbstate_t state;
  size_t len,nbytes;
  char* cvt_str;
  memset(&state,'\0',sizeof(state));
  /* this is so horrendously non portable it's kinda funny,
     it relies on the size of chars,specific input and output encoding
     and little endian byte ordering*/
  if(cur_yytext[1]=='\\'){
    if(cur_yytext[2]=='?'){return '?';}
    if(cur_yytext[2]=='x'){
      //without this if you gave \x0000 you'd segfault
      char byte[3]={cur_yytext[3],cur_yytext[4],'\0'};
      utf8_escape.bytes[1]=0x00;
      utf8_escape.bytes[0]=(unsigned char)strtol(byte,NULL,16);
    } else if(cur_yytext[2]=='u'){
      char byte1[3]={cur_yytext[3],cur_yytext[4],'\0'};
      char byte2[3]={cur_yytext[5],cur_yytext[6],'\0'};
      utf8_escape.bytes[1]=(unsigned char)strtol(byte1,NULL,16);
      utf8_escape.bytes[0]=(unsigned char)strtol(byte2,NULL,16);
    }
    /*} else {
    utf8_escape.bytes[1]=0x00;
    utf8_escape.bytes[0]=cur_yytext[1];
    }*/
    PRINT_FMT("%lc",utf8_escape.wchar);
    return utf8_escape.wchar;
  } else {
      cvt_str=cur_yytext+1;
  } 
  if(0<=mbrtowc(result,cvt_str,strlen(cvt_str),&state)){
    return (wchar_t)result[0];
  } else {
    fprintf(stderr,"error lexing char, returning null\n");
    return (wchar_t)L'\0';
  }
}
