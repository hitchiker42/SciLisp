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
static char temp_ustring[MB_LEN_MAX];
struct lisp_ustring {
  wchar_t *restrict str;
  uint32_t len;
};
union utf8_hack{
  char bytes[4];
  wchar_t wchar;
};
sexp lisp_char_to_string(sexp lisp_char){
  if(!CHARP(lisp_char)){
    return format_type_error("char->string","character",lisp_char.tag);
  } else {
    mbstate_t state;
    size_t nbytes;
    memset(&state,'\0',sizeof(state));
    nbytes=wcrtomb(temp_ustring,lisp_char.val.uchar,&state);
    if(nbytes==(size_t)-1){
      return error_sexp("conversion error in char->string");
    } else {
      char *retval=xmalloc(nbytes);
      memcpy(retval,temp_ustring,nbytes);
      return cord_sexp(retval);
    }
  }
}
/*
sexp lisp_string_to_char(sexp lisp_str){
  static_assert(0,error_check);
  wchar_t retval;
  mbstate state;
  memset(&state,'\0',sizeof(state));
} 
*/
union utf8_hack utf8_escape={.wchar=L'\0'};
wchar_t lex_char(char* cur_yytext){
  if(cur_yytext[1]=='\\'){
    if(cur_yytext[2]=='?'){return '?';}
    if(cur_yytext[2]=='x'){
      //without this if you gave \x0000 you'd segfault
      char byte[3]={cur_yytext[3],cur_yytext[4],'\0'};
      return strtol(byte,NULL,16);
    } else if(cur_yytext[2]=='u'){
      char byte1[3]={cur_yytext[3],cur_yytext[4],'\0'};
      char byte2[3]={cur_yytext[5],cur_yytext[6],'\0'};
#if __BYTE__ORDER__ ==  __ORDER_LITTLE_ENDIAN__
      utf8_escape.bytes[1]=(unsigned char)strtol(byte1,NULL,16);
      utf8_escape.bytes[0]=(unsigned char)strtol(byte2,NULL,16);
#elif __BYTE__ORDER__ == __ORDER_BIG__ENDIAN__
      utf8_escape.bytes[2]=(unsigned char)strtol(byte1,NULL,16);
      utf8_escape.bytes[3]=(unsigned char)strtol(byte2,NULL,16);
#else
      fprintf(stderr,"unknown byte order, exiting");
      exit(1);
#endif      
    }
    return utf8_escape.wchar;
  }
  wchar_t result[1];
  mbstate_t state;
  size_t len;
  int64_t nbytes;
  char* cvt_str=cur_yytext+1;;
  memset(&state,'\0',sizeof(state));
  int i;
  HERE();
  PRINT_FMT("%lc",*cur_yytext+1);
  //  nbytes=strlen(cur_yytext+1);//mbrlen(cur_yytext+1,4,&state);
  for(i=0;i<4;i++){
    fprintf(stderr,"%#0hhx",(cur_yytext+1+i)[0]);
  }
  if(0<=(nbytes=mbrtowc(result,cvt_str,strlen(cvt_str),&state))){
    puts("");
    PRINT_FMT("%lc",*result);
    return (wchar_t)result[0];
  } else {
    fprintf(stderr,"error lexing char, returning null\n");
    return (wchar_t)L'\0';
  }
}
 #if 0 
/* Copyright 2012
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * BSD License:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *   3. The name of the author may not be used to endorse or promote
 *      products derived from this software without specific prior
 *      written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
size_t utf8_from_uc(wchar_t *, const unsigned char *);
size_t utf8_from(wchar_t *, const char *);
size_t utf8_to_uc(unsigned char *, const wchar_t *);
size_t utf8_to(char *, const wchar_t *);
wchar_t *utf8_dup_from_uc(const unsigned char *);
wchar_t *utf8_dup_from(const char *);
char *utf8_dup_to(const wchar_t *);
unsigned char *utf8_dup_to_uc(const wchar_t *);
enum utf8_state { utf8_init, utf8_more1, utf8_more2, utf8_more3 };
typedef struct utf8_decoder {
  enum utf8_state state;
  wchar_t wch, wch_min;
  int head, tail, back;
  int buf[8];
} utf8_decoder_t;
int utf8_encode(wchar_t, int (*put)(int ch, void *ctx), void *ctx);
void utf8_decoder_init(utf8_decoder_t *);
wint_t utf8_decode(utf8_decoder_t *,int (*get)(void *ctx), void *ctx);
FILE *w_fopen(const wchar_t *, const wchar_t *);
#if WCHAR_MAX > 65535
#define FULL_UNICODE
#endif
#ifndef FULL_UNICODE
static void conversion_error(void) {
  fprintf(stderr,
          "encountered utf-8 character that needs full unicode support");
  longjmp(err_buf);
}
#endif
size_t utf8_from_uc(wchar_t *wdst, const unsigned char *src){
  size_t nchar = 1;
  enum utf8_state state = utf8_init;
  const unsigned char *backtrack = 0;
  wchar_t wch = 0, wch_min = 0;

  while(1){
    int ch = *src++;
    if (ch == 0) {
      if (state == utf8_init){
        break;
      }
      src = backtrack;
      if (wdst){
        *wdst++ = 0xDC00 | *src;
      }
      nchar++;
      state = utf8_init;
      continue;
    }

    switch (state) {
    case utf8_init:
      switch (ch >> 4) {
      case 0x0: case 0x1: case 0x2: case 0x3:
      case 0x4: case 0x5: case 0x6: case 0x7:
        if (wdst){
          *wdst++ = ch;
        }
        nchar++;
        break;
      case 0xC: case 0xD:
        state = utf8_more1;
        wch = (ch & 0x1F);
        wch_min = 0x80;
        break;
      case 0xE:
        state = utf8_more2;
        wch = (ch & 0xF);
        wch_min = 0x800;
        break;
      case 0xF:
#ifdef FULL_UNICODE
        if (ch < 0xF5) {
          state = utf8_more3;
          wch = (ch & 0x7);
          wch_min = 0x10000;
          break;
        }
        /* fallthrough */
#else
        conversion_error();
#endif
      default:
        if (wdst){
          *wdst++ = 0xDC00 | ch;
        }
        nchar++;
        break;
      }
      backtrack = src;
      break;
    case utf8_more1:
    case utf8_more2:
    case utf8_more3:
      if (ch >= 0x80 && ch < 0xC0) {
        wch <<= 6;
        wch |= (ch & 0x3F);
        state = (enum utf8_state) (state - 1);
        if (state == utf8_init) {
          if (wch < wch_min ||
              (wch <= 0xFFFF && (wch & 0xFF00) == 0xDC00) ||
              (wch > 0x10FFFF)){
            src = backtrack;
            if (wdst){
              *wdst++ = 0xDC00 | *src;
            }
          } else {
            if (wdst){
              *wdst++ = wch;
            }
          }
          nchar++;
        }
      } else {
        src = backtrack;
        if (wdst){
          *wdst++ = 0xDC00 | *src;
        }
        nchar++;
        state = utf8_init;
      }
      break;
    }
  }
  if (wdst){
    *wdst++ = 0;
  }
  return nchar;
}

size_t utf8_from(wchar_t *wdst, const char *src){
   return utf8_from_uc(wdst, (const unsigned char *) src);
}

size_t utf8_to_uc(unsigned char *dst, const wchar_t *wsrc){
  size_t nbyte = 1;
  wchar_t wch;
  while ((wch = *wsrc++)) {
    if (wch < 0x80) {
      nbyte += 1;
      if (dst){
        *dst++ = wch;
      }
    } else if (wch < 0x800) {
      nbyte += 2;
      if (dst) {
        *dst++ = 0xC0 | (wch >> 6);
        *dst++ = 0x80 | (wch & 0x3F);
      }
    } else if (wch < 0x10000) {
      if ((wch & 0xFF00) == 0xDC00) {
        nbyte += 1;
        if (dst){
          *dst++ = (wch & 0xFF);
        }
      } else {
        nbyte += 3;
        if (dst) {
          *dst++ = 0xE0 | (wch >> 12);
          *dst++ = 0x80 | ((wch >> 6) & 0x3F);
          *dst++ = 0x80 | (wch & 0x3F);
        }
      }
    } else if (wch < 0x110000) {
      nbyte += 4;
      if (dst) {
        *dst++ = 0xF0 | (wch >> 18);
        *dst++ = 0x80 | ((wch >> 12) & 0x3F);
        *dst++ = 0x80 | ((wch >> 6) & 0x3F);
        *dst++ = 0x80 | (wch & 0x3F);
      }
    }
  }

  if (dst){
    *dst++ = 0;
  }
  return nbyte;
}

size_t utf8_to(char *dst, const wchar_t *wsrc){
  return utf8_to_uc((unsigned char *) dst, wsrc);
}

wchar_t *utf8_dup_from_uc(const unsigned char *str){
  size_t nchar = utf8_from_uc(0, str);
  wchar_t *wstr = (wchar_t *) xmalloc(nchar * sizeof *wstr);
  utf8_from_uc(wstr, str);
  return wstr;
}

wchar_t *utf8_dup_from(const char *str){
  size_t nchar = utf8_from(0, str);
  wchar_t *wstr = (wchar_t *) xmalloc(nchar * sizeof *wstr);
  utf8_from(wstr, str);
  return wstr;
}

unsigned char *utf8_dup_to_uc(const wchar_t *wstr){
  size_t nbyte = utf8_to_uc(0, wstr);
  unsigned char *str = xmalloc(nbyte);
  utf8_to_uc(str, wstr);
  return str;
}

char *utf8_dup_to(const wchar_t *wstr){
  size_t nbyte = utf8_to(0, wstr);
  char *str = (char *) xmalloc(nbyte);
  utf8_to(str, wstr);
  return str;
}

int utf8_encode(wchar_t wch, int (*put)(int,uint64_t), void *ctx){
  if (wch < 0x80) {
    return put(wch, ctx);
  } else if (wch < 0x800) {
    return put(0xC0 | (wch >> 6), ctx) &&
           put(0x80 | (wch & 0x3F), ctx);
  } else if (wch < 0x10000) {
    if ((wch & 0xFF00) == 0xDC00) {
      return put(wch & 0xFF, ctx);
    } else {
      return put(0xE0 | (wch >> 12), ctx) &&
             put(0x80 | ((wch >> 6) & 0x3F), ctx) &&
             put(0x80 | (wch & 0x3F), ctx);
    }
  } else if (wch < 0x110000) {
    return put(0xF0 | (wch >> 18), ctx) &&
           put(0x80 | ((wch >> 12) & 0x3F), ctx) &&
           put(0x80 | ((wch >> 6) & 0x3F), ctx) &&
           put(0x80 | (wch & 0x3F), ctx);
  }
  return 0;
}

void utf8_decoder_init(utf8_decoder_t *ud){
  ud->state = utf8_init;
  ud->wch = 0;
  ud->head = ud->tail = ud->back = 0;
}

wint_t utf8_decode(utf8_decoder_t *ud, int (*get)(void *ctx), void *ctx){
  while(1){
    int ch;
    if (ud->tail != ud->head) {
      ch = ud->buf[ud->tail];
      ud->tail = (ud->tail + 1) % 8;
    } else {
      ch = get(ctx);
      ud->buf[ud->head] = ch;
      ud->head = ud->tail = (ud->head + 1) % 8;
    }

    if (ch == EOF) {
      if (ud->state == utf8_init) {
        return WEOF;
      } else {
        wchar_t wch = 0xDC00 | ud->buf[ud->back];
        ud->tail = ud->back = (ud->back + 1) % 8;
        ud->state = utf8_init;
        return wch;
      }
    }

    switch (ud->state) {
    case utf8_init:
      switch (ch >> 4) {
      case 0x0: case 0x1: case 0x2: case 0x3:
      case 0x4: case 0x5: case 0x6: case 0x7:
        ud->back = ud->tail;
        return ch;
      case 0xC: case 0xD:
        ud->state = utf8_more1;
        ud->wch = (ch & 0x1F);
        ud->wch_min = 0x80;
        break;
      case 0xE:
        ud->state = utf8_more2;
        ud->wch = (ch & 0xF);
        ud->wch_min = 0x800;
        break;
      case 0xF:
#ifdef FULL_UNICODE
        if (ch < 0xF5) {
          ud->state = utf8_more3;
          ud->wch = (ch & 0x7);
          ud->wch_min = 0x100000;
          break;
        }
        /* fallthrough */
#else
        conversion_error();
#endif
      default:
        ud->back = ud->tail;
        return 0xDC00 | ch;
      }
      break;
    case utf8_more1:
    case utf8_more2:
    case utf8_more3:
      if (ch >= 0x80 && ch < 0xC0) {
        ud->wch <<= 6;
        ud->wch |= (ch & 0x3F);
        ud->state = (enum utf8_state) (ud->state - 1);
        if (ud->state == utf8_init) {
          if (ud->wch < ud->wch_min || 
              (ud->wch <= 0xFFFF && (ud->wch & 0xFF00) == 0xDC00) ||
              (ud->wch > 0x10FFFF))
          {
            wchar_t wch = 0xDC00 | ud->buf[ud->back];
            ud->tail = ud->back = (ud->back + 1) % 8;
            return wch;
          } else {
            ud->back = ud->tail;
            return ud->wch;
          }
        }
      } else {
        wchar_t wch = 0xDC00 | ud->buf[ud->back];
        ud->tail = ud->back = (ud->back + 1) % 8;
        ud->state = utf8_init;
        return wch;
      }
      break;
    }
  }
}
#endif
