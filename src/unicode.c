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
union utf8_hack{
  char bytes[4];
  wchar_t wchar;
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
//large ammounts of this taken from the bash printf builtin
union utf8_hack utf8_escape;
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
          uvalue = (uvalue*16) + HEXVALUE(*p);
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
          for (uvalue = 0; isxdigit ((unsigned char)*p) && temp--; p++)
            uvalue = (uvalue * 16) + HEXVALUE (*p);
          if (p == cur_yytext + 2){
            fprintf(stderr,"error lexing char, expected hex digit after \\u\n");
            return -1;
          }
              /*              builtin_error (_("missing unicode digit for \\%c"), c);
              *cp = '\\';
              return 0;*/
          *new_char=uvalue;
          return p-cur_yytext;
          /*          if (uvalue <= UCHAR_MAX){
            *cp = uvalue;
          }
          else{
            temp = u32cconv (uvalue, cp);
              cp[temp] = '\0';
              if (lenp)
                *lenp = temp;
                }*/
          /*
      HERE();
      char byte1[3]={cur_yytext[3],cur_yytext[4],'\0'};
      char byte2[3]={cur_yytext[5],cur_yytext[6],'\0'};
#if __BYTE_ORDER ==  __LITTLE_ENDIAN
      HERE();
      PRINT_FMT("byte 1 %s",byte1);
      PRINT_FMT("byte 2 %s",byte2);
      utf8_escape.bytes[1]=(unsigned char)strtoul(byte1,NULL,16);
      utf8_escape.bytes[0]=(unsigned char)strtoul(byte2,NULL,16);
#elif __BYTE_ORDER == __BIG_ENDIAN
      HERE();
      utf8_escape.bytes[2]=(unsigned char)strtol(byte1,NULL,16);
      utf8_escape.bytes[3]=(unsigned char)strtol(byte2,NULL,16);
#else
      HERE();
      fprintf(stderr,"unknown byte order, exiting");
      exit(1);
#endif
return utf8_escape.wchar;*/
      case 'n':
        *new_char=(wchar_t) '\n';
        return 2;
      case 't':
        *new_char=(wchar_t) '\t';
        return 2;
      case '\\':
        *new_char=(wchar_t) '\\';
        return 2;
        //I'll add the rest later
      default:
        p--;
        break;
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
  //  nbytes=strlen(cur_yytext+1);//mbrlen(cur_yytext+1,4,&state);
  for(i=0;i<4;i++){
    fprintf(stderr,"%#0hhx",(p+i)[0]);
  }
  if(0<=(nbytes=mbrtowc(result,p,strlen(p),&state))){
    puts("");
    PRINT_FMT("%lc",*result);
    *new_char = (wchar_t)result[0];
    return strlen(p);
  } else {
    fprintf(stderr,"error lexing char\n");
    return -1;
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
