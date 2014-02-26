/*
 * Copyright (C) 2013-2014 Tucker DiNapoli
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */
//This file is not part of SciLisp, it's just some extra cord functions
//written along the lines of cordxtra.c
#include "cord.h"
#include <stdint.h>
//equivlent to asprintf, but with cords
#ifdef CORD_asprintf
#pragma push_macro("CORD_asprintf")
#undef CORD_asprintf
CORD CORD_asprintf(CORD format, ...){
  va_list ap;
  va_start(ap,format);
  CORD retval=0;
  int result = CORD_vsprintf(&retval,format,ap);
  va_end(ap);
  if(result < 0){
    return NULL;
  }
  return retval;
}
#pragma pop_macro("CORD_asprintf")
#endif
//move these somewhere else
//functions to convert an extendable cord to a lisp or c string
//optimizing the case where less that CORD_BUFSZ chars have
//been read into the ec cord
/*
lisp_string *CORD_ec_to_lisp_string(CORD_ec buf,uint32_t len,int mb){
  lisp_string *retval;
  if(buf[0].ec_cord){
    retval=xmalloc(sizeof(lisp_string));
    retval->cord=CORD_ec_to_cord(buf);
  } else {
    retval=xmalloc_atomic(sizeof(lisp_string)+len);
    retval->string=retval+sizeof(lisp_string);
    memcpy(retval->string,buf[0].ec_buf,len);
  }
  retval->len=len;
  retval->multibyte=mb;
  return retval;
  }*/
char *CORD_ec_to_char_star(CORD_ec buf){
  if(buf[0].ec_cord){
    return (char*)CORD_to_char_star(CORD_ec_to_cord(buf));
  } else {
    int len = buf->ec_bufptr-buf->ec_buf;
    char *retval = GC_malloc_atomic(len+1);
    if(!retval){
      ABORT("Out of memory");
    }
    memcpy(retval,buf[0].ec_buf,len);
    retval[len]='\0';
    return retval;
  }
}
//analogue of strspn
size_t CORD_span(CORD s,const char *accept){
  //create table of 256 bytes with the bytes corrsponding to
  //characters in accept set to 1 and all others set to 0
  CORD_pos pos;
  CORD_set_pos(pos,s,0);
  return CORD_pos_span(pos,accept);
}
size_t CORD_pos_span(CORD_pos pos,const char *accept){
  uint8_t flags[256]={0};
  size_t len;
  while(*accept){
    flags[*accept++]=1;
  }
  char c;
  while(CORD_pos_valid(pos)){
    if(flags[CORD_pos_fetch(pos)]){
      CORD_next(pos);
      len++;
    } else {
      break;
    }
  }
  return len;
}
/* Closer analogues to strchr/strrchr than CORD_chr/CORD_rchr,
   the versions which act on positions allow for searching from
   arbitary positinons, and the versions which act on CORDs are
   just shortcuts
 */
CORD CORD_pos_strchr(CORD_pos p,char c){
  while(CORD_pos_valid(p)){
    if(CORD_pos_fetch(s) == c){
      break;
    }
    CORD_next(p);
  }
  if(CORD_pos_valid(p)){
    return CORD_pos_to_cord(p);
  } else {
    return 0;
  }
}

CORD CORD_pos_strrchr(CORD_pos p,char c){
  while(CORD_pos_valid(p)){
    if(CORD_pos_fetch(s) == c){
      break;
    }
    CORD_prev(p);
  }
  if(CORD_pos_valid(p)){
    return CORD_pos_to_cord(p);
  } else {
    return 0;
  }
}
CORD CORD_strchr(CORD s,char c){
  CORD_pos p;
  CORD_set_pos(p,s,0);
  return CORD_pos_strchr(p,c):
}
CORD CORD_strrchr(CORD s,char c){
  CORD_pos p;
  CORD_set_pos(p,s,CORD_len(s));
  return CORD_pos_strchr(p,c):
}
CORD CORD_trim(CORD s,char *remove){
  uint8_t flags[256]={0};
  while(*remove){
    flags[*remove++]=1;
  }
 CORD_pos p;
  CORD_set_pos(p,s,0);
  while(CORD_pos_valid(p)){
    if(flags[CORD_pos_fetch(p)]){
      break;
    } else {
      CORD_next(p);
    }
  }
  if(!CORD_pos_valid(p)){
    return 0;
  }
  size_t start=CORD_pos_to_index(p);
  CORD_set_pos(p,s,CORD_len(s));
  size_t len=start+1;
  while(CORD_pos_valid(p)){
    if(flags[CORD_pos_fetch(p)]){
      len=CORD_pos_to_index(p);//maybe +1;
      break;
    } else {
      CORD_next(p);
    }
  }
  return CORD_substr(s,start,len-start);
}
CORD CORD_strip(CORD s){
  return CORD_trim(s," \t\n");
}
#define ascii_upcase(c) ({char _c = c;          \
      (_c>=0x61&&_c<=0x7A)?_c-0x20:_c;})
#define ascii_downcase(c) ({char _c = c;        \
      (_c>=0x41&&_c<=0x5A)?_c+0x20:_c;})
struct cord_iter_data {
  char *result;
  uint32_t index;
  //  char(*f)(char);
};
int upcase_iter(char c,void *client_data){
  struct cord_iter_data *data=(struct cord_iter_data *)client_data;
  data->result[data->index++]=ascii_upcase(c);
  return 0;
}
int upcase_batched_iter(char *s,void *client_data){
  struct cord_iter_data *data=(struct cord_iter_data *)client_data;
  int i=data->index;
  while(*s){
    data->result[i++]=ascii_upcase(*s++);
  }
  data->index=i;
  return 0;
}
int downcase_iter(char c,void *client_data){
  struct cord_iter_data *data=(struct cord_iter_data *)client_data;
  data->result[data->index++]=ascii_downcase(c);
  return 0;
}
int downcase_batched_iter(char *s,void *client_data){
  struct cord_iter_data *data=(struct cord_iter_data *)client_data;
  int i=data->index;
  while(*s){
    data->result[i++]=ascii_downcase(*s++);
  }
  data->index=i;
  return 0;
}
//slightly different than CORD_cmp, assumes one of str1 or str2
//isn't a c string so we need to use cord positions, and assumes
//str1 and str2 are non null, finally returns 1 if equal and 0 othrewise
uint32_t CORD_equal(CORD str1,CORD str2){
  CORD_pos xpos;
  CORD_pos ypos;
  register size_t avail, yavail;
  CORD_set_pos(xpos, x, 0);
  CORD_set_pos(ypos, y, 0);
  for(;;) {
    if (!CORD_pos_valid(xpos)) {
      if (CORD_pos_valid(ypos)) {
        return 0;
      } else {
        return 1;
      }
    }
    if (!CORD_pos_valid(ypos)) {
      return 0;
    }
    if ((avail = CORD_pos_chars_left(xpos)) <= 0
        || (yavail = CORD_pos_chars_left(ypos)) <= 0) {
      register char xcurrent = CORD_pos_fetch(xpos);
      register char ycurrent = CORD_pos_fetch(ypos);
      if (xcurrent != ycurrent) return(0);
      CORD_next(xpos);
      CORD_next(ypos);
    } else {
      /* process as many characters as we can */
      register int result;

      if (avail > yavail) avail = yavail;
      result = strncmp(CORD_pos_cur_char_addr(xpos),
                       CORD_pos_cur_char_addr(ypos), avail);
      if (result != 0) return(0);
      CORD_pos_advance(xpos, avail);
      CORD_pos_advance(ypos, avail);
    }
  }
}
#define CORD_stream_bufsize 128

#if 0
//flush current buf into cord and refil buf
void CORD_stream_rw_flush_buf(CORD_stream_rw *x){
  register size_t len = x->bufptr - x->buf;
  char * s;
  if (len == 0) return;
  s = GC_MALLOC_ATOMIC(len+1);
  memcpy(s, x->buf, len);
  s[len] = '\0';
  CORD start=CORD_substr(x->cord,0,x->buf_start);
  CORD end=CORD_substr(x->cord,x->index,CORD_len(x->cord)-s->index);
  x->cord = CORD_cat_char_star(start, s, len);
  x->cord = CORD_cat(x->cord,end);
  x->bufptr = x->buf;
}
void CORD_stream_rw_refill_buf(CORD_stream_rw *x){
  register size_t len = x->bufptr - x->buf;
  char * s;
  if (len == 0) return;
  s = GC_MALLOC_ATOMIC(len+1);
  memcpy(s, x->buf, len);
  s[len] = '\0';
  CORD start=CORD_substr(x->cord,0,x->buf_start);
  CORD end=CORD_substr(x->cord,x->index,CORD_len(x->cord)-s->index);
  x->cord = CORD_cat_char_star(start, s, len);
  x->cord = CORD_cat(x->cord,end);
  x->bufptr = x->buf;
  int bufsize=MAX(CORD_stream_bufsize,CORD_len(end));
  memcpy(x->buf,CORD_to_char_star(CORD_substr(end,0,bufsize)),
         CORD_stream_bufsize);
  memset(x->buf+bufsize,'\0',CORD_stream_bufsize-bufsize);
  s->buf_start=x->index;
}
struct CORD_stream_rw {
  CORD cord;
  char *bufptr;
  char buf[CORD_stream_bufsize+1];
  uint32_t index;//always refers to current index in actual cord
  uint32_t buf_start;//index that the current buffer starts at
  int fd;//possible backing file
};
#define maybe_refill_buf(s)                     \
  (s->bufptr-s->buf>=CORD_stream_bufsize?       \
   CORD_stream_rw_refill_buf(s),0:0)
char CORD_stream_rw_read_char(CORD_stream_rw *s){
  maybe_refill_buf(s);
  return s->bufptr[s->index++];
}
void CORD_stream_rw_write_char(CORD_stream_rw *s,char c){
  maybe_refill_buf(s);
  s->bufptr[s->index++]=c;
}
void CORD_stream_rw_write(CORD_stream_rw *s,char *buf,size_t sz){
  CORD_stream_rw_flush_buf(s);
  register size_t len = s->bufptr - s->buf;
  char * str;
  if (len == 0) return;
  str = GC_MALLOC_ATOMIC(len+1);
  memcpy(str, s->buf, len);
  CORD start=CORD_substr(s->cord,0,s->buf_start);
  if(s->index+sz>=CORD_len(s->cord)){
    s->cord=CORD_cat_char_star(start,str,len);
    s->cord=CORD_cat_char_star(s->cord,buf,sz);
  } else {
    CORD end=CORD_substr(s->cord,s->index+sz,CORD_len(s->cord));
    s->cord=CORD_cat_char_star(start,str,len);
    s->cord=CORD_cat_char_star(s->cord,buf,sz);
    s->cord=CORD_cat(s->cord,end);
  }
}  
#endif
