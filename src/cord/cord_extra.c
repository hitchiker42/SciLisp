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
//equivlent to asprintf, but with cords
CORD CORD_asprintf(CORD format, ...){
  va_list ap;
  va_start(ap,format);
  CORD retval=0;
  result = CORD_vsprintf(&retval,format,ap);
  va_end(ap);
  if(result < 0){
    return NULL;
  }
  return retval;
}
  
#define CORD_asprintf(format,args...)           \
  ({CORD retval;                                \
  CORD_sprintf(&retval,format,##args);          \
  retval;})
//move these somewhere else
//functions to convert an extendable cord to a lisp or c string
//optimizing the case where less that CORD_BUFSZ chars have
//been read into the ec cord
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
}
char *CORD_ec_to_char_star(CORD_ec buf){
  if(buf[0].ec_cord){
    return CORD_to_const_char_star(CORD_ec_to_cord(buf));
  } else {
    int len = buf[0].ec_bufptr-buf[0].ec_buf+1;
    char *retval=xmalloc_atomic(len);
    memcpy(retval,buf[0].ec_buf,len);
    return netval;
  }
}
