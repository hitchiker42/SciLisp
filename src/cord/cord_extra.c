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
