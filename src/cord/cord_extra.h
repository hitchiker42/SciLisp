
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
//This file is not part of SciLisp, it's just some extra cord macros
#ifndef CORD_H
#error "Don't include cord_extra.h directly, use cord.h instead"
#endif
#include <stdint.h>
#define CORD_strdup(str) CORD_from_char_star(str)
#define CORD_append(val,ext) val=CORD_cat(val,ext)
#define CORD_cat_line(cord1,cord2) CORD_catn(3,cord1,cord2,"\n")
#define CORD_append_line(val,ext) val=CORD_cat_line(val,ext)
//this needs to be a macro since sring literals have an array type
//and decay to pointers when passed to functions
#define CORD_cat_literal(cord,string_literal)                   \
  CORD_cat_char_star(cord,string_literal,sizeof(string_literal)-1)
//does the same job as the macro below, but in a function
//CORD CORD_asprintf(CORD format,...);
#define CORD_asprintf(format,args...)           \
  ({CORD retval;                                \
    CORD_sprintf(&retval,format,##args);        \
    retval;})

size_t CORD_pos_span(CORD_pos pos,const char *accept);
size_t CORD_span(CORD s,const char *accept);
#define CORD_fputs CORD_put
#define CORD_puts(s) CORD_put(CORD_cat(s,"\n"),stdout)
uint32_t CORD_equal(CORD str1,CORD str2);
