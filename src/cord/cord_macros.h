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
#define CORD_strdup(str) CORD_from_char_star(str)
#define CORD_append(val,ext) val=CORD_cat(val,ext)
#define CORD_cat_line(cord1,cord2) CORD_catn(3,cord1,cord2,"\n")
#define CORD_append_line(val,ext) val=CORD_cat_line(val,ext)
//also defined as a function in cord_extra.c
#define CORD_asprintf(format,args...)           \
  ({CORD retval;                                \
  CORD_sprintf(&retval,format,##args);          \
  retval;})

