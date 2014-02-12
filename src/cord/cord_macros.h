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

//macros for treating a CORD_pos as a stream
//I'm not sure what happend if you fetch from an invalid
//CORD_pos, if it returns eof then I don't need to
//do anything special, if not I need to check if it's valid
//then if not return eof
#define CORD_getc(pos)                          \
  ({char c=CORD_pos_fetch(pos);                 \
  CORD_next(pos);                               \
  c;})
#define CORD_peek(pos) (CORD_pos_fetch(pos))
#define CORD_ungetc(pos) (CORD_prev(pos))
#define CORD_get_str(pos,n)                     \
  ({char *str=xmalloc_atomic(n);                \
    int i=0;                                    \
    while(i<n && CORD_pos_valid(pos)){          \
      str[i++]=CORD_fetch(pos);                 \
      CORD_next(pos);                           \
    }                                           \
    str;})
#define CORD_eat_line(pos)                      \
  ({while(CORD_pos_valid(pos)){                 \
      if(CORD_pos_fetch(pos) == '\n'){          \
        break;                                  \
      }                                         \
      CORD_next(pos);                           \
    }                                           \
    ;})
//takes some advantage of cords, I think
#define CORD_get_line(pos)                      \
  ({CORD line=CORD_pos_to_cord(pos);            \
  int len;                                      \
  while(CORD_pos_valid(pos)){                   \
    if(CORD_pos_fetch(pos) == '\n'){            \
      break;                                    \
    }                                           \
    CORD_next(pos);                             \
    len++;                                      \
  }                                             \
  line=CORD_substr(line,0,len);                 \
  CORD_to_const_char_star(line);})

