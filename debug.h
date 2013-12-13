/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include <assert.h>
#include <execinfo.h>
#if defined (HERE_ON) && !(defined (HERE_OFF))\
  || (defined (DEBUG)) && !(defined (NDEBUG))
#ifndef DEBUG
#define DEBUG
#endif
#define HERE() debug_printf("here at %s,line %d\n",__FILE__,__LINE__)
#define HERE_MSG(string) debug_printf("here at %s,line %d\n%s\n"\
                                 ,__FILE__,__LINE__,string)
#define HERE_FMT(string,fmt...) debug_printf(string "\n",##fmt);HERE()
#define PRINT_MSG(string) CORD_debug_printf(CORD_cat(string,"\n"))
#define PRINT_FMT(string,fmt...) CORD_debug_printf(CORD_cat(string,"\n"),##fmt)
#else
#define HERE()
#define HERE_MSG(string)
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#endif
#if defined (VERBOSE_LEXING) && !(defined (QUIET_LEXING))
#define LEX_MSG(string) fputs(string,stderr);fputs("\n",stderr)
#define LEX_FMT(string,fmt...) fprintf(stderr,string "\n",##fmt)
#else
#define LEX_MSG(string)
#define LEX_FMT(string,fmt...)
#endif
#ifdef static_assert
#undef static_assert
#endif
#define static_assert(val,error)                \
  static const char error[(val)?1:-1]
static void print_trace(void){
  void *array[50];
  size_t size,i;
  char** strings;
  size = backtrace(array,50);
  strings = backtrace_symbols(array,size);
  printf("obtained %zd stack frames. \n", size);
  for(i=0;i<size;i++){
    printf("%s\n",strings[i]);
  }
  free(strings);
}
static void CORD_ndebug_printf(CORD fmt __attribute__((unused)),...){
  return;
}
static void ndebug_printf(CORD fmt __attribute__((unused)),...){
  return;
}
static void default_CORD_debug_printf(CORD fmt,...){
  va_list ap;
  va_start(ap,fmt);
  CORD_vfprintf(stderr,fmt,ap);
  return;
}
static void default_debug_printf(CORD fmt,...){
  va_list ap;
  va_start(ap,fmt);
  vfprintf(stderr,fmt,ap);
  return;
}
//not quite debugging but it fits best here
#define return_errno(fn_name)                                           \
  int ___errsave=errno;                                                 \
  char* ___errmsg=strerror(errno);                                      \
  CORD ___errorstr;                                                     \
  CORD_sprintf(&___errorstr,"%s failed with error number %d, %s",fn_name,___errsave,___errmsg); \
  return error_sexp(___errorstr)
#define CAT(x,rest...) x##rest
