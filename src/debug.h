/* Header file defining debug macros, if DEBUG is undefined
   or NDEBUG is defined the macro are noops

   Copyright (C) 2013-2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
#include <assert.h>
#include <execinfo.h>
#if (defined (DEBUG)) && !(defined (NDEBUG))
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

/*
  static assert, there's compiler support for static_asserts, but I like
  mine, since you can specify something of an error message(as long as
  your message is a valid c identifier)
 */
#ifdef static_assert
#undef static_assert
#endif
#define static_assert(val,error)                \
  ({static const char error[(val)?1:-1];;})
/*
  assert that variable is of the type type, or can be
  cast to it. I can't really think of a way to prevent
  implicit casting, but in most use cases (i.e pointer casts)
  there should at least be a comiler warning

 */
#define type_assert(type,variable)              \
  ({type error_test=variable;;})
#define size_assert(size,value)                 \
  ({static const char error[(sizeof(value)==size)?1:-1];;})
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
/*
  The debugging rountines are parameterised by a print function, allowing debugging
  to be turned off at runtimes (although it won't change the performance)
 */
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
#include "error.h"
//not quite debugging but it fits best here
#define return_errno(fn_name)                                           \
  int ___errsave=errno;                                                 \
  char* ___errmsg=strerror(errno);                                      \
  CORD ___errorstr;                                                     \
  CORD_sprintf(&___errorstr,"%s failed with error number %d, %s",fn_name,___errsave,___errmsg); \
  return error_sexp(___errorstr)

static const char * lisp_strerror(int errnum){
  //I think this works, but I need to test to make sure
  if(errnum>=1&&errnum<=133){//errnum is aliased to a c errno value
    char *buf;
    size_t buflen=0;
    strerror_r(errnum,buf,buflen);
    return buf;
  } else {
    switch(errnum){
      case ELISP_STACK_OVERFLOW:
        return "Lisp stack overflow";
      default:
        return "Lisp specific error numbers are currently undocumented";
    }
  }
}
static CORD get_gc_info(){
  struct GC_prof_stats_s* stats=alloca(sizeof(struct GC_prof_stats_s));
  GC_get_prof_stats(stats,sizeof(struct GC_prof_stats_s));
  CORD stats_str;
  CORD_sprintf(&stats_str,"GC Stats:\nHeap Size: %lu\nFree Bytes %lu\n"
               "Unmapped Bytes: %lu\nBytes Allocated Since Last GC: %lu\n"
               "Bytes Allocated Before Last GC: %lu\nNon GC Bytes: %lu\n"
               "GC Cycle Number: %lu\nGC Marker Threads: %lu\n"
               "Bytes Reclaimed By Last GC:%lu\n"
               "Bytes Reclaimed Before Last GC: %lu", stats->heapsize_full,
               stats->free_bytes_full,stats->unmapped_bytes,
               stats->bytes_allocd_since_gc,stats->allocd_bytes_before_gc,
               stats->non_gc_bytes,stats->gc_no,stats->markers_m1,
               stats->bytes_reclaimed_since_gc,
               stats->reclaimed_bytes_before_gc);
  return stats_str;
}
#define PRINT_GC_INFO() (CORD_debug_printf(get_gc_info()))
