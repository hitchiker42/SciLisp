#include <assert.h>
#include <execinfo.h>
#if defined (HERE_ON) && !(defined (HERE_OFF))\
  || (defined (DEBUG)) && !(defined (NDEBUG))
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define HERE_MSG(string) fprintf(stderr,"here at %s,line %d\n%s\n"\
                                 ,__FILE__,__LINE__,string)
#define PRINT_MSG(string) CORD_fprintf(stderr,string);fputs("\n",stderr)
#define PRINT_FMT(string,fmt...) CORD_fprintf(stderr,string,##fmt);fputs("\n",stderr)
#else
#define HERE()
#define HERE_MSG(string)
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#endif
#if defined (VERBOSE_LEXING) && !(defined (QUIET_LEXING))
#define LEX_MSG(string) fputs(string,stderr);fputs("\n",stderr)
#define LEX_FMT(string,fmt...) fprintf(stderr,string,##fmt);fputs("\n",stderr)
#else
#define LEX_MSG(string)
#define LEX_FMT(string,fmt...)
#endif
#define format_error_str(format,args...) CORD_sprintf(&error_str,format,##args)
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
