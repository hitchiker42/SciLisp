#ifndef __COMMON_H__
#define __COMMON_H__
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <string.h>
#include <uthash.h>
#include <wchar.h>
#include <unistd.h>
#include <stdarg.h>
#define HERE_ON
#if defined (HERE_ON)
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define HERE_MSG(string) fprintf(stderr,"here at %s,line %d\n%s\n"\
                                 ,__FILE__,__LINE__,string)
#define PRINT_MSG(string) fputs(string,stderr);fputs("\n",stderr)
#else
#define HERE()
#define HERE_MSG(string)
#define PRINT_MSG(string)
#endif
#define my_error(str,fmt...) fprintf(stderr,str,fmt);abort()
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
typedef enum _tag _tag;
typedef union data data;
typedef struct sexp sexp;
typedef struct cons cons;
typedef struct symref symref;
typedef data YYSTYPE;
typedef const sexp(*sexp_binop)(sexp,sexp);
extern symref* symbolTable;
enum _tag {
  _nil = -1,
  _cons = 0,
  _double = 1,
  _long = 2,
  _char = 3,
  _str = 4,
  _fun = 5,
  _sym = 6
};
union data {
  double real64;
  long int64;
  wchar_t utf8_char;
  char* string;
  cons* pair;
  symref* var;//incldues functions
  void* fun;
};
struct sexp{
  _tag tag;
  data val;
};
struct symref{
  char* name;
  sexp val;
  UT_hash_handle hh;
};
struct cons{
  sexp car;
  sexp cdr;
};
#define addSym(Var)\
  HASH_ADD_KEYPTR(hh, symbolTable, Var->name, strlen(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
#define getSym(name,Var)\
  HASH_FIND_STR(symbolTable,name,Var)
static const sexp NIL={-1,0};
extern sexp mklist(sexp head,...);
extern sexp mkImproper(sexp head,...);
#endif
