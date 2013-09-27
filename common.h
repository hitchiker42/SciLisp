#ifndef __COMMON_H__
#define __COMMON_H__
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <string.h>
#include <uthash.h>
#include <wchar.h>
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define my_error(str,...fmt) fprintf(stderr,str,fmt);abort()
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
#define _GNU_SOURCE//might need to include on command line, for asnprintf
typedef enum _tag _tag;
typedef union data data;
typedef struct sexp sexp;
typedef struct cons cons
typedef struct symref symref;
extern symref* symbolTable;
struct symref{
  char* name;
  datatype val;
  UT_hash_handle hh;
};
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
  wchar utf8_char;
  char* string;
  cons* pair;
  symref* var;//incldues functions
};
structure sexp{
  data val;
  _tag tag;
};
structure cons{
  sexp car;
  sexp cdr;
} 
static inline void addSym(symref* Var){
  HASH_ADD_KEYPTR(hh, symbolTable, Var->name, strlen(Var->name), Var);
}
static inline void getSym(const char* name,symref* Var){
  HASH_FIND_STR(symbolTable,name,Var);
}
#endif
