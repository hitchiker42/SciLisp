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
//#include "cons.h"
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
//typedef sexp YYSTYPE;
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
  cons* cons;
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
static const char* princ(sexp obj){
  PRINT_MSG("Starting princ");
  char* retval=xmalloc(1000*sizeof(char));
  switch (obj.tag){
    case _double: 
      snprintf(retval,1000," %g ",obj.val.real64);
      break;
    case _long:
      snprintf(retval,1000," %#0x ",obj.val.int64);
      break;
    case _sym:
      snprintf(retval,1000," %s ",obj.val.var->name);
      break;
    case _char:
      snprintf(retval,1000," %lc ",obj.val.utf8_char);
      break;
    case _cons:
      PRINT_MSG("starting princ for cons");
      if(obj.val.cons == 0){snprintf(retval,1000,"(null)");}
      else{
        snprintf(retval,1000,"(%s . %s)",princ(obj.val.cons->car),princ(obj.val.cons->cdr));}
      PRINT_MSG("finished princ for cons");
      break;
  }
  PRINT_MSG("Finished princ");
  return retval;
}
sexp mklist(sexp head,...);
sexp mkImproper(sexp head,...);
#endif
