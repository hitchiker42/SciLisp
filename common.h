#ifndef __COMMON_H__
#define __COMMON_H__
#include <stdio.h>
#include <stdlib.h>
#define GC_DEBUG
#include <gc.h>
#include <string.h>
#include <uthash.h>
#include <wchar.h>
#include <unistd.h>
#include <stdarg.h>
#include <setjmp.h>
//#include "cons.h"
#define HERE_ON
#define VERBOSE_LEXING
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
#if defined (VERBOSE_LEXING)
#define LEX_MSG(string) fputs(string,stderr);fputs("\n",stderr)
#else
#define LEX_MSG(string)
#endif
#define my_abort(str,fmt...) fprintf(stderr,str,##fmt);abort()
#define my_err(str,fmt...) fprintf(stderr,str,##fmt);return(NIL)
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
typedef enum _tag _tag;
typedef enum TOKEN TOKEN;
typedef union data data;
typedef struct sexp sexp;
typedef struct cons cons;
typedef struct symref symref;
//typedef sexp YYSTYPE;
typedef const sexp(*sexp_binop)(sexp,sexp);
enum _tag {
  _nil = -1,
  _cons = 0,
  _double = 1,
  _long = 2,
  _char = 3,
  _str = 4,
  _fun = 5,
  _sym = 6,
  _special = 7,
  _macro = 8
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
enum TOKEN{
  TOK_EOF=-1,
  //literals|ID
  TOK_INT=1,
  TOK_REAL=2,
  TOK_CHAR=3,
  TOK_STRING=4,
  TOK_ID=5,
  //reserved words
  TOK_LAMBDA=6,
  TOK_DEF=7,//def or define
  TOK_SETQ=8,
  TOK_DEFUN=9,
  TOK_IF=10,
  TOK_DO=11,
  TOK_PROGN=12,
  TOK_LET=13,
  TOK_TAG=14,//tagbody
  TOK_GO=15,
  TOK_QUOTE=16,//literal quote or the word quote
  TOK_QUASI=17,
  TOK_EVAL=18,
  TOK_DEFMACRO=19,
  TOK_COMMA=20,
  TOK_COMMENT_START=21,
  TOK_COMMENT_END=22,
  //Types
  TOK_TYPEDEF=23,
  TOK_TYPEINFO=24,
  TOK_DATATYPE=25,
  TOK_STRUCT=26,
  TOK_UNION=27,
  TOK_ENUM=28,
  //delimiters
  TOK_LPAREN=50,
  TOK_RPAREN=51,
  TOK_LBRACE=52,
  TOK_RBRACE=53,
  TOK_LCBRACE=54,
  TOK_RCBRACE=55
};
#define addSym(Var)\
  HASH_ADD_KEYPTR(hh, symbolTable, Var->name, strlen(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
#define getSym(name,Var)\
  HASH_FIND_STR(symbolTable,name,Var)
static const sexp NIL={-1,0};
symref* symbolTable;
sexp* yylval;
static const char* tag_name(_tag obj_tag){
  switch(obj_tag){
    case _nil:
      return "nil";
    case _cons:
      return "cons";
    case _double:
      return "double";
    case _long:
      return "long";
    case _char:
      return "char";
    case _str:
      return "string";
    case _fun:
      return "function";
    case _sym:
      return "symbol";
    case _special:
      return "special form";
    case _macro:
      return "macro";
  }
}
static const char* princ(sexp obj){
  PRINT_MSG("Starting princ");
  fprintf(stderr,"princ for a %s object",tag_name(obj.tag));
  char* retval;
  switch (obj.tag){
    case _double: 
      asprintf(&retval,"%g",(double)obj.val.real64);
      fprintf(stderr,"real %g",obj.val.real64);
      break;
    case _long:
      asprintf(&retval,"%#0x",(long)obj.val.int64);
      fprintf(stderr,"long %#0x",obj.val.int64);
      break;
    case _fun:
    case _sym:     
      asprintf(&retval,"%s",obj.val.var->name);
      fputs(obj.val.var->name,stderr);
      break;
    case _char:
      asprintf(&retval,"%lc",obj.val.utf8_char);
      break;
    case _cons:
      PRINT_MSG("starting princ for cons");
      if(obj.val.cons == 0){snprintf(retval,1000,"(null)");}
      else{
        const char* car_str=princ(obj.val.cons->car);
        const char* cdr_str=princ(obj.val.cons->cdr);
        fputs(car_str,stderr);
        fputs(cdr_str,stderr);
        asprintf(&retval,"(%s . %s)",car_str,cdr_str);
      }
      PRINT_MSG("finished princ for cons");
      break;
  }
  PRINT_MSG("Finished princ");
  return retval;
}
sexp yyparse(FILE* input);
void codegen(const char* output,FILE* c_code,sexp ast);
sexp internal_eval(sexp expr);
#endif
