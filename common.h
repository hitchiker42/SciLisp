/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
/*Refactor special forms to use enum type*/
#ifndef __COMMON_H__
#define __COMMON_H__
#include <stdio.h>
#include <stdlib.h>
//#define GC_DEBUG
#include <gc.h>
#include <unistd.h>
#include <stdarg.h>
#include <setjmp.h>
#include <math.h>
#include <signal.h>
#include "types.h"
//#include "lex.yy.h"
#define HERE_ON
//#define VERBOSE_LEXING
#if defined (HERE_ON) && !(defined (HERE_OFF))
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define HERE_MSG(string) fprintf(stderr,"here at %s,line %d\n%s\n"\
                                 ,__FILE__,__LINE__,string)
#define PRINT_MSG(string) fputs(string,stderr);fputs("\n",stderr)
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);fputs("\n",stderr)
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
#define my_abort(str,fmt...) fprintf(stderr,str,##fmt);abort()
#define my_err(str,fmt...) fprintf(stderr,str,##fmt);return(NIL)
#define xmalloc GC_MALLOC
#define xrealloc GC_REALLOC
#define xfree GC_FREE
#define xmalloc_atomic GC_MALLOC_ATOMIC
#define double_sexp(double_val) (sexp){_double,(data)(double)double_val}
#define long_sexp(long_val) (sexp){_long,(data)(long)long_val}
#define cons_sexp(cons_val) (sexp){_cons,(data)(cons*)cons_val}
#define addSym(Var)\
  HASH_ADD_KEYPTR(hh, symbolTable, Var->name, strlen(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
#define getSym(name,Var)\
  HASH_FIND_STR(symbolTable,name,Var)
#define symVal(symref_sexp) symref_sexp.val.var->val.val
#define CORD_strdup(str) CORD_from_char_star(str)
static const sexp NIL={-1,0};
symref symbolTable;
sexp* yylval;
const char* restrict tag_name(_tag obj_tag);
const char* restrict princ(sexp obj);
CORD print(sexp obj);
sexp yyparse(FILE* input);
void codegen(const char* output,FILE* c_code,sexp ast);
sexp internal_eval(sexp expr);
static inline double getDoubleVal(sexp x){
  switch(x.tag){
    case _double:
      return x.val.real64;
    case _long:
      HERE();
      return (double)x.val.int64;
  }
}
static inline int isTrue(sexp x){
  if(x.tag == _nil){return 0;}
  else if(x.tag ==_double){
    if(ceil(x.val.real64)){return 1;}
    return 0;
  } else if(x.tag ==_long){
    if(x.val.int64){return 1;}
    return 0;
  }
  return 1;
}
#endif
