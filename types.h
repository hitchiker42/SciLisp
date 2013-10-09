/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "include/cord.h"
#include <string.h>
#include "include/uthash.h"
#include <wchar.h>
#include <getopt.h>
typedef enum _tag _tag;//different types of a lisp object
typedef enum TOKEN TOKEN;//type of values returned from yylex
typedef enum special_form special_form;//different types of special forms
typedef union data data;//core representation of a lisp object
typedef union env_type env_type;//generic environment
typedef union symbol_type symbol_type;//generic symbol
typedef union funcall funcall;//type of primitive functions
typedef union symbol_ref symbol_ref;//generic symbol pointer
typedef union symbol_val symbol_val;//generic symbol
typedef struct sexp sexp;//type of all lisp objects
typedef struct cons cons;//cons cell, for lists,pairs and everything else
typedef struct fxn_proto fxn_proto;//primitive function prototype
typedef struct symbol symbol;//generic symbol type
typedef struct local_symbol local_symbol;//type of symbol used in local envs
typedef struct global_symbol global_symbol;//type of symbol used in global envs
typedef struct env env;//generic symbol namespace
typedef struct local_env local_env;//linked list representing a local namespace
typedef struct global_env global_env;//hash table representing global namespace
typedef struct lambda lambda;//type of lambda expressions
typedef const sexp(*sexp_binop)(sexp,sexp);//not used
typedef const char* restrict c_string;//type of \0 terminated c strings
typedef symbol* symref;//type of generic symbol referances
typedef global_symbol* global_symref;//"" global ""
typedef local_symbol* local_symref;//"" local ""
typedef fxn_proto* fxn_ptr;//pointer to primitive function
//c macros to test for a specific type
#define NILP(obj) (obj.tag == _nil)
#define CONSP(obj) (obj.tag == _cons || obj.tag == _list)
#define NUMBERP(obj) (obj.tag == _double || obj.tag == _long)
#define FLOATP(obj) (obj.tag == _double)
#define INTP(obj) (obj.tag == _long)
#define SYMBOLP(obj) (obj.tag == _sym)
#define SPECP(obj) (obj.tag== _special)
#define STRINGP(obj) (obj.tag == _str)
#define CHARP(obj) (obj.tag == _char)
#define FUNP(obj) (obj.tag == _fun)
#define LAMBDAP(obj) (obj.tag == _lam)
#define NUM_TYPES 16
enum _tag {
  _false = -3,
  _uninterned = -2,
  _nil = -1,
  _cons = 0,
  _double = 1,
  _long = 2,
  _char = 3,
  _str = 4,
  _fun = 5,
  _sym = 6,
  _special = 7,
  _macro = 8,
  _type = 9,
  _array = 10,
  _true = 11,
  _list = 12,
  _quoted = 13,
  _lam = 14,
  _lenv = 15,
};
enum special_form{
  _def=0,
  _defun=1,
  _setq=2,
  _if=3,
  _let=4,
  _do=5,
  _lambda=6,
  _progn=7,
  _go=8,
  _tagbody=9,
  _struct=10,
  _union=11,
  _datatype=12,
  _enum=13,
  _eval=14,
  _defmacro=15,
  _quasi=16,
  _quote=17,
  _comma=18,
  _and=19,
  _or=20,
  _main=21,
  _while=22,
};
union data {
  double real64;
  long int64;
  wchar_t utf8_char;
  c_string string;
  CORD cord;
  cons* cons;
  symref var;
  fxn_ptr fun;
  lambda* lam;
  special_form special;
  data* array;
  _tag meta;
  sexp* quoted;
  local_symref lenv;
};
struct sexp{
  _tag tag;
  short len;//length of a list, array or string
  short meta;//random meta data, needs to be an enum at some point
  data val;
};
struct cons{
  sexp car;
  sexp cdr;
};
enum TOKEN{
  TOK_EOF=-1,
  //literals|ID 0-20
  TOK_INT=1,
  TOK_REAL=2,
  TOK_CHAR=3,
  TOK_STRING=4,
  TOK_ID=5,  
  //reserved words/characters 20-30
  TOK_SPECIAL=20,
  TOK_QUOTE=19,
  TOK_COMMENT_START=21,
  TOK_COMMENT_END=22,
  TOK_DOT=23,
  TOK_COLON=24,
  TOK_LAMBDA=25,
  //Types 40-50
  TOK_TYPEDEF=40,
  TOK_TYPEINFO=41,
  //delimiters 50 - 60
  TOK_LPAREN=50,
  TOK_RPAREN=51,
  TOK_LBRACE=52,
  TOK_RBRACE=53,
  TOK_LCBRACE=54,
  TOK_RCBRACE=55
};
union funcall{
  sexp(*f0)(void);
  sexp(*f1)(sexp);
  sexp(*f2)(sexp,sexp);
  sexp(*f3)(sexp,sexp,sexp);
  sexp(*f4)(sexp,sexp,sexp,sexp);
  sexp(*fmany)(sexp,...);
};
struct fxn_proto{
  CORD cname;//non-prim sets cname to 0
  CORD lispname;
  //max_args == -1 means remaining args as a list
  short min_args,max_args;
  funcall fxn_call;
};
#define FMAX_ARGS(fxn) fxn.val.fun->max_args
#define FMIN_ARGS(fxn) fxn.val.fun->min_args
#define FCNAME(fxn) fxn.val.fun->cname
#define FLNAME(fxn) fxn.val.fun->lispname
#define F_CALL(fxn) fxn.val.fun->fxn_call
struct symbol{
  CORD name;
  sexp val;
};
struct global_symbol{
  CORD name;
  sexp val;
  UT_hash_handle hh;
};
struct local_symbol{
  CORD name;
  sexp val;
  local_symref next;
};
struct local_env{
  env* enclosing;
  local_symref head;
};
struct global_env{
  env* enclosing;
  global_symref head;
};
union symbol_ref{
  global_symref global;
  local_symref local;
};
union symbol_val{
  local_symbol local;
  global_symbol global;
};
struct env{
  env* enclosing;
  symbol_ref head;
  enum {
    _local=0,
    _global=1
  } tag;
};
struct lambda{
  local_env env;
  short minargs,maxargs;
  sexp body;
};
//aviable command line options
static struct option long_options[] = {
  {"output",required_argument,0,'o'},
  {0       ,0                ,0,0  }
};
global_env globalSymbolTable;
env topLevelEnv;
//basically env.h(move to seperate file?)
local_symref getLocalSym(local_env cur_env,CORD name);
global_symref getGlobalSym(CORD name);
symref getSym(env cur_env,CORD name);
symref addSym(env cur_env,symref Var);
symref addGlobalSym(symref Var);
symref addLocalSym(local_env cur_env,symref Var);
//type punning macros
#define toSymbol(sym) (*(symbol*)&sym)
#define toSymref(ref) (*(symref*)&(ref))
static inline size_t symbolSize(env cur_env){
  switch(cur_env.tag){
    case _global:
      return sizeof(global_symbol);
    case _local:
      return sizeof(local_symbol);
  }
}
#define getGlobalSymMacro(name,Var)                                  \
  HASH_FIND_STR(globalSymbolTable.head,(const char *)name,Var)
#define addGlobalSymMacro(Var)                                               \
  HASH_ADD_KEYPTR(hh, globalSymbolTable.head, Var->name, strlen(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
//literal objects for each type
#define mkTypeSym(name,mval)                                    \
  static const sexp name = {.tag=-2, .val={.meta = mval}}
mkTypeSym(Quninterned,-2);
mkTypeSym(Qnil,-1);
mkTypeSym(Qcons,0);
mkTypeSym(Qdouble,1);
mkTypeSym(Qlong,2);
mkTypeSym(Qchar,3);
mkTypeSym(Qstr,4);
mkTypeSym(Qfun,5);
mkTypeSym(Qsym,6);
mkTypeSym(Qspec,7);
mkTypeSym(Qmacro,8);
mkTypeSym(Qtype,9);
mkTypeSym(Qarr,10);
mkTypeSym(Qtrue,11);
//static const sexp typeArray[NUM_TYPES-1] = {Quninterned,Qnil,Qcons,Qdouble,Qlong,Qchar,Qstr,Qfun,Qsym,Qspec,Qmacro,Qtype,Qarr,Qtrue};
//static sexp typeOf(sexp obj){
//  return typeArray[obj.tag+2];
//}
//defines what values are considered false
#define isTrue(x)                                            \
  (x.tag == _false ? 0 :                                     \
    (x.tag == _nil ? 0 :                                     \
     (x.tag == _long ? (x.val.int64 == 0 ? 0 : 1) :             \
      (x.tag == _double ? (x.val.real64 == 0.0 ? 0 : 1) : 1))))
//possible compilier backends
enum backend{
  c=0,
  llvm=1,
  as=2,
};
