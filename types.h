/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "include/cord.h"
#include <string.h>
#include <uthash.h>
#include <wchar.h>
#include <getopt.h>
typedef enum _tag _tag;
typedef enum TOKEN TOKEN;
typedef enum special_form special_form;
typedef union data data;
typedef union env_type env_type;
typedef union symbol_type symbol_type;
typedef union funcall funcall;
typedef struct sexp sexp;
typedef struct cons cons;
typedef struct symbol symbol;
typedef struct fxn_proto fxn_proto;
typedef struct env env;
typedef struct local_symbol local_symbol;
typedef struct local_env local_env;
typedef struct hash_env hash_env;
typedef struct array array;
typedef const sexp(*sexp_binop)(sexp,sexp);
typedef const char* restrict c_string;
typedef symbol* symref;
typedef fxn_proto* fxn_ptr;
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
#define NUM_TYPES 16
enum _tag {
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
  void* raw_fun;
  special_form special;
  array* arr;
  _tag meta;
  sexp* quoted;
};
struct sexp{
  _tag tag;
  data val;
};
struct symbol{
  const char* restrict name;
  sexp val;
  UT_hash_handle hh;
};
struct cons{
  sexp car;
  sexp cdr;
};
struct array{
  _tag tag;
  data *vals;
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
  TOK_SPECIAL=20,
  TOK_QUOTE=19,
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
union funcall{
  sexp(*f0)(void);
  sexp(*f1)(sexp);
  sexp(*f2)(sexp,sexp);
  sexp(*f3)(sexp,sexp,sexp);
  sexp(*f4)(sexp,sexp,sexp,sexp);
  sexp(*fmany)(sexp,...);
};
struct fxn_proto{
  CORD cname;
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
struct local_symbol{
  CORD name;
  sexp val;
  local_symbol* next;
};
struct local_env{
  env* enclosing;
  local_symbol* head;
};
struct hash_env{
  env* enclosing;
  symref head;
};
union env_type{
  local_env local;
  hash_env hash;
};
union symbol_type{
  symref global;
  local_symbol* local;
};
struct env{
  enum {
    _local=0,
    _hash=1
  } tag;
  env_type env;
};
static struct option long_options[] = {
  {"output",required_argument,0,'o'},
  {0       ,0                ,0,0  }
};
hash_env globalSymbolTable;
sexp getSymlocal(local_env cur_env,CORD name);
sexp lookupSym(env* cur_env,CORD name);
#define getSym(name,Var)                                \
  HASH_FIND_STR(globalSymbolTable.head,(const char *)name,Var)
#define addSym(Var)                                                     \
  HASH_ADD_KEYPTR(hh, globalSymbolTable.head, Var->name, strlen(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
#define mkTypeSym(name,val)                     \
  static const sexp name = {-2, {.meta = val}}
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
#define isTrue(x)                                            \
  (x.tag == _nil ? 0 :                                       \
    (x.tag = _long ? (x.val.int64 == 0 ? 0 : 1) :         \
     (x.tag = _double ? (x.val.real64 == 0.0 ? 0 : 1) : 1)))
    
enum backend{
  c=0,
  llvm=1,
  as=2,
};

