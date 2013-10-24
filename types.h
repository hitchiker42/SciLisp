/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#define uthash_malloc(sz) GC_MALLOC(sz)
#define uthash_free(ptr,sz) GC_FREE(ptr)
#define HASH_USING_NO_STRICT_ALIASING
#define HASH_FUNCTION HASH_MUR
#include <cord.h>
#include <string.h>
#include "include/uthash.h"
#include <wchar.h>
#include <getopt.h>
#include <limits.h>
#include "regex.h"
typedef enum _tag _tag;//different types of a lisp object
typedef enum TOKEN TOKEN;//type of values returned from yylex
typedef enum special_form special_form;//different types of special forms
typedef enum sexp_meta sexp_meta;
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
typedef struct array_symbol array_symbol;//type of symbol used in function arguments, if I use it
typedef struct env env;//generic symbol namespace
typedef struct local_env local_env;//linked list representing a local namespace
typedef struct global_env global_env;//hash table representing global namespace
typedef struct array_env array_env;//array used to hold function arguments, maybe eventually
typedef struct lambda lambda;//type of lambda expressions
typedef struct function function;//struct of min/max args and union of lambda/fxn_proto
typedef struct scoped_sexp scoped_sexp;//an sexp and it's containing environment
typedef struct function_args function_args;
typedef const sexp(*sexp_binop)(sexp,sexp);//not used
typedef const char* restrict c_string;//type of \0 terminated c strings
typedef symbol* symref;//type of generic symbol referances
typedef global_symbol* global_symref;//"" global ""
typedef local_symbol* local_symref;//"" local ""
typedef array_symbol *array_symref;//"" array ""
typedef fxn_proto* fxn_ptr;//pointer to primitive function
//c macros to test for a specific type
#define NILP(obj) (obj.tag == _nil)
#define CONSP(obj) (obj.tag == _cons || obj.tag == _list || obj.tag == _dpair)
#define NUMBERP(obj) (obj.tag == _double || obj.tag == _long)
#define FLOATP(obj) (obj.tag == _double)
#define AS_DOUBLE(obj) (obj.val.real64)
#define INTP(obj) (obj.tag == _long)
#define AS_LONG(obj) (obj.val.int64)
#define SYMBOLP(obj) (obj.tag == _sym)
#define AS_SYMBOL(obj) (obj.val.var)
#define SPECP(obj) (obj.tag== _special)
#define STRINGP(obj) (obj.tag == _str || obj.tag == _ustr)
#define AS_STRING(obj) (obj.val.cord)
#define CHARP(obj) (obj.tag == _char)
#define AS_CHAR(obj) (obj.val.utf8_char)
#define FUNP(obj) (obj.tag == _fun)
#define ARRAYP(obj) (obj.tag == _array)
#define AS_ARRAY(obj) (obj.val.array)
#define LAMBDAP(obj) (obj.tag == _lam)
#define FUNCTIONP(obj)(obj.tag == _lam || obj.tag == _fun)
#define STREAMP(obj)(obj.tag ==_stream)
#define REGEXP(obj)(obj.tag == _regex)
enum _tag {
  _error = -4,//type of errors, value is a string
  _false = -3,//type of #f, actual value is undefined
  _uninterned = -2,//type of uninterned symbols, value is symbol(var)
  _nil = -1,//type of nil, singular object,vaule is undefined
  _cons = 0,//type of cons cells(aka lisp programs), value is cons
  _double = 1,//type of floating point numbers, value is real64
  _long = 2,//type of integers, vaule is int64
  _char = 3,//type of chars(c type wchar_t),value is utf8_char
  _str = 4,//type of strings, value is cord
  _fun = 5,//type of builtin functions,value is function pointer,fun
  _sym = 6,//type of symbols,value is var
  _special = 7,//type of special form,value is meta(a _tag value)
  _macro = 8,//type of macros, unimplemented
  _type = 9,//type of types, unimplemened
  _array = 10,//type of arrays, element type in meta, vaule is array
  _true = 11,//type of #t, singular value
  _list = 12,//type of lists,value is cons
  _lam = 13,//type of lambda, value is lam
  _lenv = 14,//type of local environments,value is lenv
  _dpair = 15,
  _ustr = 16,//utf8 string
  _regex = 17,//compiled regular expression
  _stream = 18,//type of input/output streams, corrsponds to c FILE*
  _matrix =19,//array for mathematical calculations
  _keyword = 20,
  _funarg = 21,
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
  _prog1=23,
  _dolist=24,
};
//sign bit determines quoting
#define quote_mask  0x7fff
#define strippQuote(meta) quote_mask & meta
#define isQuoted(meta)  0x8000 & meta
#define MakeQuoted(meta)  _quoted_value | meta
enum sexp_meta{
  _quoted_utf8_string=(short)0x8003,
  _quoted_long_array=(short)0x8002,
  _quoted_double_array=(short)0x8001,
  _quoted_value = (short)0x8,
  _double_array=1,
  _long_array=2,
  _utf8_string=3,
};
union data {
  double real64;
  long int64;
  wchar_t utf8_char;//depreciated
  wchar_t uchar;//try to change all utf8_chars to this
  wchar_t *ustr;
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
  regex_t* regex;
  //before I use keyword symbols I need to figure out an efficent 
  //way to compare them
  FILE* stream;
  function_args* funarg;
};
//I'm thinking of using a bitfield for quoting...I'm not sure though
//
struct sexp{
  _tag tag;//could be shorter if need be  
  sexp_meta meta : 16;//random meta data(sign bit determines quoting)
  unsigned short len;//length of a list, array or string
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
  TOK_LISP_TRUE=6,
  TOK_LISP_FALSE=7,
  //reserved words/characters 18-30  
  TOK_QUOTE=18,
  TOK_QUASI=19,
  TOK_SPECIAL=20,
  TOK_COMMENT_START=21,
  TOK_COMMENT_END=22,
  TOK_DOT=23,
  TOK_COLON=24,
  TOK_LAMBDA=25,
  TOK_AROBASE=26,
  TOK_COMMA=27,
  //Types 40-50
  TOK_TYPEDEF=40,
  TOK_TYPEINFO=41,
  //delimiters 50 - 60
  TOK_LPAREN=50,
  TOK_RPAREN=51,
  TOK_LBRACE=52,
  TOK_RBRACE=53,
  TOK_LCBRACE=54,
  TOK_RCBRACE=55,
};
union funcall{
  sexp(*f0)(void);
  sexp(*f1)(sexp);
  sexp(*f2)(sexp,sexp);
  sexp(*f3)(sexp,sexp,sexp);
  sexp(*f4)(sexp,sexp,sexp,sexp);
  sexp(*fmany)(sexp,...);
};
struct function{
  short min_args;
  short max_args;
  short opt_args;
  short rest_parameter;
  union {
    fxn_proto* prim;
    lambda* lam;
  } fun;
  enum {
    _primFun=0,
    _lambdaFun=1,
  } fxn_type;
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
struct array_symref{
  CORD name;
  sexp val;
  int index;
};
struct local_env{
  env* enclosing;
  local_symref head;
};
struct global_env{
  env* enclosing;
  global_symref head;
};
//this is a better way of doing this
//but needs work
#define LAST_OPTARG(funarg) funarg->opt_args[funarg->num_opt_args]
#define LAST_REQARG(funarg) funarg->req_args[funarg->num_req_args]
struct function_args{
  short num_req_args;
  short num_opt_args;
  short num_keyword_args;//not implemented yet
  short has_rest_arg;
  symref* req_args;//req arg is just a symref
  symref* opt_args;//opt arg is a symref with a possible default value
  sexp* keyword_args;//unimplemened, but here for the future
  symref* rest_arg;//a rest arg is a list of 0 or more arguments
  symref* args[4];
  int max_args;//number of args in c/llvm must be max_args
};
struct function_new{
  struct function_args* args;
  CORD lname;//lambdas should be #<lambda{number via global counter}>
  union {
    lambda* lambda_fun;
    fxn_ptr compiled_fun;
  } function_ptr;
  enum {
    _lambda_fun,
    _compiled_fun,
  } fun_type;
};
struct array_env{
  env* enclosing;
  array_symref head;
};
union symbol_ref{
  global_symref global;
  local_symref local;
  //  array_symref array;
};
union symbol_val{
  local_symbol local;
  global_symbol global;
  //  array_symbol array;
};
struct env{
  env* enclosing;
  symbol_ref head;  
  enum {
    _local=0,
    _global=1,
  } tag;
};
struct lambda{
  local_env *env;
  short minargs,maxargs;
  sexp body;
};
struct scoped_sexp{
  sexp sexp;
  env* env;
};
//aviable command line options, struct args: 1st arg option name
//2nd arg=enum{no_argument=0,required_argument=1,optional_argument=2}
//3rd arg can be set to a variable adress to allow that argument to set it
//for any long argument with a corrsponding short option this must be 0
//4th arg is the value to load into 3rd arg if it is not null otherwise
//it should be set to the equivlant short option
static struct option long_options[];
static struct option long_options[] = {
  {"backend",1,0,'b'},
  {"eval"   ,1,0,'e'},
  {"help"   ,0,0,'h'},
  {"load"   ,1,0,'l'},
  {"output" ,1,0,'o'},
  {"quiet"  ,0,0,'q'},
  {"test"   ,0,0,'t'},
  {"version",0,0,'v'},
  {0,0,0,0}
};
global_env globalSymbolTable;
env topLevelEnv;
//basically env.h(move to seperate file?)
local_symref getLocalSym(local_env *cur_env,CORD name);
global_symref getGlobalSym(CORD name);
symref getSym(env *cur_env,CORD name);
symref addSym(env *cur_env,symref Var);
symref addGlobalSym(symref Var);
symref addLocalSym(local_env *cur_env,symref Var);
//type punning macros
#define toSymbol(sym) (*(symbol*)&sym)
#define toSymref(ref) (*(symref*)&(ref))
static inline size_t symbolSize(env *cur_env){
  switch(cur_env->tag){
    case _global:
      return sizeof(global_symbol);
    case _local:
      return sizeof(local_symbol);
 }
}
#define getGlobalSymMacro(name,Var)                                  \
  HASH_FIND_STR(globalSymbolTable.head,(const char *)name,Var)
#define addGlobalSymMacro(Var)                                          \
  HASH_ADD_KEYPTR(hh, globalSymbolTable.head, Var->name, strlen(Var->name), Var)
  //         hh_name, head,        key_ptr,   key_len,           item_ptr
//literal objects for each type
#define mkTypeSym(name,mval)                                    \
  static const sexp name = {.tag=_type, .val={.meta = mval}}
mkTypeSym(Qerror,-4);
mkTypeSym(Qfalse,-3);
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
mkTypeSym(Qlist,12);
mkTypeSym(Qlam,13);
mkTypeSym(Qlenv,14);
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
static function prim_to_fun(fxn_proto *prim){
  return (function){.min_args=prim->min_args,
      .max_args=prim->max_args,.fun={.prim = prim},0};
}
#define mkTypeCase(type,tag) case tag: return type  
static sexp typeOf(sexp obj){
  switch (obj.tag){
    mkTypeCase(Qerror,-4);
    mkTypeCase(Qfalse,-3);
    mkTypeCase(Quninterned,-2);
    mkTypeCase(Qnil,-1);
    mkTypeCase(Qcons,0);
    mkTypeCase(Qdouble,1);
    mkTypeCase(Qlong,2);
    mkTypeCase(Qchar,3);
    mkTypeCase(Qstr,4);
    mkTypeCase(Qfun,5);
    mkTypeCase(Qsym,6);
    mkTypeCase(Qspec,7);
    mkTypeCase(Qmacro,8);
    mkTypeCase(Qtype,9);
    mkTypeCase(Qarr,10);
    mkTypeCase(Qtrue,11);
    mkTypeCase(Qlist,12);
    mkTypeCase(Qlam,13);
    mkTypeCase(Qlenv,14);
  }
}

enum operator{
  _add,
  _sub,
  _mul,
  _div,
  _pow,
  _max,
  _min,
  _logand,
  _logior,
  _logxor,
};
#define getVal(obj)                                             \
  (obj.tag == _error ? obj.val.cord  :                          \
   (obj.tag == _false ? -3  :                                   \
    (obj.tag == _uninterned ? -2  :                             \
     (obj.tag == _nil ? 0  :                                    \
      (obj.tag == _cons ? obj.val.cons  :                       \
       (obj.tag == _double ? obj.val.real64  :                  \
        (obj.tag == _long ? obj.val.int64  :                    \
         (obj.tag == _char ? obj.val.uchar  :                   \
          (obj.tag == _str ? obj.val.cord  :                    \
           (obj.tag == _fun ? obj.val.fun  :                    \
            (obj.tag == _sym ? obj.val.var  :                   \
             (obj.tag == _special ? obj.val.meta  :             \
              (obj.tag == _macro ? 0.  :                        \
               (obj.tag == _type ? obj.val.meta  :              \
                (obj.tag == _array ? obj.val.array  :           \
                 (obj.tag == _true ? 11  :                      \
                  (obj.tag == _list ? obj.val.cons  :           \
                   (obj.tag == _lam ? obj.val.lam  :            \
                    (obj.tag == _lenv ? obj.val.lenv  :         \
                     (obj.tag == _dpair ? obj.val.cons  :       \
                      (obj.tag == _ustr ? obj.val.ustr  :       \
                       (obj.tag == _regex ? obj.val.regex  :    \
                        (obj.tag == _stream ? obj.val.stream  : \
                         )))))))))))))))))))))))
/*struct typehash{
  _tag type;
  char* name;
  long hash;
};
{.tag =_double,.name = "double",.hash = 0x276d1506}
{.tag =_long,.name = "long",.hash = 0xc6417873}
{.tag =_nil,.name = "nil",.hash = 0x6b8a33b2}
{.tag =_cons,.name = "cons",.hash = 0xf114bd8e}
{.tag =_string,.name = "string",.hash = 0xa17d48e}
{.tag =_char,.name = "char",.hash = 0xf118d13d}
{.tag =_symbol,.name = "symbol",.hash = 0x83615d2d}
{.tag =_special,.name = "special",.hash = 0x59d387a}
{.tag =_type,.name = "type",.hash = 0x9fa9a73d}
{.tag =_array,.name = "array",.hash = 0x11b0a054}
{.tag =_boolean,.name = "boolean",.hash = 0x624dd20f}
{.tag =_lambda,.name = "lambda",.hash = 0x65d0c568}*/
