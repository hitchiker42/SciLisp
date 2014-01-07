#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/PassManager.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm-c/Core.h>
#define specifyDeclaredStruct(structType,types) structType::setBody(types),structType
#define ArefArgs(numargs)  (ArrayRef<Type*>(LispArgs,numargs))
#define mkFunType(numargs) (FunctionType::get(LispSexp,ArefArgs(numargs),false))
#define SexpToAref(type,obj) (ArrayRef<type>((type*)obj.val.array,obj.len))
#define voidFunction() (FunctionType::get(LispSexp,false))
#ifdef DEFUN
#undef DEFUN
#endif
typedef struct name_args_pair name_args_pair;
struct name_args_pair{
  char* name;
  int args;
};
//THIS IS BROKEN, THESE NEED TO BE THE C NAMES
//"DEFUN(\"[^\"]+\",\\([^,]+\\),[[:digit:]]+,\\([[:digit:]]+\\));" -> {\1,\2},

static name_args_pair lisp_prims[]={
  {"lisp_conspcall",2}, {"lisp_numberpcall",1}, {"lisp_arraypcall",1},
  {"lisp_nilpcall",1}, {"lisp_addcall",2}, {"lisp_subcall",2},
  {"lisp_mulcall",2}, {"lisp_divcall",2}, {"lisp_xorcall",2},
  {"lisp_logandcall",2}, {"lisp_logorcall",2}, {"carcall",1},
  {"cdrcall",1}, {"caarcall",1}, {"cadrcall",1}, {"cddrcall",1},
  {"cdarcall",1}, {"caaarcall",1}, {"caadrcall",1}, {"caddrcall",1},
  {"cdddrcall",1}, {"cddarcall",1}, {"cdaarcall",1}, {"cadarcall",1},
  {"cdadrcall",1}, {"Conscall",2}, {"mapcarcall",2},
  {"lisp_typeNamecall",1}, {"lisp_printcall",1}, {"reducecall",2},
  {"lisp_ltcall",2}, {"lisp_gtcall",2}, {"lisp_gtecall",2},
  {"lisp_ltecall",2}, {"lisp_necall",2}, {"lisp_equalscall",2},
  {"lisp_powcall",2}, {"lisp_sqrtcall",1}, {"lisp_coscall",1},
  {"lisp_sincall",1}, {"lisp_tancall",1}, {"lisp_expcall",1},
  {"lisp_logcall",1}, {"lisp_abscall",1}, {"ashcall",2},
  {"lisp_modcall",2}, {"lisp_randfloatcall",1}, {"lisp_randintcall",0},
  {"lisp_iotacall",4}, {"arefcall",2}, {"array_to_listcall",1},
  {"lisp_evalcall",1}, {"lisp_lengthcall",1}, {"lisp_roundcall",2}}
/*
{"+",2}, {"-",2}, {"*",2}, {"/",2},
 {"logxor",2}, {"logand",2}, {"logor",2}, {"car",1}, {"cdr",1},
{"caar",1}, {"cadr",1}, {"cddr",1}, {"cdar",1}, {"caaar",1},
{"caadr",1}, {"caddr",1}, {"cdddr",1}, {"cddar",1}, {"cdaar",1},
{"cadar",1}, {"cdadr",1}, {"cons",2}, {"mapcar",2}, {"typeName",1},
{"print",1}, {"reduce",2}, {"<",2}, {">",2}, {">=",2}, {"<=",2},
{"!=",2}, {"=",2}, {"expt",2}, {"sqrt",1}, {"cos",1}, {"sin",1},
{"tan",1}, {"exp",1}, {"log",1}, {"abs",1}, {"ash",2}, {"mod",2},
{"drand",1}, {"lrand",0}, {"iota",4}, {"aref",2}, {"array->list",1},
{"eval",1}, {"length",1}, {"round",2}};*/

extern "C" {
  //#include "common.h"
  //#include "prim.h"
  //#include "cons.h"
  #include "include/cord.h"
  #include "include/uthash.h"
  //because C++ can't deal with real code we need to cut and paste
  //this from C to remove the difficult stuff
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
typedef struct function function;//struct of min/max args and union of lambda/fxn_proto
typedef struct scoped_sexp scoped_sexp;//an sexp and it's containing environment
typedef const sexp(*sexp_binop)(sexp,sexp);//not used
typedef const char*  c_string;//type of \0 terminated c strings
typedef symbol* symref;//type of generic symbol referances
typedef global_symbol* global_symref;//"" global ""
typedef local_symbol* local_symref;//"" local ""
typedef fxn_proto* fxn_ptr;//pointer to primitive function
//c macros to test for a specific type
#define NILP(obj) (obj.tag == _nil)
#define CONSP(obj) (obj.tag == _cons || obj.tag == _list)
#define NUMBERP(obj) (obj.tag == _double || obj.tag == _long)
#define FLOATP(obj) (obj.tag == _double)
#define AS_DOUBLE(obj) (obj.val.real64)
#define INTP(obj) (obj.tag == _long)
#define AS_LONG(obj) (obj.val.int64)
#define SYMBOLP(obj) (obj.tag == _sym)
#define AS_SYMBOL(obj) (obj.val.var)
#define SPECP(obj) (obj.tag== _special)
#define STRINGP(obj) (obj.tag == _str)
#define AS_STRING(obj) (obj.val.cord)
#define CHARP(obj) (obj.tag == _char)
#define AS_CHAR(obj) (obj.val.utf8_char)
#define FUNP(obj) (obj.tag == _fun)
#define ARRAYP(obj) (obj.tag == _array)
#define AS_ARRAY(obj) (obj.val.array)
#define LAMBDAP(obj) (obj.tag == _lam)
#define FUNCTIONP(obj)(obj.tag == _lam || obj.tag == _fun)
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
};
//sign bit determines quoting
#define quote_mask  0x7fff
#define stripQuote(meta) quote_mask & meta
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
  wchar_t utf8_char;
  wchar_t utf8_str;
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
  _tag tag;//could be shorter if need be
  unsigned short len;//length of a list, array or string
  sexp_meta meta : 16;//random meta data(sign bit determines quoting)
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
struct function{
  short min_args;
  short max_args;
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
  CORD cname;//non-prim sets cname to 0...yeah no
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
struct scoped_sexp{
  sexp sexp;
  env* env;
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
}
