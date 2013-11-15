/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//Don't include this file directly, it's included by common.h

//including cord.h is weird, it includes "cord_pos.h" which isn't
//automatically installed for some reason so we specify the actual
//file we're including explicitly, as thats the easiest way to fix things
#include "gc/include/gc/cord.h"
#include <string.h>
#include <setjmp.h>
#include <wchar.h>
#include <stdint.h>
#include <getopt.h>
#include <limits.h>
#include "regex.h"
#include <gmp.h>
#include <mpfr.h>
#include <mpf2mpfr.h>
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
//typedef struct fxn_proto fxn_proto;//primitive function prototype
typedef struct symbol symbol;//generic symbol type
typedef struct local_symbol local_symbol;//type of symbol used in local envs
typedef struct global_symbol global_symbol;//type of symbol used in global envs
typedef struct env env;//generic symbol namespace
typedef struct local_env local_env;//linked list representing a local namespace
typedef struct function_env function_env;//Actually used for function arguments
typedef struct lambda lambda;//type of lambda expressions
typedef struct lambda_new lambda_new;//type of lambda expressions
typedef struct function function;//struct of min/max args and union of lambda/fxn_proto
typedef struct function_new function_new;//struct of min/max args and union of lambda/fxn_proto
typedef struct scoped_sexp scoped_sexp;//an sexp and it's containing environment
typedef struct typed_symbol typed_symbol;
typedef struct function_args function_args;
typedef struct obarray obarray;
typedef struct obarray_entry obarray_entry;
typedef struct obarray_env obarray_env;
typedef struct macro macro;
typedef const sexp(*sexp_binop)(sexp,sexp);//not used
typedef const char* restrict c_string;//type of \0 terminated c strings
typedef symbol *symref;//type of generic symbol referances
typedef local_symbol *local_symref;//"" local ""
typedef symbol *keyword_symref;
typedef symbol keyword_symbol;
typedef typed_symbol *typed_symref;
//typedef fxn_proto* fxn_ptr;//pointer to primitive function
//c macros to test for a specific type
#define NILP(obj) (obj.tag == _nil)
#define CONSP(obj) (obj.tag == _cons || obj.tag == _list || obj.tag == _dpair)
#define LISTP(obj) (obj.tag == _list)
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
#define FUNP(obj) (obj.tag == _fun && obj.val.fun->type == _compiled_fun)
#define ARRAYP(obj) (obj.tag == _array)
#define AS_ARRAY(obj) (obj.val.array)
#define LAMBDAP(obj) (obj.tag == _fun && obj.val.fun->type == _lambda_fun)
#define FUNCTIONP(obj)(obj.tag == _fun)
#define STREAMP(obj)(obj.tag ==_stream)
#define REGEXP(obj)(obj.tag == _regex)
#define ERRORP(obj)(obj.tag == _error)
#define BIGINTP(obj)(obj.tag == _bigint)
#define BIGFLOATP(obj) (obj.tag == _bigfloat)
#define BIGNUMP(obj) (obj.tag == _double || obj.tag == _long || \
                     obj.tag == _bigint || obj.tag == _bigfloat)
#define ENVP(obj)(obj.tag == _env)
#define MACROP(obj) (obj.tag == _macro)
#define INT8P(obj) (obj.tag == _byte)
#define INT16P(obj) (obj.tag == _short)
#define INT32P(obj) (obj.tag == _int)
#define INT64P(obj) (obj.tag == _long)
#define REAL32P(obj) (obj.tag == _float)
//key point to this enum is that arathmatic types are numbered in their type
//heriarchy, ints by size < floats by size < bigint < bigfloat, if you add any
//new types make sure it fits in the heirachy correcty
enum _tag {
  _unbound=-0xf,
  _error = -4,//type of errors, value is a string
  _false = -3,//type of #f, actual value is undefined
  _uninterned = -2,//type of uninterned symbols, value is symbol(var)
  _nil = -1,//type of nil, singular object,vaule is undefined
  _cons = 0,//type of cons cells(aka lisp programs), value is cons
  //arithmatic types, room for 7 more types currently
  _byte = 1,
  _ubyte = 2,
  _short = 3,
  _ushort = 4,
  _int = 5,
  _uint = 6,
  _long = 7,//type of integers, vaule is int64
  _ulong = 8,
  _float = 9,
  _double = 10,//type of floating point numbers, value is real64
  _bigint = 11,
  _bigfloat = 12,
  _char = 19,//type of chars(c type wchar_t),value is utf8_char
  _str = 20,//type of strings, value is cord
  _array = 21,//type of arrays, element type in meta, vaule is array
  _ustr = 22,//utf8 string
  _regex = 23,//compiled regular expression
  _stream = 24,//type of input/output streams, corrsponds to c FILE*
  _matrix =25,//array for mathematical calculations
  _list = 26,//type of lists,value is cons
  _dpair = 27,
  _fun = 31,//type of builtin functions,value is function pointer,fun
  _sym = 32,//type of symbols,value is var
  _special = 33,//type of special form,value is meta(a _tag value)
  _macro = 34,//type of macros, unimplemented
  _type = 35,//type of types, unimplemened
  _lam = 36,//type of lambda, value is lam
  _lenv = 37,//type of local environments,value is lenv
  _env = 38,
  _keyword = 39,
  _funarg = 40,//really need to add an s
  _funargs=40,//but yay for c hacks, I bet yout couldn't do this in most languages
  _true = 41,//type of #t, singular value
  _obarray = 42,
  _label = 43,//a c jmp_buf, in lisp a target for return or go
  _cptr=44,//opaque c pointer
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
  _dotimes=25,
  _return=26,
  _unwind_protect=27,
  _block=28,
  
};
enum sexp_meta{
  _double_array=_double,
  _long_array=_long,
  _utf8_string=_char,
  _splice_list=_list,
};
union data {//keep max size at 64 bits
  float real32;
  double real64;
  int8_t int8;
  uint8_t uint8;
  int16_t int16;
  uint16_t uint16;
  int32_t int32;
  uint32_t uint32;
  int64_t int64;
  uint64_t uint64;
  wchar_t utf8_char;//depreciated
  wchar_t uchar;//try to change all utf8_chars to this
  wchar_t *ustr;
  c_string string;
  CORD cord;
  cons *cons;
  symref var;
  keyword_symref keyword;
  function *fun;
  lambda *lam;
  special_form special;
  data *array;
  _tag meta;
  sexp *quoted;
  local_symref lenv;
  env *cur_env;
  regex_t *regex;
  FILE *stream;
  function_args* funarg;//depreciated
  function_args* funargs;
  //  function_new* fnew;
  mpz_t *bigint;
  mpfr_t *bigfloat;
  obarray* ob;
  macro *mac;
  jmp_buf *label;
  void *cptr;
};
//meta is for mutualy exclusvie information
//whlie the next 8 bits are for inclusive information
struct sexp{//128 bits/16 bytes
  _tag tag;//could be shorter if need be  
  sexp_meta meta : 8;//random metadata
  unsigned int quoted :2;
  int setfable :1;
  int has_comma :1;
  uint16_t len;//length of a list, array or string
  data val;
};
struct cons{//32 bytes
  sexp car;
  sexp cdr;
};
enum TOKEN{
  TOK_UNKN=-2,
  TOK_EOF=-1,
  //literals|ID 0-20
  TOK_INT=1,
  TOK_REAL=2,
  TOK_CHAR=3,
  TOK_STRING=4,
  TOK_ID=5,
  TOK_LISP_TRUE=6,
  TOK_LISP_FALSE=7,
  TOK_KEYSYM=8,
  //reserved words/characters 18-30  
  TOK_QUOTE=18,
  TOK_QUASI=19,//`
  TOK_SPECIAL=20,
  TOK_COMMENT_START=21,//#|
  TOK_COMMENT_END=22,//|#
  TOK_DOT=23,
  TOK_COLON=24,
  TOK_LAMBDA=25,//lambda or defun
  TOK_AROBASE=26,//@
  TOK_COMMA=27,
  TOK_LIST_SPLICE=28,//,@
  TOK_MACRO=29,
  TOK_RETURN=30,
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
  sexp(*f5)(sexp,sexp,sexp,sexp,sexp);
  sexp(*f6)(sexp,sexp,sexp,sexp,sexp,sexp);
  sexp(*f7)(sexp,sexp,sexp,sexp,sexp,sexp,sexp);
  sexp(*fmany)(sexp,...);//really there's no need for this
};
#define FCNAME(fxn) fxn.val.fun->cname
#define FLNAME(fxn) fxn.val.fun->lname
//this is a better way of doing this
//but needs work
#define LAST_ARG(funarg) funarg->args[funarg->max_args]
#define ADD_REQ_ARG(funarg) funarg->num_req_args++;funarg->max_args++
#define ADD_OPT_ARG(funarg) funarg->num_opt_args++;funarg->max_args++
#define ADD_KEYWORD_ARG(funarg) funarg->num_keyword_args++;funarg->max_args++
#define ADD_REST_ARG(funarg) funarg->has_rest_arg++;funarg->max_args++
//the one issue with this structure is looping throgh it requires
//two loop variables, one for the index in args and one for
//the index in num_req/opt/keyword_args
struct function_args{
  uint16_t num_req_args;//0-num_req_args are required
  uint16_t num_opt_args;//num_req_args-num_req_args+num_opt_args
  uint16_t num_keyword_args;//num_opt_args-num_opt_args+num_keyword_args
  uint16_t has_rest_arg;//0 or 1(only one keyword allowed
  symbol* args;
  int max_args;//number of args in c/llvm must be max_args
};
//typedef function_new* fxn_ptr
#define CALL_PRIM(fxn) (fxn.val.fun->comp)
struct function{//36 bytes
  function_args* args;//64
  CORD lname;//lambdas should be #<lambda{number via global counter}>(64)
  CORD cname;//name in c, and llvm I suppose(64)
  union {
    lambda* lam;
    funcall comp;
  };//(64)
  enum {//(32)
    _lambda_fun,
    _compiled_fun,
  } type;
};
struct macro{
  function_args* args;
  CORD lname;
  sexp body;
};
struct lambda{
  env *env;//for closures
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
  {"no-debug",0,0,'n'},
  {"output" ,1,0,'o'},
  {"quiet"  ,0,0,'q'},
  {"test"   ,0,0,'t'},
  {"version",0,0,'v'},
  {0,0,0,0}
};
//literal objects for each type
//should probably generate these or something
#define mkTypeSym(name,mval)                                    \
  static const sexp name = {.tag=_type, .val={.meta = mval}}
mkTypeSym(Qerror,_error);
mkTypeSym(Qfalse,_false);
mkTypeSym(Quninterned,_uninterned);
mkTypeSym(Qnil,_nil);
mkTypeSym(Qcons,_cons);
mkTypeSym(Qdouble,_double);
mkTypeSym(Qlong,_long);
mkTypeSym(Qbigint,_bigint);
mkTypeSym(Qbigfloat,_bigfloat);
mkTypeSym(Qchar,_char);
mkTypeSym(Qstr,_str);
mkTypeSym(Qfun,_fun);
mkTypeSym(Qsym,_sym);
mkTypeSym(Qspec,_special);
mkTypeSym(Qmacro,_macro);
mkTypeSym(Qtype,_type);
mkTypeSym(Qdpair,_dpair);
mkTypeSym(Qarr,_array);
mkTypeSym(Qtrue,_true);
mkTypeSym(Qlist,_list);
mkTypeSym(Qlenv,_lenv);
mkTypeSym(Qenv,_env);
mkTypeSym(Qobarray,_obarray);
mkTypeSym(Qfunargs,_funargs);
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
#define mkTypeCase(type,tag) case tag: return type  
static sexp typeOfTag(_tag tag){
  switch (tag){
    mkTypeCase(Qerror,_error);
    mkTypeCase(Qfalse,_false);
    mkTypeCase(Quninterned,_uninterned);
    mkTypeCase(Qnil,_nil);
    mkTypeCase(Qcons,_cons);
    mkTypeCase(Qdouble,_double);
    mkTypeCase(Qlong,_long);
    mkTypeCase(Qbigint,_bigint);
    mkTypeCase(Qbigfloat,_bigfloat);
    mkTypeCase(Qchar,_char);
    mkTypeCase(Qstr,_str);
    mkTypeCase(Qfun,_fun);
    mkTypeCase(Qsym,_sym);
    mkTypeCase(Qspec,_special);
    mkTypeCase(Qmacro,_macro);
    mkTypeCase(Qtype,_type);
    mkTypeCase(Qdpair,_dpair);
    mkTypeCase(Qarr,_array);
    mkTypeCase(Qtrue,_true);
    mkTypeCase(Qlist,_list);
    mkTypeCase(Qlenv,_lenv);
    mkTypeCase(Qenv,_env);
    mkTypeCase(Qobarray,_obarray);
    mkTypeCase(Qfunargs,_funargs);
  }
}
static sexp typeOf(sexp obj){
  return typeOfTag(obj.tag);
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
/*struct array_symref{
  CORD name;
  sexp val;
  int index;
  };*/
/*struct array_env{
  env* enclosing;
  array_symref head;
  };*/
#undef mkTypeCase
#undef mkTypeSym
