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
typedef union ctype_val ctype_val;
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
typedef struct function_args function_args;
typedef struct obarray obarray;
typedef struct obarray_entry obarray_entry;
typedef struct obarray_env obarray_env;
typedef struct macro macro;
typedef struct ctype ctype;
typedef struct c_data c_data;
typedef struct symbol_props symbol_props;
typedef struct re_match_data re_match_data;
typedef struct re_pattern_buffer regex_t;
typedef struct hash_table hash_table;
typedef struct lisp_tree lisp_tree;
typedef struct hash_table hash_table;
typedef struct hash_entry hash_entry;
typedef struct lisp_heap lisp_heap;
typedef struct lisp_condition lisp_condition;//error handling
typedef const sexp(*sexp_binop)(sexp,sexp);//not used
typedef const char* restrict c_string;//type of \0 terminated c strings
typedef symbol *symref;//type of generic symbol referances
typedef local_symbol *local_symref;//"" local ""
typedef symbol *keyword_symref;
typedef symbol keyword_symbol;
//typedefs akin to the ones in stdint.h and sml
typedef float real32_t;
typedef double real64_t;
typedef char char8_t;
typedef wchar_t char32_t;
//typedef fxn_proto* fxn_ptr;//pointer to primitive function
//c macros to test for a specific type
#define NILP(obj) (obj.tag == _nil)
#define CONSP(obj) (obj.tag == _cons || obj.tag == _list || obj.tag == _dpair)
#define LISTP(obj) (obj.tag == _list)
#define NUMBERP(obj) (obj.tag == _double || obj.tag == _long)
#define FLOATP(obj) (obj.tag == _double)
#define AS_DOUBLE(obj) (obj.val.real64)
//this may change if I actually implement shorter int types
#define INTP(obj) (obj.tag == _long)
#define CHARP(obj) (obj.tag == _char)
#define AS_LONG(obj) (obj.val.int64)
#define SYMBOLP(obj) (obj.tag == _sym)
#define AS_SYMBOL(obj) (obj.val.var)
#define SPECP(obj) (obj.tag== _special)
#define STRINGP(obj) (obj.tag == _str || obj.tag == _ustr)
#define AS_STRING(obj) (obj.val.cord)
#define CHARP(obj) (obj.tag == _char)
#define AS_CHAR(obj) (obj.val.utf8_char)
#define FUNP(obj) (obj.tag == _fun && obj.val.fun->type == _compiled_fun)
#define FUN_N_P(obj,n) (FUNP(obj) && obj.val.fun->args->max_args==n)
#define FUN0P(obj) FUN_N_P(obj,0)
#define FUN1P(obj) FUN_N_P(obj,1)
#define FUN2P(obj) FUN_N_P(obj,2)
#define FUN3P(obj) FUN_N_P(obj,3)
#define ARRAYP(obj) (obj.tag == _array)
#define AS_ARRAY(obj) (obj.val.array)
#define LAMBDAP(obj) (obj.tag == _fun && obj.val.fun->type == _lambda_fun)
#define FUNCTIONP(obj)(obj.tag == _fun)
#define STREAMP(obj)(obj.tag ==_stream)
#define REGEXP(obj)(obj.tag == _regex)
#define ERRORP(obj)(obj.tag == _error)
#define SEQUENCEP(obj) (CONSP(obj) || ARRAYP(obj))
#define BIGINTP(obj)(obj.tag == _bigint)
#define BIGFLOATP(obj) (obj.tag == _bigfloat)
#define BIGNUMP(obj) (NUMBERP(obj) || BIGINTP(obj) || BIGFLOATP(obj))
#define ENVP(obj)(obj.tag == _env)
#define MACROP(obj) (obj.tag == _macro)
#define INT8P(obj) (obj.tag == _byte)
#define INT16P(obj) (obj.tag == _short)
#define INT32P(obj) (obj.tag == _int)
#define INT64P(obj) (obj.tag == _long)
#define REAL32P(obj) (obj.tag == _float)
#define REAL64P(obj) (obj.tag == _double)
#define KEYWORDP(obj) (obj.tag == _keyword)
#define HEAPP(obj) (obj.tag == _heap)
#define TYPEP(obj) (obj.tag == _type)
#define RE_MATCHP(obj) (obj.tag == _re_data)
#define TYPED_ARRAYP(obj) (obj.tag==_typed_array)
#define TYPE_OR_NIL(obj,typecheck) (typecheck(obj) || NILP(obj))
#define CONS_OR_NIL(obj) TYPE_OR_NIL(obj,CONSP)
#define LITERALP(obj) (obj.is_ptr == 0)
#define IS_POINTER(obj) (obj.is_ptr == 1)
#define LISP_TREEP(obj) (obj.tag == _tree)
#define HASHTABLEP(obj) (obj.tag == _hashtable)
#define CDATAP(obj) (obj.tag == _cdata)
#define format_type_error(fun,expected,got)                             \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r but got %r", \
               fun,expected,tag_name(got)),                             \
  error_sexp(type_error_str)
#define format_type_error_named(fun,expected,got,name)                      \
  CORD_sprintf(&type_error_str,                                         \
               "type error in %r, expected a(n) %r for %r but got a(n) %r", \
               fun,expected,name,tag_name(got)),                        \
    error_sexp(type_error_str)
#define format_type_error2(fun,expected1,got1,expected2,got2)           \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r and %r"   \
               ", but got %r and %r",fun,expected1,expected2,           \
               tag_name(got1),tag_name(got2)),                          \
  error_sexp(type_error_str)
#define format_type_error3(fun,expected1,got1,expected2,got2,expected3,got3) \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r,%r and %r"   \
               ", but got %r,%r and %r",fun,expected1,expected2,expected3, \
               tag_name(got1),tag_name(got2),tag_name(got3)),           \
  error_sexp(type_error_str)
#define format_type_error_opt(fun,expected,got)                         \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r or no argument" \
               ", but got %r",fun,expected,tag_name(got)),   \
  error_sexp(type_error_str)
#define format_type_error_opt_named(fun,name,expected,got)              \
  CORD_sprintf(&type_error_str,"type error in %r,expected %r or nothing for argument %r" \
               ", but got %r",fun,expected,name,tag_name(got)),         \
  error_sexp(type_error_str)
#define format_type_error_key(fun,named,expected,got)                   \
  format_type_error_opt_named(fun,named,expected,got)
#define format_type_error_opt2(fun,expected1,expected2,got)             \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r or %r"    \
               ", but got %r",fun,expected1,expected2,tag_name(got)),   \
  error_sexp(type_error_str)
#define const_real64_sexp(real64_val) {.tag=_real64,.val={.real64=real64_val}}
#define const_int64_sexp(int64_val) {.tag=_int64,.val={.int64=int64_val}}
#define const_uint64_sexp(uint64_val) {.tag=_uint64,.val={.uint64=uint64_val}}
//key point to this enum is that arathmatic types are numbered in their type
//heriarchy, ints by size < floats by size < bigint < bigfloat, if you add any
//new types make sure it fits in the heirachy correcty
enum _tag {
  _unbound=-0xf,
  _error = -4,//type of errors, value is a string
  _false = -3,//type of #f, actual value is undefined
  _uninterned = -2,//type of uninterned symbols, value is symbol(var)
  _nil = -1,//type of nil, singular object,vaule is undefined
  //so this is kinda really not good.
  /* so I just realized that a whole bunch of my code kinda
   * implicitly depends on _cons being 0, because I tend not
   * to set tag values to each cell when building lists the tag is
   * 0, so when I test if it's a cons cell it is, which, to be fair does 
   * seem to work out. But it's worth knowing this, because if i ever change
   * this to a non 0 value everything will probably break*/
  _cons = 0,//type of cons cells(aka lisp programs), value is cons
  //arithmatic types, room for 7 more types currently
  //unlike some other enum aliases these are both meant to be useable
  //they exist for convience
  _byte = 1,_int8 = 1,
  _ubyte = 2,_uint8 = 2,
  _short = 3,_int16 = 3,
  _ushort = 4,_uint16 = 4,
  _int = 5,_int32 = 5,
  _uint = 6,_uint32 = 6,
  _long = 7,_int64 = 7,//type of integers, vaule is int64
  _ulong = 8,_uint64 = 8,
  _float = 9,_real32 = 9,
  _double = 10,_real64 = 10,//type of floating point numbers, value is real64
  _bigint = 11,_mpz=11,
  _bigfloat = 12,_mpfr=12,
  _char = 19,_uchar=19,//type of chars(c type wchar_t),value is utf8_char
  _str = 20,_cord=20,_string=20,//type of strings, value is cord
  _array = 21,//type of arrays, element type in meta, vaule is array
  _ustr = 22,//utf8 string
  _regex = 23,//compiled regular expression
  _stream = 24,_file=24,//type of input/output streams, corrsponds to c FILE*
  _matrix =25,//array for mathematical calculations
  _list = 26,//type of lists,value is cons
  _dpair = 27,_dotted_pair=27,
  _fun = 31,//type of builtin functions,value is function pointer,fun
  _sym = 32,_symbol=32,//type of symbols,value is var
  _special = 33,_spec=33,//type of special form,value is meta(a _tag value)
  _macro = 34,//type of macros, unimplemented
  _type = 35,//type of types, unimplemened
  _lam = 36,//type of lambda, value is lam
  _lenv = 37,//type of local environments,value is lenv
  _env = 38,_environment=38,
  _keyword = 39,
  _funarg = 40,_funargs=40,
  _true = 41,//type of #t, singular value
  _obarray = 42,
  _label = 43,//a c jmp_buf, in lisp a target for return or go
  _ctype=44,//c ffi type
  _cdata=45,//c value and typeinfo(includes c pointer types)
  _opaque=46,//generic opaque c struct/union
  _re_data=47,//re match data
  _hash_table=48,_hashtable=48,
  _tree=49,
  _tree_node=50,
  _typed_array=51,
  _heap=52,
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
  _eval=14,//shouldn't be a special form
  _defmacro=15,
  _quasi=16,
  _quote=17,
  _comma=18,
  _and=19,//definately shouldn't be a special form
  _or=20,//"" ""
  _main=21,
  _while=22,
  _prog1=23,//shouldn't be a special form
  _dolist=24,//while should be the only special looping construct
  _dotimes=25,//""""
  _return=26,
  _unwind_protect=27,
  _block=28,
  _defvar=29,
  _defconst=30,
  _flet=31,
  _let_star=32,
  _progv=33,
};
//mutually exclusive metadata, a single int value might have multiple names
enum sexp_meta{
  _basic_tree=1,_avl_tree=2,_rb_tree=3,_splay_tree=4,
  _red_node=1,_black_node=0,_leaf=2,
  _double_array=_double,_float_array=_float,
  _real64_array=_real64,_real32_array=_real32,
  _long_array=_long,
  _utf8_string=_char,
  _splice_list=_list,
  _builtin_macro=_macro,
};
union data {//keep max size at 64 bits
  CORD cord;
  FILE *stream;
  _tag meta;
  c_data *c_val;
  c_string string;//unused I think
  cons *cons;
  ctype *ctype;
  data *typed_array;
  env *cur_env;
  function *fun;
  function_args* funarg;//depreciated
  function_args* funargs;
  hash_table *hashtable;
  lisp_heap *heap;
  int8_t int8;
  int16_t int16;
  int32_t int32;
  int64_t int64;
  jmp_buf *label;
  keyword_symref keyword;
  lisp_tree *tree;
  local_symref lenv;
  macro *mac;
  mpfr_t *bigfloat;
  mpz_t *bigint;
  obarray* ob;
  real32_t real32;
  real64_t real64;
  regex_t *regex;
  re_match_data *re_data;
  sexp *array;
  special_form special;
  symref var;
  uint8_t uint8;
  uint16_t uint16;
  uint32_t uint32;
  uint64_t uint64;
  void *opaque;
  wchar_t *ustr;
  wchar_t uchar;//try to change all utf8_chars to this
  wchar_t utf8_char;//depreciated
};
//meta is for mutualy exclusvie information
//whlie the next 8 bits are for inclusive information
struct sexp{//128 bits/16 bytes
  _tag tag;//could be shorter if need be  | 32
  sexp_meta meta : 8;//random metadata    | 40
  unsigned int quoted :2;//               | 42
  int setfable :1;//                      | 43
  int has_comma :1;//                     | 44
  int is_ptr:1;//                         | 45
  //3 bits free
  uint16_t len;//length of a sequence     | 61
  data val;//                             | 128
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
  TOK_LET=31,
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
  TOK_DBL_LBRACE=56,
  TOK_DBL_RBRACE=57,
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
  uint16_t has_rest_arg;//0 or 1(only one restarg allowed
  symbol* args;
  union{
    int max_args;//number of args in c/llvm must be max_args
    int maxargs;//because I tend to use this by mistake
  };
};
//The layout of the function and macro structures is important, the first three
//fields are the same. When calling a builtin macro it is treated for the most
//part as a function, so if the args,lname ,and comp fields don't match we'll
//get and error when evaling builtin macros
#define CALL_PRIM(fxn) (fxn.val.fun->comp)
struct function{//36 bytes
  function_args* args;//64
  CORD lname;//lambdas should be #<lambda{number via global counter}>(64)
  union {
    lambda* lam;
    funcall comp;
  };//(64)
  CORD cname;//name in c, and llvm I suppose(64)
  enum {//(32)
    _lambda_fun,
    _compiled_fun,
    _compiled_macro=_builtin_macro,
  } type;
  uint32_t maxargs;//extra 32 bits, so we can save a bit of
  //indirection, though I probably won't use this
};
struct macro{
  function_args* args;
  CORD lname;
  union {
    sexp body;
    funcall comp;
  };
};
struct lambda{
  env *env;//for closures
  sexp body;
};
//defines what values are considered false
//currently, these are false,nil,numerical 0 or a null pointer
#define isTrue(x)                                                       \
  (x.tag == _false ? 0 :                                                \
   (x.tag == _nil ? 0 :                                                 \
     (x.tag == _double ? (x.val.real64 == 0.0 ? 0 : 1) :                \
      ((x.tag == _long || x.is_ptr) ? (x.val.int64 == 0 ? 0 : 1) : 1))))
//possible compilier backends
enum backend{
  c=0,
  llvm=1,
  as=2,
};
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
static const sexp LISP_INT64_MAX=const_int64_sexp(INT64_MAX);
static const sexp LISP_INT64_MIN=const_int64_sexp(INT64_MIN);
static const sexp LISP_UINT64_MAX=const_uint64_sexp(UINT64_MAX);
/*static sexp make_sexp_from_type_and_data(data val,_tag type){
  switch(type){
  case _double:return double_sexp(val);
  case _long:return long_sexp(val);
  }
  }*/
/*static sexp lisp_copy(sexp obj);
static sexp copy_array(sexp arr){
  sexp *retval=xmalloc(sizeof(sexp)*arr.len);
  memcpy(retval,arr.val.array,arr.len*sizeof(sexp));
  return array_sexp(retval,arr.len);
}
static sexp copy_symref(sexp sym){
  sexp retval=sym;
  retval.val.var=xmalloc(sizeof(symbol));
  *retval.val.var=*sym.val.var;//shallow copy, copy name and props
  retval.val.var->val=lisp_copy(sym.val.var->val);
  return retval;
}

static sexp lisp_copy(sexp obj){
  if(!IS_POINTER(obj)){
    return obj;
  }
  sexp retval;
  retval=obj;//shallow copy, to copy tag and metadata
  switch(obj.tag){
    case _list:
    case _cons:
      return copy_cons(obj);
    case _array:
      return copy_array(obj);
    case _sym:
      return copy_symref(obj);
  }
  }*/
