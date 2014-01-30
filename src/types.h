/* Declaration and definitions of various structures and unions

Copyright (C) 2014 Tucker DiNapoli

This file is part of SciLisp.

SciLisp is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SciLisp is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SciLisp.  If not, see <http://www.gnu.org*/
#ifndef __COMMON_H__
#error "Don't include \"types.h\" directly; use \"common.h\" instead."
#endif

#include "gc/cord.h"
#include <string.h>
#include <setjmp.h>
#include <wchar.h>
#include <stdint.h>
#include <getopt.h>
#include <limits.h>
#include <float.h>
#include <gmp.h>
#include <mpfr.h>
#include <mpf2mpfr.h>
#ifdef USE_COMPLEX_H
#include <complex.h>
#undef I
typedef float complex imag32_t;
typedef double complex imag64_t;
//usable a lvalues unlike creal/cimag
#define realpart(x) (((double*)(&x))[0])
#define imagpart(x) (((double*)(&x))[1])
#define realpartf(x) (((float*)(&x))[0])
#define imagpartf(x) (((float*)(&x))[1])
#endif
typedef enum sexp_tag sexp_tag;//different types of a lisp object
typedef enum TOKEN TOKEN;//type of values returned from yylex
typedef union lisp_data lisp_data;//core representation of a lisp object
typedef union funcall funcall;//type of primitive functions (bad name)
typedef union ctype_val ctype_val;
typedef union data data;//actual data for any lisp object(should this be lisp_data?)
typedef struct sexp sexp;//type of all lisp objects
typedef struct cons cons;//cons cell, for lists,pairs and everything else
//typedef struct fxn_proto fxn_proto;//primitive function prototype
typedef struct symbol symbol;//generic symbol type
//symbol names, a string, a length, a hash value and some properties
typedef struct symbol_name symbol_name;
typedef struct environment environment;//generic symbol namespace
typedef struct package package;//lisp packages/modules
typedef struct scoped_sexp scoped_sexp;//an sexp and it's containing environment
typedef struct obarray obarray;//obarrays, actually hash tables for symbols
typedef struct ctype ctype;
typedef struct c_data c_data;
typedef struct re_match_data re_match_data;
typedef struct re_pattern_buffer regex_t;
typedef struct hash_table hash_table;//hash table for use in lisp(obarrays are mostly for c)
typedef struct hash_entry hash_entry;
typedef struct lisp_condition lisp_condition;//error handling
typedef struct lisp_string lisp_string;//string/CORD + length
typedef struct lisp_array lisp_array;//array/typed array/matrix
typedef struct subr subr;//any kind of subroutine(macro,function,special form,etc)
typedef struct frame frame;//a jmp_buf and information to reinitialize lisp environment
typedef frame *frame_addr;
typedef environment *env_ptr;
typedef symbol *symref;//type of generic symbol references
//typedefs akin to the ones in stdint.h and sml
typedef float real32_t;
typedef double real64_t;
typedef char char8_t;
typedef wchar_t char32_t;
//c macros to test for a specific type
#define ARRAYP(obj) (obj.tag == sexp_array)
#define AS_ARRAY(obj) (obj.val.array)
#define AS_CHAR(obj) (obj.val.utf8_char)
#define AS_DOUBLE(obj) (obj.val.real64)
#define AS_LONG(obj) (obj.val.int64)
#define AS_STRING(obj) (obj.val.cord)
#define AS_SYMBOL(obj) (obj.val.var)
#define BIGFLOATP(obj) (obj.tag == sexp_bigfloat)
#define BIGINTP(obj)(obj.tag == sexp_bigint)
#define BIGNUMP(obj) (obj.tag >= 1 && obj.tag <= 12)
#define CDATAP(obj) (obj.tag == sexp_cdata)
#define CHARP(obj) (obj.tag == sexp_char)
#define CONSP(obj) (obj.tag == sexp_cons)
#define CONS_OR_NIL(obj) TYPE_OR_NIL(obj,CONSP)
#define ENVP(obj)(obj.tag == sexp_env)
#define ERRORP(obj)(obj.tag == sexp_error)
#define FLOATP(obj) (obj.tag == sexp_double)
#define SUBRP(obj) (obj.tag == sexp_subr)
#define FUNP(obj) (obj.tag == sexp_fun && obj.val.fun->type == sexp_compiled_fun)
#define FUN_N_P(obj,n) (FUNP(obj) && obj.val.fun->args->max_args==n)
#define HASHTABLEP(obj) (obj.tag == sexp_hashtable)
#define INT16P(obj) (obj.tag == sexp_short)
#define INT32P(obj) (obj.tag == sexp_int)
#define INT64P(obj) (obj.tag == sexp_long)
#define INT8P(obj) (obj.tag == sexp_byte)
#define INTP(obj) (obj.tag == sexp_long || obj.tag== sexp_ulong)
#define INT_ANYP(obj)(obj.tag >=1 && obj.tag <= 8)
#define IS_POINTER(obj) (obj.is_ptr == 1)
#define LAMBDAP(obj) (obj.tag == sexp_fun && obj.val.fun->type == sexp_lambda_fun)
#define LISTP(obj) (obj.tag == sexp_list)
#define LITERALP(obj) (obj.is_ptr == 0)
#define MACROP(obj) (obj.tag == sexp_macro)
#define NILP(obj) (obj.tag == sexp_nil)
#define NUMBERP(obj) (obj.tag>=1 && obj.tag<=10)
#define OPAQUEP(obj) (obj.tag == sexp_opaque)
#define REAL32P(obj) (obj.tag == sexp_float)
#define REAL64P(obj) (obj.tag == sexp_double)
#define REALP(obj) (obj.tag == sexp_double || obj.tag == sexp_float)
#define REGEXP(obj)(obj.tag == sexp_regex)
#define RE_MATCHP(obj) (obj.tag == sexp_re_data)
#define SEQUENCEP(obj) (CONSP(obj) || (ARRAYP(obj) && obj.val.array->dims == 1) || STRINGP(obj))
#define STREAMP(obj)(obj.tag ==_stream)
#define STRINGP(obj) (obj.tag == sexp_str)
#define SYMBOLP(obj) (obj.tag == sexp_sym)
#define TYPEP(obj) (obj.tag == sexp_type)
#define TYPE_OR_NIL(obj,typecheck) (typecheck(obj) || NILP(obj))
#define UINT64P(obj) (obj.tag == sexp_ulong)
#define UNBOUND(obj) (obj.tag == sexp_unbound)
//errors, defined somewhere else
/*extern symbol *Etype;
extern symbol *Ekey;
extern symbol *Eargs;
extern symbol *Ebounds;
extern symbol *Efile;
extern symbol *Eread;
extern symbol *Eundefined;
extern symbol *E*/
#define NUM_EQ(obj1,obj2)                       \
  ((obj1.tag<=8?obj1.val.uint64:obj1.val.real64)== \
    (obj2.tag<=8?obj2.val.uint64:obj2.val.real64)
#define EQ(obj1,obj2)                                                   \
  ((NUMBERP(obj1) && NUMBERP(obj2))?NUM_EQ(obj1,obj2):                  \
   ((obj1.tag==obj2.tag) && (obj1.val.uint64 == obj.val.uint64)))
//macros to format error strings
#include "error_fmt.h"
#define const_real64_sexp(real64_val) {.tag=sexp_real64,.val={.real64=real64_val}}
#define const_real32_sexp(real32_val) {.tag=sexp_real32,.val={.real32=real32_val}}
#define const_int64_sexp(int64_val) {.tag=sexp_int64,.val={.int64=int64_val}}
#define const_uint64_sexp(uint64_val) {.tag=sexp_uint64,.val={.uint64=uint64_val}}
//key point to this enum is that arithmetic types are numbered in their type
//hierarchy, ints by size < floats by size < bigint < bigfloat, if you add any
//new types make sure it fits in the hierarchy correctly
//TODO:
//This needs to be cleaned up some, so that all non pointer types (excepting bigint and bigfloat)
//come before all pointer types, also nil should be 0
enum sexp_tag {
  //literals
  sexp_nil = 0,
  sexp_char = 1, sexp_uchar = 1,
  //numbers
  sexp_byte = 2,sexp_int8 = 2,
  sexp_ubyte = 3,sexp_uint8 = 3,
  sexp_short = 4,sexp_int16 = 4,
  sexp_ushort = 5,sexp_uint16 = 5,
  sexp_int = 6,sexp_int32 = 6,
  sexp_uint = 7,sexp_uint32 = 7,
  sexp_long = 8,sexp_int64 = 8,//type of integers1, value is int64
  sexp_ulong = 9,sexp_uint64 = 9,
  sexp_float = 10,sexp_real32 = 10,
  sexp_double = 11,sexp_real64 = 11,//type of floating point numbers1, value is real64
  //end of literals(mostly)
  sexp_bigint = 12,sexp_mpz=12,
  sexp_bigfloat = 13,sexp_mpfr=13,
  //end of numbers
  sexp_str = 20,sexp_string=20,//type of strings, value is cord
  sexp_array = 21,//type of arrays, element type in meta, vaule is array
  sexp_regex = 23,//compiled regular expression
  sexp_stream = 24,sexp_file=24,//type of input/output streams, corresponds to c FILE*
  sexp_matrix =25,//array for mathematical calculations
  sexp_cons = 26,//type of lists,value is cons
  sexp_subr=30,//typo of functions,macros,special forms and builtins
  sexp_sym = 32,sexp_symbol=32,//type of symbols,value is var
  sexp_type = 35,//type of types
  sexp_env = 38,sexp_environment=38,
  //maybo just a boolean type instead of two
  sexp_false = 40,//#f, singular value
  sexp_true = 41,//#t, singular value
  sexp_obarray = 42,
  sexp_frame = 43,
  sexp_ctype=44,//c ffi type
  sexp_cdata=45,//c value and typeinfo(includes c pointer types)
  sexp_opaque=46,//generic opaque c struct/union
  sexp_regexp_data=47,//re match data
  sexp_hash_table=48,sexp_hashtable=48,
  sexp_sfmt=53,//random state
  //internal use only
  sexp_c_char=0xfc,//stupid c standard, why can't char just be signed or unsigned
  sexp_uninterned=0xfd,
  sexp_unbound=0xfe,
  sexp_error=0xff,
};
union data {//keep max size at 64 bits
  uint64_t uint64;//just so this is the default type
  FILE *stream;
  c_data *c_val;
  cons *cons;
  const char *c_string;
  char c_char;//internal use, so strings can be cast to data*
  ctype *ctype;
  env_ptr env;
  hash_table *hashtable;
  int8_t int8;
  int16_t int16;
  int32_t int32;
  int64_t int64;
  frame_addr frame;
  lisp_string *string;
  mpfr_t *bigfloat;
  mpz_t *bigint;
  obarray* ob;
  real32_t real32;
  real64_t real64;
  regex_t *regex;
  re_match_data *re_data;
  lisp_array *array;
  subr *subr;
  symbol *sym;
  sexp_tag type;
  uint8_t uint8;
  uint16_t uint16;
  uint32_t uint32;
  void *opaque;
  wchar_t uchar;
};
//I really want to make this 64 bits, but then I'd need to indrect for pretty much
//every data type except for integers
struct sexp {//96 bits,
  data val;//                             | 128
  uint16_t simple_len;
  uint8_t tag;
  uint8_t meta;//opaque meta data for different things,
  unsigned int is_ptr :1;
  unsigned int padding :7;
  //i.e, the type of an array, or string, or frame, etc...
};
struct cons {//32 bytes
  sexp car;
  sexp cdr;
};
enum TOKEN {
  TOK_ERROR=-3,
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
  TOK_STRUDEL=26,//@
  TOK_COMMA=27,
  TOK_LIST_SPLICE=28,//,@
  TOK_HASH=29,//#
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
  TOK_DBL_LBRACE=56,//[[
  TOK_DBL_RBRACE=57,//]]
  TOK_MAT_OPEN=58,//"[|", start a literal blas compatiable matrix
  TOK_MAT_CLOSE=59,//"|]", close """"
};
//this is almost exactly the way emacs does builtin functions
union funcall{
  sexp(*f0)(void);
  sexp(*f1)(sexp);
  sexp(*f2)(sexp,sexp);
  sexp(*f3)(sexp,sexp,sexp);
  sexp(*f4)(sexp,sexp,sexp,sexp);
  sexp(*f5)(sexp,sexp,sexp,sexp,sexp);
  sexp(*f6)(sexp,sexp,sexp,sexp,sexp,sexp);
  sexp(*f7)(sexp,sexp,sexp,sexp,sexp,sexp,sexp);
  //more can be added if/when needed
  sexp(*fmany)(uint64_t,sexp*);
  sexp(*funevaled)(sexp);//sexp is presumably a list
  sexp(*fspecial)(sexp,env_ptr);
};
//for things like map and reduce
/*static inline sexp call_many_with_2(funcall f,sexp a,sexp b){
  sexp args[2]={a,b};
  return f.fmany(2,args);
}
static inline sexp  call_many_with_1(funcall f,sexp a){
  sexp args[1]={a};
  return f.fmany(1,args);
}
static inline sexp call_many_with_2_implicit(sexp a,sexp b){
  sexp (*f)(uint64_t,sexp*)=current_env->data_ptr->val.fun->comp.fmany;
  sexp args[2]={a,b};
  return f(2,args);
}
static inline sexp call_many_with_1_implicit(sexp a,sexp b){
  sexp (*f)(uint64_t,sexp*)=current_env->data_ptr->val.fun->comp.fmany;
  sexp args[1]={a};
  return f(1,args);
  }*/
typedef enum {
  rec_none,
  rec_simple,
  rec_tail,
} recursion_type;
enum subr_type {
    subr_lambda,
    subr_closure,
    subr_compiled,
    subr_compiler_macro,
    subr_special_form,
    subr_macro,
};
/*
  structure of strings in lisp,
  strings immutable, we use cords for actions that would normally use mutable strings
  ie sprintf, concatenation, modifying substrings etc
  strings are kept internally in utf-8 encoding (ie multibyte)
  the wide character string part is for representing strings
  as arrays of characters(which are currently wide characters)
*/
struct lisp_string {
  //kinda a silly union since a CORD is technically a typedef for const char*
  //but it makes code clearer in places
  //a lisp string is a cord if string[0] == '\0'
  union {
    const char *string;
    wchar_t *wide_string;//don't know if I'll use this or not but eh
    CORD cord;
  };
  uint32_t len;//length in bytes (i.e. for multibyte strings not the length in chars)
  uint8_t multibyte;//0=no,1=yes(2=widechar if that's ever valid)
};
struct lambda_list {
  cons *req_args;//( num_req_args . [reqargs ... ])
  cons *opt_args;//( num_opt_args . [(argname . default)...)]
  cons *key_args;//(num_key_args .  [[key . (var . default)]...])
  symbol *rest_arg;
};
//lisp subroutine, either a builtin function, a special form, a compilier macro
//or a lisp function(a lambda) or a lisp macro
struct subr {
  union {
    struct {
      funcall comp;
      uint16_t req_args;//42
      uint16_t opt_args;//num_req_args-num_req_args+num_opt_args 44
      uint16_t keyword_args;//num_opt_args-num_opt_args+num_keyword_args 46
      uint16_t rest_arg;//0 or 1(only one restarg allowed) 48
    };
    struct {
      lambda_list *lambda_arglist;
      cons *lambda_body;
    };    
  };//8
  lisp_string lname;//lambdas should be #<lambda{number via global counter}> 16
  lisp_string signature;//function signature 32
  lisp_string cname;//name in c, and llvm I suppose 40
  uint32_t maxargs;//52
  uint8_t subr_type;//53
  unsigned int rec_fun :2;//0 not-recursive,1 recursive, 2 tail recursive
  unsigned int pure_fun :1;//no change to it's arguments, should be most functions
  unsigned int const_fun :1;//returns the same result given the same arguments //53.5
  int :0;
  //in short it doesn't rely on pointers
};
static inline lisp_string get_signature(subr *lisp_subr){
  return lisp_subr->signature;
}
//defines what values are considered false
//currently, these are false,nil,numerical 0 or a null pointer
#define is_true(x)                               \
  (x.val.uint64 == 0 || x.tag == sexp_real64 && x.val.real64 == 0.0)
//possible compiler backends
enum backend{
  c_backend=0,
  llvm_backend=1,
  asm_backend=2,
  interpreter_backend=3,
};
enum operator{
  binop_add,
  binop_sub,
  binop_mul,
  binop_div,
  binop_pow,
  binop_max,
  binop_min,
  binop_logand,
  binop_logior,
  binop_logxor,
  binop_logandn,
  binop_logeqv,
  binop_lt,
  binop_le,
  binop_eq,
  binop_ne,
  binop_ge,
  binop_gt,
};
static const sexp LISP_INT64_MAX=const_int64_sexp(INT64_MAX);
static const sexp LISP_INT64_MIN=const_int64_sexp(INT64_MIN);
static const sexp LISP_UINT64_MAX=const_uint64_sexp(UINT64_MAX);
static const sexp LISP_REAL32_MAX=const_real32_sexp(FLT_MAX);
static const sexp LISP_REAL64_MAX=const_real64_sexp(DBL_MAX);
static const sexp LISP_REAL32_MIN=const_real32_sexp(FLT_MIN);
static const sexp LISP_REAL64_MIN=const_real64_sexp(DBL_MIN);
static const sexp LISP_REAL32_EPSILON=const_real32_sexp(FLT_EPSILON);
static const sexp LISP_REAL64_EPSILON=const_real64_sexp(DBL_EPSILON);
static const sexp lisp_int64_1=const_int64_sexp(1);
static const sexp lisp_int64_m1=const_int64_sexp(-1);
static const sexp lisp_int64_0=const_int64_sexp(0);
static const sexp lisp_real64_1=const_real64_sexp(1);
static const sexp lisp_real64_0=const_real64_sexp(0);
sexp lisp_bigint_0;
sexp lisp_bigint_1;
sexp lisp_bigfloat_0;
sexp lisp_bigfloat_1;
extern sexp get_type_from_string(CORD typestring);
/* structure for arrays, typed arrays, matrices and vectors */
struct lisp_array {
  union {
    sexp *vector;//1-D array of sexps
    sexp *array;//2-D array of sexps
    data *typed_vector;//1-D array of a specific type
    data *array_vector;//2-D array of a specific type
    //mathmatical arrays, for use with blas,lapack,etc
    double *double_matrix;
    float *float_matrix;
#ifdef HAVE_COMPLEX_H
    float complex *float_complex_matrix;
    double complex *double_complex_matrix;
#endif
    void *data;//anything else (specificly 3+ dimensional arrays)
  };
  union {
    uint64_t len;//1-D
    struct {//2-D
      uint32_t rows;
      uint32_t cols;
    };
    uint32_t *dimensions;
  };
  uint8_t type;//not sure what this should be in a multityped array
  uint8_t dims;//max of 256 dimensions, could be enlarged if needed
  int :0;
};
struct lisp_simple_vector {
  void *data;
  uint64_t len;
  uint8_t type;
};
int type=0;