/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//Don't include this file directly, it's included by common.h

//including cord.h is weird, it includes "cord_pos.h" which isn't
//automatically installed for some reason so we specify the actual
//file we're including explicitly, as thats the easiest way to fix things
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
#endif
typedef enum sexp_tag sexp_tag;//different types of a lisp object
typedef enum TOKEN TOKEN;//type of values returned from yylex
typedef union lisp_data lisp_data;//core representation of a lisp object
typedef union funcall funcall;//type of primitive functions
typedef union ctype_val ctype_val;
typedef union data data;
typedef struct sexp sexp;//type of all lisp objects
typedef struct cons cons;//cons cell, for lists,pairs and everything else
//typedef struct fxn_proto fxn_proto;//primitive function prototype
typedef struct symbol symbol;//generic symbol type
typedef struct environment environment;//generic symbol namespace
typedef struct scoped_sexp scoped_sexp;//an sexp and it's containing environment
typedef struct obarray obarray;
typedef struct obarray_entry obarray_entry;
typedef struct obarray_env obarray_env;
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
typedef struct lisp_string lisp_string;
typedef struct subr subr;//any kind of subroutine(macro,function,special form,etc)
typedef struct frame frame;
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
#define CONSP(obj) (obj.tag == sexp_cons || obj.tag == sexp_list || obj.tag == sexp_dpair)
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
#define INTP(obj) (obj.tag == sexp_long || obj.tag==_ulong)
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
#define SEQUENCEP(obj) (CONSP(obj) || ARRAYP(obj))
#define SPECP(obj) (obj.tag== sexp_special)
#define STREAMP(obj)(obj.tag ==_stream)
#define STRINGP(obj) (obj.tag == sexp_str)
#define SYMBOLP(obj) (obj.tag == sexp_sym)
#define TYPEP(obj) (obj.tag == sexp_type)
#define TYPE_OR_NIL(obj,typecheck) (typecheck(obj) || NILP(obj))
#define UINT64P(obj) (obj.tag == sexp_ulong)
//hack
symbol *Etype;
#define NUM_EQ(obj1,obj2)                       \
  ((obj1.tag<=8?obj1.val.uint64:obj1.val.real64)== \
    (obj2.tag<=8?obj2.val.uint64:obj2.val.real64)
#define EQ(obj1,obj2)                                                   \
  ((NUMBERP(obj1) && NUMBERP(obj2))?NUM_EQ(obj1,obj2):                  \
   ((obj1.tag==obj2.tag) && (obj1.val.uint64 == obj.val.uint64)))
#define format_type_error_va(fun,format,args...)                        \
  ({CORD type_error_str;                                                \
    CORD_sprintf(&type_error_str,"type error in %r, ",fun);             \
    type_error_str=CORD_cat(type_error_str,format);                     \
    CORD_sprintf(&type_error_str,type_error_str,args);                  \
    raise_simple_error((uint64_t)Etype,make_string(type_error_str);})
#define format_type_error(fun,expected,got)                             \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r but got %r", \
               fun,expected,tag_name(got));                             \
  raise_simple_error((uint64_t)Etype,make_string(type_error_str));})
#define format_type_error_named(fun,name,expected,got)                  \
  ({CORD type_error_str;                                                \
    CORD_sprintf(&type_error_str,                                       \
                 "type error in %r, expected a(n) %r for %r but got a(n) %r", \
                 fun,expected,name,tag_name(got)),                      \
      raise_simple_error((uint64_t)Etype,make_string(type_error_str));})
#define format_type_error2(fun,expected1,got1,expected2,got2)           \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r and %r"   \
               ", but got %r and %r",fun,expected1,expected2,           \
               tag_name(got1),tag_name(got2)),                          \
    error_sexp(type_error_str)
#define format_type_error3(fun,expected1,got1,expected2,got2,expected3,got3) \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r,%r and %r" \
               ", but got %r,%r and %r",fun,expected1,expected2,expected3, \
               tag_name(got1),tag_name(got2),tag_name(got3)),           \
    error_sexp(type_error_str)
#define format_type_error_opt(fun,expected,got)                         \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r or no argument" \
               ", but got %r",fun,expected,tag_name(got)),              \
    error_sexp(type_error_str)
#define format_type_error_opt_named(fun,name,expected,got)              \
  CORD_sprintf(&type_error_str,"type error in %r,expected %r or nothing for argument %r" \
               ", but got %r",fun,expected,name,tag_name(got)),         \
    error_sexp(type_error_str)
#define format_type_error_key(fun,named,expected,got)   \
  format_type_error_opt_named(fun,named,expected,got)
#define format_type_error_opt2(fun,expected1,expected2,got)             \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r or %r"    \
               ", but got %r",fun,expected1,expected2,tag_name(got)),   \
    error_sexp(type_error_str)
#define format_type_error_opt2_named(fun,name,expected1,expected2,got)  \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r,%r or nothing" \
               "for %r, but got %r",fun,expected1,expected2,name,tag_name(got)), \
    error_sexp(type_error_str)
#define format_type_error_rest(fun,expected,failed_arg)                 \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r for the rest argument," \
               "but received an %r (value was %r)",                     \
               fun,expected,tag_name(failed_arg.tag),print(failed_arg)), \
    error_sexp(type_error_str)
#define const_real64_sexp(real64_val) {.tag=_real64,.val={.real64=real64_val}}
#define const_int64_sexp(int64_val) {.tag=_int64,.val={.int64=int64_val}}
#define const_uint64_sexp(uint64_val) {.tag=_uint64,.val={.uint64=uint64_val}}
//key point to this enum is that arithmetic types are numbered in their type
//hierarchy, ints by size < floats by size < bigint < bigfloat, if you add any
//new types make sure it fits in the hierarchy correctly
//TODO:
//This needs to be cleaned up some, so that all non pointer types (excepting bigint and bigfloat)
//come before all pointer types, also nil should be 0 
enum sexp_tag {
  sexp_nil = 0,
  sexp_char = 1, sexp_uchar = 1,
  //start of numbers
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
  //end of literals
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
  sexp_false = 40,
  sexp_true = 41,//type of #t, singular value
  sexp_obarray = 42,
  sexp_frame = 43,
  sexp_ctype=44,//c ffi type
  sexp_cdata=45,//c value and typeinfo(includes c pointer types)
  sexp_opaque=46,//generic opaque c struct/union
  sexp_resexp_data=47,//re match data
  sexp_hashsexp_table=48,sexp_hashtable=48,
  sexp_tree=49,
  sexp_treesexp_node=50,
  sexp_typedsexp_array=51,
  sexp_heap=52,
  sexp_sfmt=53,//random state,B
  sexp_uninterned=0xfd,
  sexp_unbound=0xfe,
  sexp_error=0xff,
};
union data {//keep max size at 64 bits
  FILE *stream;
  c_data *c_val;
  cons *cons;
  const char *simple_string;
  ctype *ctype;
  env *env;
  hash_table *hashtable;
  lisp_heap *heap;
  int8_t int8;
  int16_t int16;
  int32_t int32;
  int64_t int64;
  jmp_buf *label;
  lisp_tree *tree;
  lisp_string *string;
  macro *mac;
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
  uint64_t uint64;
  void *opaque;
  wchar_t uchar;
};
//I really want to make this 64 bits, but then I'd need to indrect for pretty much
//every data type except for integers
struct sexp {//96 bits,
  union{
    data val;//                             | 128
    uint64_t opaque_2;
  };
  uint8_t tag;
  uint16_t simple_len;
  unsigned int is_ptr :1;
  unsigned int padding :7;
  uint8_t meta;//opaque meta data for different things,
  //i.e, the type of an array, or string, or frame, etc...
};
struct cons {//32 bytes
  sexp car;
  sexp cdr;
};
enum TOKEN {
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
};
typedef enum {
  rec_none,
  rec_simple,
  rec_tail,
} recursion_type;
//lisp subroutine, either a builtin function, a special form, a compilier macro
//or a lisp function(a lambda) or a lisp macro
struct subr {
  lisp_string lname;//lambdas should be #<lambda{number via global counter}> 8 | 16
  union {
    cons *lambda;
    funcall comp;
  };//8 | 24
  lisp_string signature;//function signature 8 | 32
  lisp_string cname;//name in c, and llvm I suppose 8 | 40
  enum {// 4 | 44
    fun_lambda,
    fun_closure,
    fun_compiled,
    fun_compiler_macro,
    fun_special_form,
    fun_macro;
  } type;
  union{
    uint16_t num_req_args;//0-num_req_args are required
    uint16_t req_args;
  };
  uint16_t num_opt_args;//num_req_args-num_req_args+num_opt_args
  uint16_t num_keyword_args;//num_opt_args-num_opt_args+num_keyword_args
  uint16_t has_rest_arg;//0 or 1(only one restarg allowed)
  uint32_t maxargs;//extra 32 bits, so we can save a bit of  (4 | 48)
  recursion_type rec;
};
static inline CORD get_signature(function *fun_or_macro){
  return fun_or_macro->signature;
}
//defines what values are considered false
//currently, these are false,nil,numerical 0 or a null pointer
#define is_true(x)                               \
  (x.val == 0 || x.tag == sexp_real64 && x.val == 0.0)
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
static const sexp LISP_REAL32_EPSILON=const_real32_epsilon(FLT_EPSILON);
static const sexp LISP_REAL64_EPSILON=const_real64_epsilon(DBL_EPSILON);
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
    struct {//3-D,4-D
      uint16_t a;
      uint16_t b;
      uint16_t c;
      uint16_t d;
    };
    //pointer to a struct with matrix meta data
  };
  uint8_t dims;//max of 256 dimensions, could be enlarged if needed
  sexp_tag type;//not sure what this should be in a multityped array
  unsigned int padding :24;
}  
