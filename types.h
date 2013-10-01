typedef enum _tag _tag;
typedef enum TOKEN TOKEN;
typedef enum special_form special_form;
typedef union data data;
typedef struct sexp sexp;
typedef struct cons cons;
typedef struct symref symref;
typedef struct fxn_proto fxn_proto;
//typedef sexp YYSTYPE;
typedef const sexp(*sexp_binop)(sexp,sexp);
typedef const char* restrict c_string;
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
  _comma=18
};
union data {
  double real64;
  long int64;
  wchar_t utf8_char;
  c_string string;
  cons* cons;
  symref* var;
  void* fun;
  special_form special;
};
struct sexp{
  _tag tag;
  data val;
};
struct symref{
  const char* restrict name;
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
struct fxn_proto{
  int num_args;
  _tag* arg_types;
  void* function;
};
