#include "common.h"
#define CORD_BUFSZ 256
#include "ec.h"
#include "common.h"
#include "extra/read_tables.h"
//Need to add eof testing in here somewhere
struct array_stream {
  const char *arr;
  uint32_t index;
  uint32_t len;
};
#define arr_getc(arr) (arr.arr[arr.index++])
#define arr_peek(arr) (arr.arr[arr.index])
#define arr_ungetc(arr) (arr.index--)
#define arr_get_str(arr,n)                      \
  ({char *str=xmalloc_atomic(n);                \
    memcpy(n,str,MAX(n,arr.len));               \
    str;})
//assume null terminated array
#define arr_eat_line(arr)                           \
  ({char *endptr=strchr(arr.arr,'\n');              \
  arr.index=(arr.arr-endptr)+1;                     \
  arr.arr=endptr+1;                                 \
  ;})
#define arr_get_line(arr)                       \
  ({char *endptr=strchr(arr,'=n');              \
    char *str=xmalloc_atomic(endptr-arr.arr+1); \
    memcpy(str,arr,arr,entptr-arr.arr);         \
    str[endptr-arr.arr]='\0';                   \
    arr.index=(endptr-arr.arr)+1;               \
    arr.arr=endptr+1;                           \
    str;})

#define stream_getc(stream) (getc(stream))
#define stream_peek(stream)                     \
  ({char c=getc(stream);                        \
    ungetc(stream);                             \
    c;})
#define stream_ungetc(stream) (ungetc(stream))
#define stream_get_str(stream,n)                \
  ({char *str=xmalloc_atomic(n);                \
    fread(str,n,1,stream);                      \
    str;})
#define stream_eat_line(stream)                 \
  (while(getc(stream)!='\n');)
//we can't use the libc getline because of gc
#define stream_get_line(stream)                 \
  ({uint32_t size=16;                           \
    int i=0;                                    \
    char *str=xmalloc_atomic(16);               \
    char c;                                     \
  LOOP:for(;i<size;i++){                        \
      c=str[i++]=getc(stream);                  \
      if(c=='\n' || c=='\0'){                   \
        break;                                  \
      }                                         \
    }                                           \
    if(i>=size){                                \
      str=xrealloc(str,(size*=2));              \
      goto LOOP;                                \
    }                                           \
    str;})

#define generic_getc(type,obj) type##_getc(obj)
#define generic_peek(type,obj) type##_peek(obj)
#define generic_ungetc(type,obj) type##_ungetc(obj)
#define generic_get_str(type,obj,len) type##_get_str(obj,len)
#define generic_eat_line(type,obj) type##_eat_line(obj)
#define generic_get_line(type,obj) type##_get_line(obj)
//this is because the actual function used has an awful name
#define string_scan_chars(string,chars)         \
  (strpbrk(string,chars))
/*different possible streams to read from:
  CORDs, using CORD_pos
    also CORDs are used for reading from functions via
    CORD_from_fn
  FILES, using standard getc/ungetc
  character arrays, using some struct for internal state
 */
//char c=read_char(intput);if(c<0)... would also work
#define READ_CHAR_MULTIBYTE(input,mb_ptr)       \
  ({uint8_t c= read_char(input);                \
    if(c>=0x80){                                \
      *mb_ptr=1;                                \
    }                                           \
    c;})

#define is_valid_symbol_char(c)                 \
  (!invalid_symbol_char[(uint8_t)c])
#define is_invalid_symbol_char(c)               \
  (invalid_symbol_char[(uint8_t)c])
//I don't know how I'm going to do input, but for now
//assume I have functions read_char and unread_char
//to get and put back characters, that should be generic
//enough for anything

//Also I should use a dispatch table rather that switches
sexp internal_read(char *input,int *pch,int flags){
  char c;
  *pch=0;
  while((c=(read_char(input)))!=EOF){
    switch(c){
      case ' ':
      case '\n':
      case '\t':
      case '\v':
        continue;
      case ';':
        eat_line(input);
        continue;
      case '(':
        return read_list(input);
      case '[':
        return read_array(input);
      case ')':
      case '}':
      case ']':
      case '.':
        *pch=c;
        return NIL;
      case '"'
        return read_double_quoted_string(input);
      case '?'
        return read_char_literal(input);
      case '\'':{
        return c_list2(Qquote,read_sexp(input));
      }
      case '|':
        return read_symbol_verbatim(input);
      case '#':
        return read_sharp(input);
      default:
        return read_symbol_or_number(input);
    }
  }
}
sexp read_bigint(char *input,int radix){
  int64_t num;
  char *endptr;
  errno=0;
  //two reasons to do this, first if the number will fit
  //into a normal int64 we can initialize from that rather
  //than a string, and even if not this will still tell us
  //how long the number is
  num=strtol(input,&endptr,radix);
  if(errno){
    if(errno==ERANGE){
      mpz_t bignum;
      //mpz expects a null terminated string, so to avoid coping the entire
      //number (which has to be pretty long) we store the first invalid
      //character and then set that location to null, and then
      //restore it afterword
      char end=*endptr;
      *entptr='\0';
      mpz_init_set_str(bignum,input,radix);
      *endptr=end;
      return bigint_sexp(bignum);
    }
    //invalid sequence
  } else {
    mpz_t bignum;
    mpz_init_set_si(bignum,num);
    return bigint_sexp(bignum);
  }
}
#define twos_compliment(num) (~num+1)
//abstracted in case for some reason I don't want to use strtol
sexp read_integer(char *input,int radix){
  int64_t num;
  char *endptr;
  errno=0;
  num=strtol(input,&endptr,radix);
  if(errno){
    current_env->error_num=errno;
    raise_simple_error(Esystem,"Error in strtol");
  }
  return int64_sexp(num);
}

#endif
sexp read_sharp(char *input){
  switch(*input++){
    case '|':{
      int comment_depth=1;
      char *pipe_ptr;
      while(comment_depth){
        pipe_ptr=strchr(input,'|');
        if(!pipe_ptr){
          raise_simple_error(Eread,"Error unterminated comment");
        }
        if(*(pipe_ptr-1)=='#'){
          comment_depth++;
        } else if (*(pipe_ptr+1)=='#'){
          comment_depth--;
        }
        input+pipe_ptr;
      }
    }
    case 's':
      return read_hash_table(input);
    case 'x':
    case 'X':
      return read_integer(input,16);
    case 'Z':
    case 'z':
      return read_bigint(input,0);
    case 'F':
    case 'f':
      return read_bigfloat(input,0);
    case '<':{//this already means a read error, but try to get a better description
      char *invalid_end=strchr(input,'>');
      if(!invalid_end){
        raise_simple_error(Eread,"Error unterminated invalid read");
      } else {
        lisp_string invalid_obj=xmalloc(sizeof(lisp_string));
        lisp_string->string=input-1;
        lisp_String->len=invalid_end-(input-1);
        raise_sexp_error(Eread,string_sexp(invalid_obj));
      }

    }
  }
}
#define HEXVALUE(c)                                     \
  (((c) >= 'a' && (c) <= 'f')                           \
   ? (c)-'a'+10                                         \
   : (c) >= 'A' && (c) <= 'F' ? (c)-'A'+10 : (c)-'0')
//large ammounts of this taken from the bash printf builtin
static inline char parse_simple_escape(char escape_char){
  //shamelessly stolen from emacs out of shear lazyness
  switch(escape_char){
    case 'a':
      return 0x7;
    case 'b':
      return '\b';
    case 'd':
      return 0x7f;
    case 'e':
      return 0x1b;
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\\':
      return '\\';
    case '0':
      return '\0';
    case 'x':
    case 'u':
    case 'U':
      return (char)-1;
      //\char = char for any non special char, this implictly includes
      //\? for chars and \" for strings, this does mean however
      //that \<non ascii unicode char> will cause an error
    default:
      if(((uint8_t)c)>0x80){
        raise_simple_error(Eread,"Illegial unicode character following a \\");
      }
      return escape_char;
  }
}
//rewrite to use read_char
int parse_escape_internal(char *input,char** output,int *outlen){
  //input is the text immediately following a backslash
  *output=parse_simple_escape(*input);
  if(*output != ((char)-1)){
    if(outlen){
      *outlen=1;
    }
    return 1;
  }
  int temp=0;
  uint32_t uvalue;
  int udigits=0;
  char *p=input+1;
  switch(*input++){
    case 'x':{
      //note to self: --  (prefix or postfix) has higer precidence than &&
      uvalue=parse_hex_escape(p);
      //parse_escape raises an error on a malformed escape sequence
      *output=uvalue;
      if(outlen){
        *outlen=1;
      }
      return p-input;
    }
    case 'U':
      udigits=8;
    case 'u':
      if(!udigits){
        udigits=4;
      }
      uvalue=parse_unicode_escape(p);
      //parse_escape raises an error on a malformed escape sequence
      uint64_t utf8_val;
      if(utf8_encode_char((uint8_t*)&utf8_val,uvalue)==(size_t)-1){
        raise_simple_error(Eread,"Invald unicode seqence");
      }
      if(outlen){
        outlen=utf8_mb_char_len((uint8_t*)&utf8_val)[0];
      }
      *(uint64_t**)output=utf8_val;
      return p-input;
    default:
      assert(0);
  }
}
char parse_hex_escape(char *input){//sets input to the first character after the escape
  if(!(isxdigit(*input))){
    raise_simple_error(Eread,"error lexing char, expected hex digit after \\x\n");
  }
  if(isxdigit(*input+1)){
    //pretty sure I can't just use strtol because \x010 is 1, not 10
    //sets input to input+2 and returns the hexvalue of *input,*input+1
    return((16*HEXVALUE(*input++))+HEXVALUE(*input++));
  } else {
    return (HEXVALUE(*input++));
  }
}
uint32_t parse_unicode_escape(uint8_t *input,int udigits){
  uint32_t uvalue;
  temp=udigits;
  for (uvalue = 0; isxdigit(*input) && --temp; input++){
    uvalue <<= 4;
    uvalue += HEXVALUE(*input);
  }
  if (temp == udigits){
    raise_simple_error(Eread,"error lexing char, expected hex digit after \\u or \\U\n");
  }
  return uvalue;
}
sexp read_symbol_verbatim(char *input){
  int i=0;
  int mb=0;
  char c;
  CORD_ec buf;
  CORD_ec_init(buf);
  while(1){
    c=read_char(input);
    if(c=='\\'){
      CORD_ec_append(read_char(input));
    } else if (c=='|'){
      break;
    } else {
      CORD_ec_append(READ_CHAR_MULTIBYTE(input,&mb));
    }
  }
}
static inline sexp symbol_from_ec_cord(CORD_ec buf){
  uint32_t len;
  if(!buf[0].ec_CORD){
    len=buf[0].ec_buf-buf[0].ec_bufptr;
    return c_intern(buf[0].ec_buf,len,current_obarray);
  } else {
    CORD name=CORD_ec_to_cord(buf);
    len=CORD_len(name);
    return c_intern_no_copy(CORD_to_const_char_star(name),len,current_obarray);
  }
  sym_name->len=len;
  sym_name->multibyte=mb;
}
static sexp read_double_quoted_string(char *input){
  CORD_ec buf;
  CORD_ec_init(buf);
  uint8_t c;
  int len;
  int mb;
  while(1){
    c=read_char(input);
    if(c=='\\'){
      char *escape;
      int nbytes;
      //this should always return a vaild length
      //as it raises and exception on an invalid escape
      //parse escape needs a character oriented interface
      parse_escape_internal(input,&escape,&nbytes);
      len+=nbytes;
      if(nbytes>1){
        mb=1;
        while(nbytes>0){
          CORD_ec_append(*escape++);
          nbytes--;
        }
      } else {
        CORD_ec_append(*escape);
      }
      continue;
    } else if(c=='"'){
      break;
    } else {
      //I don't check for valid unicode input here, should I?
      CORD_ec_append(c);
      len++;
    }
  }
  lisp_string *retval;
  if(buf[0].ec_cord){
    retval=xmalloc(sizeof(lisp_string));
    retval->cord=CORD_ec_to_cord(buf);
  } else {
    retval=xmalloc_atomic(sizeof(lisp_string)+len);
    retval->string=retval+sizeof(lisp_string);
    memcpy(retval->string,buf[0].ec_buf,len);
  }
  retval->len=len;
  retval->multibyte=mb;
  return string_sexp(retval);
}
//move these somewhere else 
//functions to convert an extendable cord to a lisp or c string
//optimizing the case where less that CORD_BUFSZ chars have
//been read into the ec cord
lisp_string *CORD_ec_to_lisp_string(CORD_ec buf,uint32_t len,int mb){
  lisp_string *retval;
  if(buf[0].ec_cord){
    retval=xmalloc(sizeof(lisp_string));
    retval->cord=CORD_ec_to_cord(buf);
  } else {
    retval=xmalloc_atomic(sizeof(lisp_string)+len);
    retval->string=retval+sizeof(lisp_string);
    memcpy(retval->string,buf[0].ec_buf,len);
  }
  retval->len=len;
  retval->multibyte=mb;
  return retval;
}
char *CORD_ec_to_char_star(CORD_ec buf){
  if(buf[0].ec_cord){
    return CORD_to_const_char_star(CORD_ec_to_cord(buf));
  } else {
    int len = buf[0].ec_bufptr-buf[0].ec_buf+1;
    char *retval=xmalloc_atomic(len);
    memcpy(retval,buf[0].ec_buf,len);
    return netval;
  }
}
sexp read_symbol_or_number(char *input){
  CORD_ec buf;
  CORD_ec_init(buf);
  uint8_t c;
  int len;
  int mb;
  while((c==read_char(input))){
    if(invalid_symbol_char[c]){
      raise_simple_error_fmt("invalid chararcter %c in symbol",c);
     }   
    if(c=='\\'){
      CORD_ec_append(read_char(input));
    } else {
      CORD_ec_append(c);
    }
    len++;
  }
  char *str;//=CORD_ec_to_char_star(buf);
  if(string_scan_chars(str,"0123456789")){
    sexp maybe_num=maybe_read_number(str);
    if(!NILP(maybe_num)){
      return maybe_num;
    }
  }
}
