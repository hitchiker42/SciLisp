#include "common.h"
#define CORD_BUFSZ 256
#include "ec.h"
#include "common.h"

//char c=read_char(intput);if(c<0)... would also work
#define READ_CHAR_MULTIBYTE(input,mb_ptr)       \
  ({uint8_t c= read_char(input);                \
    if(c>=0x80){                                \
      *mb_ptr=1;                                \
    }                                           \
    c;})
//I don't know how I'm going to do input, but for now
//assume I have functions read_char and unread_char 
//to get and put back characters, that should be generic
//enough for anything

//Also I should use a dispatch table rather that switches
sexp internal_read(char *input,int *pch,int flags){
  char c;
  *pch=0;
  while(c=*input++){
    switch(c){
      case ' ':
      case '\n':
      case '\t':
      case '\v':
        continue;
      case ';':
        while((c=*input++)!='\n');
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
        sexp val=read_sexp(input);
        return c_list2(Qquote,val);
      }
      case '|':
        return read_symbol_verbatim(input);
      case '#':
        return read_sharp(input);
        
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
      //this is because lisp uses question marks to express characters
    case '?':
      return '?';
    case '"':
      return '"':
    case '0':
      return '\0';
    default:
      return (char)-1;
  }
}

int parse_escape_internal(char *input,char** output){//input is the text immediately following a backslash
  *output=parse_simple_escape(*input);
  if(*output != ((char)-1)){
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
      if(uvalue == (char)-1 || p == input+1){
        return 0;//parse hex escape will print an error message
      }
      *output=uvalue;
      return p-input;
    }
    case 'U':
      udigits=8;
    case 'u':
      if(!udigits){
        udigits=4;
      }
      uvalue=parse_unicode_escape(p);
      if (uvalue == (uint32_t)-1 || p == input + 1){
        return 0;//parse unicode will print an error message
      }
      uint64_t utf8_val;
      if(utf8_encode_char((uint8_t*)&utf8_val,uvalue)==(size_t)-1){
        return 0;
      }
      *(uint64_t**)output=utf8_val;
      return p-input;
    default:
      return 0;
  }
}
char parse_hex_escape(char *input){//sets input to the first character after the escape
  if(!(isxdigit(*input))){
    fprintf(stderr,"error lexing char, expected hex digit after \\x\n");
    return (char)-1;//incase char is unsigned
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
    fprintf(stderr,"error lexing char, expected hex digit after \\u or \\U\n");
    return (wchar_t)-1;
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

