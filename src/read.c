#define CORD_BUFSZ 256
#include "common.h"
#include "cons.h"
#include "read.h"//function prototypes of external functions
#include "extra/read_tables.h"//various tables used in the reader
#include "read_aux.h"//macros used internally in the reader
#include "read_input.h"//the mess that is how I do input(it's not that bad)
#include "lisp_utf8.h"

//Need to add eof testing in here somewhere

static int null_ch;//global write only variable

//this is because the actual function used has an awful name
#define string_scan_chars(string,chars)         \
  (strpbrk(string,chars))
/*different possible streams to read from:
  CORDs, using CORD_pos
    also used for reading from functions via CORD_from_fn
  FILES, using standard getc/ungetc
  character arrays, using some struct for internal state
 */
//char c=read_char(intput);if(c<0)... would also work for eof
read_input *make_cord_input(CORD input){
  CORD_pos_ptr p=xmalloc(sizeof(struct CORD_Pos));
  CORD_set_pos(p,input,0);
  read_input *retval=xmalloc(sizeof(read_input));
  *retval=(read_input){.input=p,.input_type=cord_read_input};
  return retval;
}
read_input *make_stream_input(FILE *input){
  read_input *retval=xmalloc(sizeof(read_input));
  read_stream *stream=xmalloc(sizeof(read_stream));
  stream->stream=input;
  *retval=(read_input){.input=stream,.input_type=stream_read_input};
  return retval;
}
read_input *make_string_input(const char *string){
  uint32_t len=strlen(string);
  read_input *retval=xmalloc(sizeof(read_input));
  array_stream *arr=xmalloc(sizeof(array_stream));
  *arr=(array_stream){.arr=string,.len=len,.index=0};
  *retval=(read_input){.input=arr,.input_type=string_read_input};
}
read_input *make_string_input_len(const char *string,uint32_t len){
  read_input *retval=xmalloc(sizeof(read_input));
  array_stream *arr=xmalloc(sizeof(array_stream));
  *arr=(array_stream){.arr=string,.len=len,.index=0};
  *retval=(read_input){.input=arr,.input_type=string_read_input};
}
sexp start_read(read_input *input){
  return read_0(input,0);
}
sexp read_from_cord(CORD input){
  return start_read(make_cord_input(input));
}
sexp read_from_stream(FILE *input){
  return start_read(make_stream_input(input));
}
sexp read_from_string(char *input){
  return start_read(make_string_input(input));
}
//used internally to read files for compilation or evaluation
sexp read_file(FILE *input){
  int fd=fileno(input);
  off_t file_len=lseek(fd,0,SEEK_END);
  char *buf=xmalloc_atomic(file_len);
  pread(fd,buf,file_len,0);
  close(fd);
  read_input *rd_input=make_string_input_len(buf,file_len);
  array_stream *arr=(array_stream*)rd_input->input;
  sexp program=cons_sexp(xmalloc(sizeof(cons)));
  sexp retval=program;
  while(1){
    XCAR(program)=read_0(rd_input,0);
    if(arr->arr[arr->index] == EOF){
      break;
    }
    XCDR(program)=cons_sexp(xmalloc(sizeof(cons)));
    program=XCDR(program);
  }
  return retval;
}
sexp read_0(read_input *input,int flags){
  int ch;
  sexp val=internal_read(input,&ch,flags);
  if(ch){
    raise_simple_error_fmt(Eread,"invalid read syntax '%c'",ch);
  }
  return val;
}
#if (defined DEBUG) && !(defined NDEBUG)
#define READ_MESSAGE(msg) fprintf(stderr,msg "\n")
#else
#define READ_MESSAGE
#endif
//Also should I use a dispatch table rather than switches?
sexp internal_read(read_input *input,int *pch,int flags){
  //flags for now are only for backticks
  char c;
  *pch=0;
  while((c=(read_char(input)))!=EOF){
    switch(c){
      case ' ':
      case '\n':
      case '\t':
      case '\v':
        PRINT_FMT("Read a whitespace character, code %hhx",c);
        continue;
      case ';':READ_MESSAGE("reading single line comment");
        skip_line(input);
        continue;
      case '(':READ_MESSAGE("reading list");
        return read_list(input,flags);
      case '[':READ_MESSAGE("reading vector");
        return read_array(input);
      case ')':
      case '}':
      case ']':
      case '.':
        PRINT_FMT("Read a list/vector terminating char, %c",c);
        *pch=c;
        return NIL;
      case '"':READ_MESSAGE("reading double quoted string");
        return read_double_quoted_string(input);
      case '?':READ_MESSAGE("reading char literal");
        return read_char_literal(input);
      case '\'':{READ_MESSAGE("reading quoted sexp");
        return c_list2(Qquote_sexp,read_0(input,flags));
      }
      case '|':READ_MESSAGE("reading pipe");
        return read_symbol_verbatim(input);
      case '#':READ_MESSAGE("reading sharp");
        return read_sharp(input);
      case '`':{READ_MESSAGE("reading backquoted sexp");
        return c_list2(Qbackquote_sexp,read_0(input,flags+1));
      }
      case ',':READ_MESSAGE("reading comma");
        if(!flags){
          raise_simple_error(Eread,"Error comma not inside a backquote");
        }
        return c_list2(Qcomma_sexp,read_0(input,flags-1));
      default:READ_MESSAGE("reading symbol or number");
        return read_symbol_or_number(input);
    }
  }
  raise_simple_error(Eread,"Error, encountered EOF while reading");
}
static sexp read_bigint(read_input *input,int radix){
  int64_t num;
  char *endptr;
  char *accept;
  errno=0;
  if(!radix){
    uint8_t c=read_char(input);
    if(c=='0'){
      if(peek_char(input)=='x'){
        radix=16;
        accept="01234556789abcdefABCDEF";
        if(!strchr(accept,c)){
          raise_simple_error(Eread,"expected a hexidecimal digit following #Z0x");
        }
        read_char(input);

      } else {
        radix=10;
        accept="0123456789";
        if(!strchr(accept,c)){
          raise_simple_error(Eread,"expected a digit following #Z");
        }
        unread_char(input);        
      }
    }
  }
  char *str=read_span(input,accept);  
  //two reasons to do this, first if the number will fit
  //into a normal int64 we can initialize from that rather
  //than a string, and even if not this will still tell us
  //how long the number is
  num=strtol(str,&endptr,radix);
  if(errno){
    if(errno==ERANGE){
      mpz_t *bignum=xmalloc_atomic(sizeof(mpz_t));
      //mpz expects a null terminated string, so to avoid coping the entire
      //number (which has to be pretty long) we store the first invalid
      //character and then set that location to null, and then
      //restore it afterword
      char end=*endptr;
      *endptr='\0';
      mpz_init_set_str(*bignum,str,radix);
      *endptr=end;
      return bigint_sexp(bignum);
    }
    //invalid sequence
  } else {
    mpz_t *bignum=xmalloc_atomic(sizeof(mpz_t));
    mpz_init_set_si(*bignum,num);
    return bigint_sexp(bignum);
  }
}
//abstracted in case for some reason I don't want to use strtol
sexp read_integer(read_input *input,int radix){
  int64_t num;
  char *endptr;
  char *accept="0123456789";
  //temporary
  assert(radix == 0 || radix==10);
  char *str=read_span(input,accept);
  errno=0;
  num=strtol(str,&endptr,radix);
  if(errno){
    current_env->error_num=errno;
    raise_simple_error(Esystem,"Error in strtol");
  }
  return int64_sexp(num);
}

static sexp read_sharp(read_input *input){
  char c;
  switch((c=read_char(input))){
    case '|':{//nested comment, comments delimited by #| and |#
      int comment_depth=1;
      char *pipe_ptr;
      while(comment_depth){
        while((c=read_char(input)) != '#' && c != '|');
        //need to do something about eof
        //should I unread a character and go back to the loop above
        //or loop here if the next char doesn't make a comment delimiter?
        if(c=='#'){
          if((c=read_char(input))=='|'){
            comment_depth++;
          } else {
            unread_char(input);
          }
        } else if(c=='|'){
          if((c==read_char(input)=='#')){
            comment_depth--;
          } else {
            unread_char(input);
          }
        }
      }
    }
    case 's'://read a hash table where input is of the form:
      //(hash-table [prop val]* ([key val]*)) (don't actually put brackets)
      return NIL;//for now
      //      return read_hash_table(input);
    case 'x'://read a hexadecimal integer
    case 'X':
      return read_integer(input,16);
    case 'Z'://read an arbitary precision integer
    case 'z'://either in base ten or with leading 0x in base 16
      return read_bigint(input,0);
      /* need to write this first
    case 'R'://read an arbitary precison floating point number
    case 'r'://would use #f but thats used for false
    return read_bigfloat(input,0);*/
    case 't':
      return LISP_TRUE;
    case 'f':
      return LISP_FALSE;
    case '<':{//signal a read error
      //try to read whot exactly is the invalid object, for a better message
      char *invalid=read_delim(input,'>');
      if(!invalid){
        raise_simple_error(Eread,"Error unterminated invalid read");
      } else {
        lisp_string *invalid_obj=xmalloc(sizeof(lisp_string));
        invalid_obj->cord=CORD_cat("#<",invalid);
        invalid_obj->len=CORD_len(invalid)+2;
        raise_sexp_error(Eread,string_sexp(invalid_obj));
      }

    }
    case ':':{//uninterned symbol
      int len,mb;
      uint8_t c;
      CORD_ec buf;
      CORD_ec_init(buf);
      read_symbol_string(input);
      const char *name=CORD_ec_to_char_star(buf);
      symbol *new_sym=make_symbol_from_name(make_symbol_name_no_copy(name,len,0));
      new_sym->name->multibyte=mb;
      return symref_sexp(new_sym);
    }


    case 'b':
    case 'B':{
      return uint64_sexp
        (read_pow_of_two_base_number
         (input,binary_char_test,binary_char_extract,1,
          "Error expected a binary literal following a #b or #B"));
      uint64_t result=0;
      uint8_t c='\0';
      //check first char to insure there's actualy a binary number
      c=read_char(input);
      if(c-0x30 > 1){
        raise_simple_error
          (Eread,"Error expected a binary literal following a #b or #B");
      }
      //unsigned int can't be less that 0, so anything
      //other that a '0' or '1' will fail this check
      do {
        result=(result<<1)+(c-0x30);
      } while(((c=read_char(input))-0x30)<=1);
      return uint64_sexp(result);
    }
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      //in common lisp numbers can be used in two different ways
      //either for a number of some radix between 2 and 36
      //using syntax #<num><num>?R<number>
      //or for reading an n-dimesional array using syntax:
      //#<num><num>?<num>?A[array...]
      //I'm not sure what I'll allow, but for now nothing
    default:
      raise_simple_error_fmt(Eread,"Invalid read syntax #'%c'",c);
  }
}
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
      if(((uint8_t)escape_char)>0x80){
        raise_simple_error(Eread,"Illegial unicode character following a \\");
      }
      return escape_char;
  }
}
//rewrite to use read_char
static int parse_escape_internal(read_input *input,char** output){
  //input is the text immediately following a backslash
  uint8_t c=read_char(input);
  **output=parse_simple_escape(c);
  if(**output != ((char)-1)){
    return 1;
  }
  int temp=0;
  uint32_t uvalue;
  int udigits=0;
  switch((c=read_char(input))){
    case 'x':{
      //note to self: --  (prefix or postfix) has higer precidence than &&
      uvalue=parse_hex_escape(input);
      //parse_escape raises an error on a malformed escape sequence
      **output=uvalue;
      return 1;
    }
    case 'U':
      udigits=8;
    case 'u':
      if(!udigits){
        udigits=4;
      }
      uvalue=parse_unicode_escape(input,udigits);
      //parse_escape raises an error on a malformed escape sequence
      uint64_t utf8_val;
      if(utf8_encode_char((uint8_t*)&utf8_val,uvalue)==(size_t)-1){
        raise_simple_error(Eread,"Invald unicode seqence");
      }
      **(uint64_t**)output=utf8_val;
      return utf8_mb_char_len(((uint8_t*)&utf8_val)[0]);
    default:
      assert(0);
  }
}
static char parse_hex_escape(read_input *input){//sets input to the first character after the escape
  char c=read_char(input);
  if(!(isxdigit(c))){
    raise_simple_error(Eread,"error lexing char, expected hex digit after \\x\n");
  }
  if(isxdigit(peek_char(input))){
    return((16*HEXVALUE(c))+HEXVALUE(read_char(input)));
  } else {
    return HEXVALUE(c);
  }
}
static uint32_t parse_unicode_escape(read_input *input,int udigits){
  uint32_t uvalue;
  uint8_t c=read_char(input);
  if(!isxdigit(c)){
    raise_simple_error
      (Eread,"error lexing char, expected hex digit after \\u or \\U\n");
  }
  udigits--;
  do {
    uvalue <<= 4;
    uvalue += HEXVALUE(c);
  } while((--udigits) && (c=read_char(input)) && isxdigit(c));
  if(udigits){//read less than max chars, so we read an extra char
    unread_char(input);
  }
  return uvalue;
}
static sexp read_char_literal(read_input *input){
  uint8_t c=read_char(input);
  char retval[8]={0};
  if(c=='\\'){
    parse_escape_internal(input,(char**)&retval);
    return mb_char_sexp(*(uint64_t*)retval);
  } else if (c<0x80){
    return mb_char_sexp(c);
  } else if (c<0xFE){
    int mb_len=utf8_mb_char_len(c);
    retval[0]=c;
    int i=1;
    while(i<mb_len){
      c=read_char(input);
      if(UTF8_table[c] != utf8_cont){
        raise_simple_error(Eread,"Invalid unicode sequence");
      }
    }
    return mb_char_sexp(*(uint64_t*)retval);
  } else {
    raise_simple_error(Eread,"Invalid unicode sequence");
  }
}



static sexp read_symbol_verbatim(read_input *input){
  int i=0;
  int mb=0;
  char c;
  CORD_ec buf;
  CORD_ec_init(buf);
  while(1){
    c=read_char(input);
    if(c=='\\'){
      CORD_ec_append(buf,read_char(input));
    } else if (c=='|'){
      break;
    } else {
      CORD_ec_append(buf,READ_CHAR_MULTIBYTE(input,&mb));
    }
  }
}
static inline sexp symbol_from_ec_cord(CORD_ec buf){
  uint32_t len;
  if(!buf[0].ec_cord){
    len=buf[0].ec_buf-buf[0].ec_bufptr;
    return symref_sexp(c_intern(buf[0].ec_buf,len,current_obarray));
  } else {
    CORD name=CORD_ec_to_cord(buf);
    len=CORD_len(name);
    return symref_sexp(c_intern_no_copy(CORD_to_const_char_star(name),len,current_obarray));
  }
  //  sym_name->len=len;
  //  sym_name->multibyte=mb;
}
static sexp read_double_quoted_string(read_input *input){
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
      nbytes=parse_escape_internal(input,&escape);
      len+=nbytes;
      if(nbytes>1){
        mb=1;
        while(nbytes>0){
          CORD_ec_append(buf,*escape++);
          nbytes--;
        }
      } else {
        CORD_ec_append(buf,*escape);
      }
      continue;
    } else if(c=='"'){
      break;
    } else {
      //I don't check for valid unicode input here, should I?
      CORD_ec_append(buf,c);
      len++;
    }
  }
  lisp_string *retval;
  if(buf[0].ec_cord){
    retval=xmalloc(sizeof(lisp_string));
    retval->cord=CORD_ec_to_cord(buf);
  } else {
    retval=xmalloc_atomic(sizeof(lisp_string)+len);
    retval->string=(char*)(((char*)retval)+sizeof(lisp_string));
    memcpy((char*)retval->string,buf[0].ec_buf,len);
  }
  retval->len=len;
  retval->multibyte=mb;
  return string_sexp(retval);
}

//maybe one day I'll optimize this to not just use
//strtol/strtod but not today
/*
  try to interpret str as a number, first as an integer
  then as a floating point number, if str is not a valid
  representation of a number return NIL.
 */
sexp string_to_number(char *str){
  char *endptr;
  errno=0;
  //maybe eventually be like common lisp and havo a variable read-base
  int64_t result=strtol(str,&endptr,10);
  //if the whole string str is valid (and not null) then endptr is set to '\0'
  //by virtue of c strings being null terminated
  if(!(*endptr)){
    //if(errno){read str as a bigint
    return int64_sexp(result);
  }
  //to have a valid double the next character must be . e or E
  //as a floating point number is [-+]?[0-9]+(.[0-9]+)?([eE][0-9]+)?
  if(*endptr == '.' || *endptr == 'e' || *endptr == 'E'){
    errno=0;
    real64_t result=strtod(str,&endptr);
    if(!(*endptr)){
      return real64_sexp(result);
    }
  }
  return NIL;
}


//Should the multibyte field of symbol_name be determined by a parameter to
//the different symbol/symbol name creating functions or by explicitly
//setting it if necessary?
static sexp read_symbol_or_number(read_input *input){
  CORD_ec buf;
  CORD_ec_init(buf);
  uint8_t c;
  int len=0,mb=0;
  read_symbol_string(input);
  HERE();
  //maybe if(c)==':' goto qualified symbol
  unread_char(input);
  char *str=CORD_ec_to_char_star(buf);
  HERE();
  if(!mb && strchr("0123456789+-",str[0])){
    sexp maybe_num=string_to_number(str);
    if(!NILP(maybe_num)){READ_MESSAGE("read number");
      return maybe_num;
    }
  }
  HERE();
  if(read_char(input)==':'){
    //either the curent symbol is explicitly typed
    //or the current symbol is a package and we need to read another symbol
    char c=read_char(input);
    int require_exported=1;
    if(c==':'){
      //code to set type field of symbol
      //should I call read recursively or just read another string
      //and see if it's a type?
    } else if (c=='.'){
      require_exported=0;
    } else {
      unread_char(input);
    }
    //this should probably be moved to the evaluator
    //becasue <non-extant-package>::<symbol-name> is perfectly
    //valid read syntax, So I need to figure out how
    //to store qualified symbols then move this
    symbol *package_sym=c_intern(str,len,current_obarray);
    if(mb){package_sym->name->multibyte=1;}
    if(!PACKAGEP(package_sym->val)){
      raise_simple_error_fmt(Eundefined,"Error Package %r does not exist",str);
    }
    obarray *package_symbol_table=package_sym->val.val.package->symbol_table;
    CORD_ec_init(buf);
    len=mb=0;
    read_symbol_string(input);
    char *sym_name=CORD_ec_to_char_star(buf);
    symbol *qualified_sym=
      obarray_lookup_sym(make_symbol_name_no_copy(sym_name,len,0),
                         package_symbol_table);
    if(qualified_sym){
      if(mb){qualified_sym->name->multibyte=1;}
      if(qualified_sym->visibility == symbol_externally_visable ||
         !require_exported){
        return symref_sexp(qualified_sym);
      } else {
      raise_simple_error_fmt(Evisibility,"Symbol %s not externally"
                             " visable in package %s",sym_name,str);
      }
    } else {
      raise_simple_error_fmt(Eundefined,"Symbol %s not found in package %s",
                             sym_name,str);
    }
  } else {
    unread_char(input);
    symbol *retval=c_intern(str,len,current_obarray);
    if(mb){retval->name->multibyte=1;}
    return symref_sexp(retval);
  }
}
//I need to make sure all internal keywords are actually stored with a colon
static sexp read_keyword_symbol(read_input *input){
  CORD_ec buf;
  CORD_ec_init(buf);
  int mb=0,len=0;
  uint8_t c;
  CORD_ec_append(buf,':');
  read_symbol_string(input);
  char *keysym_name=CORD_ec_to_char_star(buf);
  //all keywords are kept in the global obarary
  symbol *key_sym=c_intern(keysym_name,len,global_obarray);
  //keywords are just self referental symbols
  key_sym->val=symref_sexp(key_sym);
  return symref_sexp(key_sym);
}
static sexp read_list(read_input *input,int flags){
  //this implies () == (nil . nil)
  //l don't know if I want that or not
  sexp tail=cons_sexp(xmalloc(sizeof(cons)));
  sexp ls=tail;
  sexp val;
  int ch=0;
  while(1){
    val=internal_read(input,&ch,flags);
    if(ch){
      if(ch == ')'){
        return ls;
      }
      if(ch == '.'){
        ch=0;
        internal_read(input,&ch,flags);
        if(ch){
          raise_simple_error(Eread,"Error, nothing following '.' in list");
        } else {
          SET_CDR(tail,val);
          return ls;
        }
      } else {
        //this can probably be simplified as the only other possible
        //value of ch is ']' (or '}') and it'll probably stay that way
        raise_simple_error_fmt(Eread,"Invalalid character %c found in list",ch);
      }
    }
    //since gc sets memory to 0 and nil is defined as {0} the cdr
    //will always be nil (unless explicitly set)
    XCDR(ls)=cons_sexp(xmalloc(sizeof(cons)));
    ls=XCDR(ls);
    SET_CAR(ls,val);
  }
}
//pretty much an arbitary number
#define min_stack_size 32
#define push_temp(val)                          \
  (stack_index>=stack_size?0:((temp_stack[stack_index++]=val),1))


static sexp read_array(read_input *input){
  lisp_array *arr=xmalloc(sizeof(lisp_array));
  arr->dims=1;//for now only alow literal vectors, not literal arrays
  //you can have vectors of arrays, but they won't be seen an multidimesional
  int ch=0,arr_type=0;
  sexp *old_data_ptr=current_env->data_ptr;
  register sexp temp;
  temp=internal_read(input,&ch,0);
  if(ch){
    raise_simple_error_fmt(Eread,"invalid read syntax '[''%c'",ch);
  }
  uint32_t stack_size,stack_index=0;
  //we never actually adjust the stack pointer
  sexp *temp_stack;
  if(current_env->data_top-current_env->data_ptr<min_stack_size){
    stack_size=min_stack_size;
    temp_stack = xmalloc(sizeof(sexp)*min_stack_size);
  } else {
    stack_size = current_env->data_top-current_env->data_ptr;
    temp_stack = current_env->data_ptr;
  }
  arr_type=temp.tag;
  //obviously this can't fail
  push_temp(temp);
  if(arr_type>sexp_num_tag_max){
    //if it's a typed array but not an array of atoms
    //we can't use xmalloc_atomic, so just treat it the
    //same as an untyped array (for now I might add a
    //third loop for just this kind of array later)
    goto UNTYPED;
  }
  while(1){
    temp=internal_read(input,&ch,0);
    if(temp.tag != arr_type){
      goto UNTYPED;
    }
    if(ch){
      if(ch==']'){
        if(!arr->vector){//haven't allocated any memory yet
          arr->typed_vector=xmalloc_atomic(stack_index);
          arr->len=stack_index;
          arr->type=arr_type;
          //having written the thing I see no reason that this shouldn't work
          arr->typed_vector=memcpy_stride(arr->typed_vector,
                                          temp_stack,stack_index,2);
        } else {
          //this will trash the current array and overwrite it with
          //just the data from it, it should work(I did write memcpy_stride)
          arr->typed_vector=memcpy_stride(arr->typed_vector,
                                        arr->vector,arr->len,2);
          memcpy_stride(arr->typed_vector+arr->len,
                        temp_stack,stack_index,2);
          arr->len+=stack_index;
          arr->type=arr_type;
        }
        return array_sexp(arr);
      } else {
        raise_simple_error_fmt(Eread,"invalid character '%c' in vector",ch);
      }
    }
    if(!push_temp(temp)){
      //don't use realloc, as it'll almost certainly just
      //be doing malloc followed my memcpy
      sexp *mem=xmalloc_atomic(sizeof(sexp)*arr->len+stack_size);
      memcpy(mem,arr->vector,arr->len);
      //xfree(arr->vector)//maybe?, though it is atomic, so probably not
      arr->vector=mem;
      memcpy(arr->vector+arr->len,temp_stack,stack_size);
      arr->len+=stack_size;
      stack_index=0;
      push_temp(temp);
    }
  }
 UNTYPED:
  //we should only jump here after reading a value that is of a different type
  arr->type=sexp_sexp;
  if(arr->vector){
    //we might as well copy the values on the stack since we're already
    //reallocating stuff
    sexp *mem=xmalloc(sizeof(sexp)*arr->len+stack_index);
    memcpy(mem,arr->vector,arr->len);
    arr->vector=mem;
    memcpy(arr->vector+arr->len,temp_stack,stack_index);
    stack_index=0;
    push_temp(temp);
  }
  while(1){
    temp=internal_read(input,&ch,0);
    if(ch){
      if(ch==']'){
        //just acts as xmalloc if arr->vector=NULL
        arr->vector=xrealloc(arr->vector,arr->len+stack_index);
        memcpy(arr->vector+arr->len,temp_stack,stack_index);
        arr->len+=stack_index;
        return(array_sexp(arr));
      } else {
        raise_simple_error_fmt(Eread,"invalid character '%c' in vector",ch);
      }
    }
    if(!push_temp(temp)){
      //don't use realloc, as it'll almost certainly just
      //be doing malloc followed my memcpy
      sexp *mem=xmalloc_atomic(sizeof(sexp)*arr->len+stack_size);
      memcpy(mem,arr->vector,arr->len);
      //xfree(arr->vector)//maybe?, though it is atomic, so probably not
      arr->vector=mem;
      memcpy(arr->vector+arr->len,temp_stack,stack_size);
      arr->len+=stack_size;
      stack_index=0;
      push_temp(temp);
    }
  }
}
