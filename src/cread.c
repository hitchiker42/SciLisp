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
static int null_ch;//global write only variable
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
sexp internal_read(char *input,int *pch,int flags);
sexp read_0(char *input,int flags){
  int ch;
  sexp val=internal_read(input,&ch,flags);
  if(ch){
    raise_simple_error_fmt(Eread,"invalid read syntax '%c'",ch);
  }
  return val;
}
//I don't know how I'm going to do input, but for now
//assume I have functions read_char and unread_char
//to get and put back characters, that should be generic
//enough for anything

//I need to write wappers/macros to call internal_read
//with only one argument (i.e like emacs has read1 and read0)

//Also I should use a dispatch table rather that switches
sexp internal_read(char *input,int *pch,int flags){
  //flags for now are only for backticks
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
      case '`':{
        return c_list2(Qbackquote,read_0(input,flags+1));
      }
      case ',':
        if(!flags){
          raise_simple_error(Eread,"Error comma not inside a backquote");
        }
        return c_list2(Qcomma,read_0(input,flags-1))        
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
//assumes an initialized CORD_ec buf, a uint8_t c, and ints len and mb
#define read_symbol_string(input)               \
  while((c==read_char(input))){                 \
  if(invalid_symbol_char[c]){                   \
    break;                                      \
  }                                             \
    if(c=='\\'){                                \
      CORD_ec_append(read_char(input));         \
    } else {                                    \
      CORD_ec_append(c);                        \
    }                                           \
    len++;                                      \
  }
sexp read_sharp(char *input){
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
      return read_hash_table(input);
    case 'x'://read a hexadecimal integer
    case 'X':
      return read_integer(input,16);
    case 'Z'://read an arbitary precision integer
    case 'z'://either in base ten or with leading 0x in base 16
      return read_bigint(input,0);
    case 'F'://read an arbitary precison floating point numbel
    case 'f':
      return read_bigfloat(input,0);
    case '<':{//signal a read error
      //try to read whot exactly is the invalid object, for a better message
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
    case ':':{//uninterned symbol
      int len,mb;
      uint8_t c;
      CORD_ec buf;
      CORD_ec_init(buf);
      read_symbol_string(input);
      const char *name=CORD_ec_to_char_star(buf);
      symbol *new_sym=make_symbol_from_name(make_symbol_name_no_copy(name,len));
      new_sym->name->multibyte=mb;
      return symref_sexp(new_sym);
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
  if(*endptr == '.' || endptr == 'e' || endptr == 'E'){
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
sexp read_symbol_or_number(char *input){
  CORD_ec buf;
  CORD_ec_init(buf);
  uint8_t c;
  int len=0,mb=0;
  read_symbol_string(input);
  //maybe if(c)==':' goto qualified symbol
  unread_char(input);
  char *str=CORD_ec_to_char_star(buf);
  if(!mb && strchr("0123456789+-",str[0])){
    sexp maybe_num=string_to_number(str);
    if(!NILP(maybe_num)){
      return maybe_num;
    }
  }
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
    symbol *package_sym=c_intern(str,len,current_obarary);
    if(mb){package_sym->name.multibyte=1;}
    if(!PACKAGEP(package_sym->val.tag)){
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
      if(mb){qualified_sym->name.multibyte=1;}
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
    symbol *retval=c_intern(str,len,mb,current_obarray);
    if(mb){retval->name.multibyte=1;}
  }
}
//I need to make sure all internal keywords are actually stored with a colon
sexp read_keyword_symbol(char *input){
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
sexp read_list(char *input,){
  //this implies () == (nil . nil)
  //l don't know if I want that or not
  sexp tail=cons_sexp(xmalloc(sizeof(cons)));
  sexp ls=tail;
  sexp val;
  int ch=0;
  while(1){
    val=read_internal(input,&ch,0);
    if(ch){
      if(ch == ')'){
        return ls;
      }
      if(ch == '.'){
        ch=0;
        val=read_internal(intput,&ch);
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
#define push_temp(val) (stack_index>=stack_size?NULL:temp_stack[stack_index++]=val)
sexp read_array(char *input){
  lisp_array *arr=xmalloc(sizeof(lisp_array));
  arr->dims=1;//for now only alow literal vectors, not literal arrays
  //you can have vectors of arrays, but they won't be seen an multidimesional
  int ch=0,arr_type=0;
  sexp *old_data_ptr=env->data_ptr;
  register sexp temp;
  temp=read_internal(input,&ch,0);
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
    temp=read_internal(input,&ch,0);
    if(temp->tag != arr_type){
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
        raise_simple_error_fmt("invalid character '%c' in vector",ch);
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
    temp=read_internal(input,&ch,0);
    if(ch){
      if(ch==']'){
        //just acts as xmalloc if arr->vector=NULL
        arr->vector=xrealloc(arr->vector,arr->len+stack_index);
        memcpy(arr->vector+arr->len,temp_stack,stack_index);
        arr->len+=stack_index;
        return(array_sexp(arr));
      } else {
        raise_simple_error_fmt("invalid character '%c' in vector",ch);
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
