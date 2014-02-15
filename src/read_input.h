/* Different types of streams, standard libc streams should probably only be
   used for special files (e.g stdin)
 */
typedef struct array_stream array_stream;
//a CORD_pos is a typedef for a struct CORD_Pos[1]
//and despite what some people think (and admittly what I used to think)
//arrays and pointers in c are not the same thing, not at all
//if I use a CORD_pos it won't record changes in state after calls to
//read_char,etc. So I need to use an actual pointer to the underlying struct
typedef struct CORD_Pos *CORD_pos_ptr;
struct array_stream {
  const char *arr;
  uint32_t index;
  uint32_t len;
};
//macros for treating a CORD_pos as a stream
//I'm not sure what happend if you fetch from an invalid
//CORD_pos, if it returns eof then I don't need to
//do anything special, if not I need to check if it's valid
//then if not return eof
static char CORD_read_char(CORD_pos_ptr pos){
  assert(CORD_pos_valid(pos));
  char c=CORD_pos_fetch(pos);
  CORD_next(pos);
  return c;
}
static char CORD_peek_char(CORD_pos_ptr pos){
  return CORD_pos_fetch(pos);
}
static void CORD_unread_char(CORD_pos_ptr pos){
  CORD_prev(pos);
}

static char *CORD_read_str(CORD_pos_ptr pos,int n){
  char *str=xmalloc_atomic(n);
  int i=0;
  while(i<n && CORD_pos_valid(pos)){
    str[i++]=CORD_pos_fetch(pos);
    CORD_next(pos);
  }
  return str;
}
static void CORD_skip_line(CORD_pos_ptr pos){
  while(CORD_pos_valid(pos)){
    if(CORD_pos_fetch(pos) == '\n'){
      break;
    }
    CORD_next(pos);
  }
  return;
}
//takes some advantage of cords, I think
static const char *CORD_read_delim(CORD_pos_ptr pos,char delim){
  CORD str=CORD_pos_to_cord(pos);
  int len;
  while(CORD_pos_valid(pos)){
    if(CORD_pos_fetch(pos) == delim){
      break;
    }
    CORD_next(pos);
    len++;
  }
  str=CORD_substr(str,0,len);
  return CORD_to_const_char_star(str);
}
static const char *CORD_read_span(CORD_pos_ptr pos,char *accept){
  CORD str=CORD_pos_to_cord(pos);
  size_t len=CORD_pos_span(pos,accept);
  str=CORD_substr(str,0,len);
  return CORD_to_const_char_star(str);
}
static char arr_read_char(array_stream *arr){
  return arr->arr[arr->index++];
}
static char arr_peek_char(array_stream *arr){
  return arr->arr[arr->index];
}
static void arr_unread_char(array_stream *arr){
  arr->index--;
}
static char* arr_read_str(array_stream *arr,int n){
  char *str=xmalloc_atomic(n);
  memcpy(str,arr->arr,MAX(n,arr->len));
  return str;
}
//assume null terminated array
static void arr_skip_line(array_stream *arr){
  char *endptr=strchr(arr->arr,'\n');
  arr->index=(arr->arr-endptr)+1;
  arr->arr=endptr+1;
}
static char *arr_read_delim(array_stream *arr,char delim){
  char *endptr=strchr(arr->arr,delim);
  char *str=xmalloc_atomic(endptr-arr->arr+1);
  memcpy(str,arr->arr,endptr-arr->arr);
  str[endptr-arr->arr]='\0';
  arr->index=(endptr-arr->arr)+1;
  arr->arr=endptr+1;
  return str;
}
static char *arr_read_span(array_stream *arr,char *accept){
  size_t len=strspn(arr->arr,accept);
  char *retval=xmalloc_atomic(len);
  memcpy(retval,arr->arr+arr->index,len);
  arr->index+=len;
  return retval;
}
typedef struct read_stream {
  FILE *stream;
  char c;
} read_stream;

static char stream_read_char(read_stream *stream){
  return stream->c=getc(stream->stream);
}

static char stream_peek_char(read_stream *stream){
  char c=getc(stream->stream);
  ungetc(c,stream->stream);
  return c;
}
static void stream_unread_char(read_stream *stream){
  ungetc(stream->c,stream->stream);
}

static char* stream_read_str(read_stream *stream,int n){
  char *str=xmalloc_atomic(n);
  fread(str,n,1,stream->stream);
  return str;
}
static void stream_skip_line(read_stream *stream){
  while(getc(stream->stream)!='\n');
}
//we can't use the libc getdelim because of gc
static char *stream_read_delim(read_stream *stream,char delim){
  uint32_t size=16;
  int i=0;
  char *str=xmalloc_atomic(16);
  char c;
 LOOP:for(;i<size;i++){
    c=str[i++]=getc(stream->stream);
    if(c==delim || c=='\0' || c == EOF){
      break;
    }
  }
  if(i>=size){
    str=xrealloc(str,(size*=2));
    goto LOOP;
  }
  return str;
}
static char *stream_read_span(read_stream *stream,char *accept){
  uint8_t flags[256]={0};
  while(*accept){
    flags[*accept++]=1;
  }
  CORD_ec buf;
  CORD_ec_init(buf);
  char c;
  while(accept[(c==getc(stream->stream))]){
    CORD_ec_append(buf,c);
  }
  return CORD_ec_to_char_star(buf);
}
  
static char(*read_char_funs[3])(void*) ={(char(*)(void*))arr_read_char,
                                         (char(*)(void*))CORD_read_char,
                                         (char(*)(void*))stream_read_char};
static char(*peek_char_funs[3])(void*) = {(char(*)(void*))arr_peek_char,
                                          (char(*)(void*))CORD_peek_char,
                                          (char(*)(void*))stream_peek_char};
static void(*unread_char_funs[3])(void*) = {(void(*)(void*))arr_unread_char,
                                            (void(*)(void*))CORD_unread_char,
                                            (void(*)(void*))stream_unread_char};
static void(*skip_line_funs[3])(void*)  = {(void(*)(void*))arr_skip_line,
                                           (void(*)(void*))CORD_skip_line,
                                           (void(*)(void*))stream_skip_line};
static char*(*read_str_funs[3])(void*,int)={(char*(*)(void*,int))arr_read_str,
                                            (char*(*)(void*,int))CORD_read_str,
                                            (char*(*)(void*,int))stream_read_str};
 static char*(*read_delim_funs[3])(void*,char)  = {(char*(*)(void*,char))arr_read_delim,
                                             (char*(*)(void*,char))CORD_read_delim,
                                                   (char*(*)(void*,char))stream_read_delim};
  static char*(*read_span_funs[3])(void*,char*)  = {(char*(*)(void*,char*))arr_read_span,
                                             (char*(*)(void*,char*))CORD_read_span,
                                                   (char*(*)(void*,char*))stream_read_span};
enum read_input_types {
  string_read_input=0,
  cord_read_input=1,
  stream_read_input=2,
};
 
static inline char read_char(read_input *input){
  return read_char_funs[input->input_type](input->input);
}
static inline void unread_char(read_input *input){
  unread_char_funs[input->input_type](input->input);
}
static inline char peek_char(read_input *input){
  return peek_char_funs[input->input_type](input->input);
}
static inline void skip_line(read_input *input){
  skip_line_funs[input->input_type](input->input);
}
static inline char *read_str(read_input *input,int n){
  return read_str_funs[input->input_type](input->input,n);
}
static inline char *read_delim(read_input *input,char delim){
  return read_delim_funs[input->input_type](input->input,delim);
}
static inline char *read_line(read_input *input){
  return read_delim(input,'\n');
}
static inline char *read_span(read_input *input,char *accept){
  return read_span_funs[input->input_type](input->input,accept);
}
