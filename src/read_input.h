typedef struct array_stream array_stream;
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
static char CORD_read_char(CORD_pos pos){
  char c=CORD_pos_fetch(pos);
  CORD_next(pos);
  return c;
}
static char CORD_peek_char(CORD_pos pos){
  return CORD_pos_fetch(pos);
}
static void CORD_unread_char(CORD_pos pos){
  CORD_prev(pos);
}
#define CORD_get_str(pos,n)                     \

static char *CORD_read_str(CORD_pos pos,int n){
  char *str=xmalloc_atomic(n);
  int i=0;
  while(i<n && CORD_pos_valid(pos)){
    str[i++]=CORD_pos_fetch(pos);
    CORD_next(pos);
  }
  return str;
}
static void CORD_skip_line(CORD_pos pos){
  while(CORD_pos_valid(pos)){
    if(CORD_pos_fetch(pos) == '\n'){
      break;
    }
    CORD_next(pos);
  }
  return;
}
//takes some advantage of cords, I think
static char *CORD_read_delim(CORD_pos pos,char delim){
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
static char *CORD_read_span(CORD_pos pos,char *accept){
  CORD str=CORD_pos_to_cord(pos);
  size_t len=CORD_pos_span(pos,accept);
  str=CORD_substr(str,0,len);
  return CORD_to_const_char_star(str);
}
static char arr_read_char(array_stream *input){
  return char_arr->arr[char_arr->index++];
}
static char arr_peek_char(array_stream *input){
  return char_arr->arr[char_arr->index];
}
static void arr_unread_char(array_stream *input){
  char_arr->index--;
}
static char* arr_read_str(array_stream *input,int n){
  char *str=xmalloc_atomic(n);
  memcpy(str,char_arr->arr,MAX(n,char_arr->len));
  return str;
}
//assume null terminated array
static void arr_skip_line(array_stream *input){
  char *endptr=strchr(char_arr->arr,'\n');
  char_arr->index=(char_arr->arr-endptr)+1;
  char_arr->arr=endptr+1;
}
#define arr_get_line(char_arr)                       \

static char *arr_read_delim(array_stream *input,char delim){
  char *endptr=strchr(char_arr->arr,delim);
  char *str=xmalloc_atomic(endptr-char_arr->arr+1);
  memcpy(str,char_arr->arr,endptr-char_arr->arr);
  str[endptr-char_arr->arr]='\0';
  char_arr->index=(endptr-char_arr->arr)+1;
  char_arr->arr=endptr+1;
  return str;
}
static char *arr_read_span(array_stream *input,char *accept){
  size_t len=strspn(input->arr,accept);
  char *retval=xmalloc_atomic(len);
  memcpy(retval,input->arr+input->index,len);
  intput->index+=len;
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
    c=str[i++]=getc(stream);
    if(c==delim || c=='\0' || c == EOFk){
      break;
    }
  }
  if(i>=size){
    str=xrealloc(str,(size*=2));
    goto LOOP;
  }
  return str;
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
static char*(*read_delim_funs[3])(void*)  = {(char*(*)(void*,char))arr_read_line,
                                             (char*(*)(void*,char))CORD_read_line,
                                            (char*(*)(void*.char))stream_read_line};
enum read_input_types {
  string_read_input=0,
  cord_read_input=1,
  stream_read_input=2,
};
static inline char read_char(read_input *input){
  read_char_funs[input->input_type](input->input);
}
static inline void unread_char(read_input *input){
  unread_char_funs[input->input_type](input->input);
}
static inline char peek_char(read_input *input){
  peek_char_funs[input->input_type](input->input);
}
static inline void skip_line(read_input *input){
  skip_line_funs[input->input_type](input->input);
}
static inline char *read_str(read_input *input,int n){
  read_str_funs[input->input_type](input->input,n);
}
static inline char *read_delim(read_input *input,char delim){
  read_delim_funs[input->input_type](input->input);
}
static inline char *read_line(read_input *input){
  return read_delim(input,'\n');
}
