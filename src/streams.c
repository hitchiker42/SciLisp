#include "common.h"
#include "fcntl.h"
/* Different types of streams, standard libc streams should probably only be
   used for special files (e.g stdin)
 */
typedef struct string_stream string_stream;
typedef struct CORD_stream CORD_steam;
typedef struct libc_stream libc_stream;
typedef struct file_stream file_stream;
#define STREAM_READABLE 0x1
#define STREAM_WRITABLE 0x2
#define STREAM_READWRITE 0x3
#define STREAM_APPEND 0x4
//maybe these
//#define STREAM_ASYNC 0x8
//#define STREAM_NONBLOCK 0x10
union lisp_stream {
  string_stream *string;
  cord_stream *cord;
  libc_stream *libc;
  file_stream *file;
};
/* String streams should be fast for reading and writing, but
   might be slow for appending, There should always be some
   internal buffer in the stream so appending anything less than
   the buffer size is quick, but when it's full the whole
   stream will need to be reallocated
 */
struct string_stream {
  const char *str;
  uint32_t index;
  uint32_t len;
  uint32_t mem_allocated;//should always be >= to len
#ifdef MULTI_THREADED
  pthreads_mutex_t *lock;
#endif
  uint8_t mode;//read/write/append
};
/* For cord streams reading should be reasonably fast
   and appending should be faster than pretty must any other
   typed of stream. but standard writing (ie writing when
   the current position isn't the end of the file) might
   be rather slow
 */

struct CORD_stream {
  CORD cord;
  CORD_pos pos;
#ifdef MULTI_THREADED
  pthreads_mutex_t lock[1];
#endif
  uint8_t mode;
};
/* It's probably best to use these only for compatably with c as
   the buffering of these is optimized for use in c.
   cord_streams will be better at appending and string
   streams will be better at writng. but these should at least
   have a better balance between both opperations.
 */
struct libc_stream {
  FILE *stream;
  uint8_t mode;
  uint8_t seekable;//set to 1 if stream is seekable (ie not a special file)
#ifdef MULTI_THREADED
  pthreads_mutex_t lock[1];
#endif
};
/* Most flexable stream, but also the least user friendly, it's pretty
   must a direct interfate to the actual file descriptor, so any buffering
   needs to be done manually
 */
struct file_stream {
  int fd;
  uint8_t mode;
  uint8_t seekable;
#ifdef MULTI_THREADED
  pthreads_mutex_t lock[1];
#endif
};
static uint8_t get_fd_mode(int fd){
  int fd_mode=fcntl(fd,F_GETFL);
  uint8_t retval=0;
  if((fd_mode&O_ACCMODE)==O_RDONLY){
    retval=STREAM_READABLE;
  } else if((fd_mode&O_ACCMODE)==O_WRONLY){
    retval=STREAM_WRITEABLE;
  } else if((fd_mode&O_ACCMODE)==O_RDWR){
    retval=STREAM_READWRITE;
  }
  if((fd_mode&O_APPEND)){
    retval|=STREAM_APPEND;
  }
  return retval;
}
file_stream *init_file_stream(int fd){
  file_stream *retval=xmalloc_atomic(sizeof(file_stream));
  if(lseek(fd,0,SEEK_CUR)==-1){
    if(errno==ESPIPE){
      retval->seekable=0;
    } else {
      raise_simple_error(Esystem,lisp_strerror(errno));
    }
  } else {
    retval->seekable=1;
  }
  retval->mode=get_fd_mode(fd);
#ifdef MULTI_THREADED
  pthread_mutex_init(retval->lock);
#endif
  return retval;
}
libc_stream *init_libc_stream(FILE *stream){
  //should use a typed GC_malloc, since only the first field
  //is a pointer
  libc_stream *retval=xmalloc(sizeof(libc_stream));
  if(ftello(stream)==(off_t)-1){
    if(errno==EBADF){
      retval->seekable=0;
    } else {
      raise_simple_error(Esystem,lisp_strerror(errno));
    }
  } else {
    retval->seekable=1;
  }
  retval->mode=get_fd
#ifdef MULTI_THREADED
  pthread_mutex_init(retval->lock);
#endif
  return retval;
}
CORD_stream *init_CORD_stream(CORD cord,uint8_t mode){
  //maybe allocate using typed malloc (does a CORD_pos have pointers?)
  CORD_stream *retval=xmalloc(sizeof(cord_stream));
  retval->cord=cord;
  CORD_set_pos(retval->pos,retval->cord,0);
  retval->mode=mode;
#ifdef MULTI_THREADED
  pthread_mutex_init(retval->lock);
#endif
  return retval;
}
string_stream *init_string_stream(char *string,uint8_t mode,uint32_t len){
  string_stream *retval=xmalloc(sizeof(string_stream));
  if(!string){
    retval->str=xmalloc_atomic(16);
    retval->len=0;
    retval->mem_allocated=16;
  } else {
    if(!len){
      retval->len=strlen(string);
    } else {
      retval->len=len;
    }
    retval->str=string;
    retval->mem_allocated=GC_size(string);
  }
  retval->mode=mode;
#ifdef MULTI_THREADED
  pthread_mutex_init(retval->lock);
#endif
  return retval;
}
//macros for treating a CORD_pos as a stream
//I'm not sure what happend if you fetch from an invalid
//CORD_pos, if it returns eof then I don't need to
//do anything special, if not I need to check if it's valid
//then if not return eof
static char CORD_stream_read_char(CORD_stream *cord){
  char c=CORD_pos_fetch(cord->pos);
  CORD_next(pos);
  return c;
}
static char CORD_stream_peek_char(CORD_stream *cord){
  return CORD_pos_fetch(cord->pos);
}
static void CORD_stream_unread_char(CORD_stream *cord){
  CORD_prev(cord->pos);
}
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
static char *stream_read_span(read_stream *stream,char *accept){
  uint8_t flags[256]={0};
  while(*accept){
    flags[*accept++]=1;
  }
  CORD_ec buf;
  CORD_ec_init(buf);
  char c;
  while(accept[(c==getc(stream))]){
    CORD_ec_append(buf,c);
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
static inline char *read_span(read_input *input,char *accept){
  return read_span_funs[input->input_type](input->input,accept);
}
/* Different types of streams, standard libc streams should probably only be
   used for special files (e.g stdin)
 */
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
static char *stream_read_span(read_stream *stream,char *accept){
  uint8_t flags[256]={0};
  while(*accept){
    flags[*accept++]=1;
  }
  CORD_ec buf;
  CORD_ec_init(buf);
  char c;
  while(accept[(c==getc(stream))]){
    CORD_ec_append(buf,c);
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
static inline char *read_span(read_input *input,char *accept){
  return read_span_funs[input->input_type](input->input,accept);
}
