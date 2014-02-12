//macros for treating a CORD_pos as a stream
//I'm not sure what happend if you fetch from an invalid
//CORD_pos, if it returns eof then I don't need to
//do anything special, if not I need to check if it's valid
//then if not return eof
#define CORD_getc(pos)                          \
  ({char c=CORD_pos_fetch(pos);                 \
  CORD_next(pos);                               \
  c;})
static char CORD_read_char(CORD_pos pos){
  return CORD_getc(pos);
}
#define CORD_peek(pos) (CORD_pos_fetch(pos))
static char CORD_peek_char(CORD_pos pos){
  return CORD_peek(pos);
}
#define CORD_ungetc(pos) (CORD_prev(pos))
static CORD_unread_char(CORD_pos pos){
  CORD_ungetc(pos);
}
#define CORD_get_str(pos,n)                     \
  ({char *str=xmalloc_atomic(n);                \
    int i=0;                                    \
    while(i<n && CORD_pos_valid(pos)){          \
      str[i++]=CORD_fetch(pos);                 \
      CORD_next(pos);                           \
    }                                           \
    str;})
static char *CORD_read_str(CORD_pos pos){
  CORD_get_str(pos);
}
#define CORD_eat_line(pos)                      \
  ({while(CORD_pos_valid(pos)){                 \
      if(CORD_pos_fetch(pos) == '\n'){          \
        break;                                  \
      }                                         \
      CORD_next(pos);                           \
    }                                           \
    ;})
static void CORD_skip_line(CORD_pos pos){
  CORD_eat_line(pos);
}
//takes some advantage of cords, I think
#define CORD_get_line(pos)                      \
  ({CORD line=CORD_pos_to_cord(pos);            \
  int len;                                      \
  while(CORD_pos_valid(pos)){                   \
    if(CORD_pos_fetch(pos) == '\n'){            \
      break;                                    \
    }                                           \
    CORD_next(pos);                             \
    len++;                                      \
  }                                             \
  line=CORD_substr(line,0,len);                 \
  CORD_to_const_char_star(line);})
static char *CORD_read_line(CORD_pos pos){
  CORD_get_line(pos);
}
struct array_stream {
  const char *arr;
  uint32_t index;
  uint32_t len;
};
#include "cord/cord_macros.h"
#define arr_getc(arr) (arr.arr[arr.index++])
static char arr_read_char(array_stream *input){
  return arr_getc(*input);
}
#define arr_peek(arr) (arr.arr[arr.index])
static char arr_peek_char(array_stream *input){
  return arr_peek(*input);
}
#define arr_ungetc(arr) (arr.index--)
static void arr_unread_char(array_stream *input){
  return arr_ungetc(*input);
}
#define arr_get_str(arr,n)                      \
  ({char *str=xmalloc_atomic(n);                \
    memcpy(n,str,MAX(n,arr.len));               \
    str;})
static char* arr_read_str(array_stream *input){
  return arr_get_str(*input);
}
//assume null terminated array
#define arr_eat_line(arr)                           \
  ({char *endptr=strchr(arr.arr,'\n');              \
  arr.index=(arr.arr-endptr)+1;                     \
  arr.arr=endptr+1;                                 \
  ;})
static void arr_skip_line(array_stream *input){
  return arr_eat_line(*input);
}
#define arr_get_line(arr)                       \
  ({char *endptr=strchr(arr,'=n');              \
    char *str=xmalloc_atomic(endptr-arr.arr+1); \
    memcpy(str,arr,arr,entptr-arr.arr);         \
    str[endptr-arr.arr]='\0';                   \
    arr.index=(endptr-arr.arr)+1;               \
    arr.arr=endptr+1;                           \
    str;})
static void arr_read_line(array_stream *input){
  return arr_get_line(*input);
}

#define stream_getc(stream) (getc(stream))
static char stream_read_char(FILE *stream){
  return stream_getc(stream);
}
#define stream_peek(stream)                     \
  ({char c=getc(stream);                        \
    ungetc(stream);                             \
    c;})
static char stream_peek_char(FILE *stream){
  return stream_peek(stream);
}
#define stream_ungetc(stream) (ungetc(stream))
static void stream_unread_char(FILE *stream){
  return stream_ungetc(stream);
}
#define stream_get_str(stream,n)                \
  ({char *str=xmalloc_atomic(n);                \
    fread(str,n,1,stream);                      \
    str;})
static char* stream_read_str(FILE *stream){
  return stream_get_str(stream);
}
#define stream_eat_line(stream)                 \
  (while(getc(stream)!='\n');)
static void stream_skip_line(FILE *stream){
  return stream_eat_line(stream);
}
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
static char *stream_read_line(FILE *stream){
  return stream_get_line(stream);
}

#define generic_getc(type,obj) type##_getc(obj)
#define generic_peek(type,obj) type##_peek(obj)
#define generic_ungetc(type,obj) type##_ungetc(obj)
#define generic_get_str(type,obj,len) type##_get_str(obj,len)
#define generic_eat_line(type,obj) type##_eat_line(obj)
#define generic_get_line(type,obj) type##_get_line(obj)

static char(*)(void*) 
  read_char_funs[3] = {arr_read_char,cord_read_char,stream_read_char};
static char(*)(void*) 
  peek_char_funs[3] = {arr_peek_char,cord_peek_char,stream_peek_char};
static void(*)(void*) 
  unread_char_funs[3] = {arr_unread_char,cord_unread_char,stream_unread_char};
static void(*)(void*) 
  skip_line_funs[3] = {arr_skip_line,cord_skip_line,stream_skip_line};
static char*(*)(void*) 
  read_str_funs[3] = {arr_read_str,cord_read_str,stream_read_str};
static char*(*)(void*) 
  read_line_funs[3] = {arr_read_line,cord_read_line,stream_read_line};

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
static inline char *get_line(read_input *input){
  get_line_funs[input->input_type](input->input);
}
static inline char *read_line(read_input *input){
  read_line_funs[input->input_type](input->input);
}
