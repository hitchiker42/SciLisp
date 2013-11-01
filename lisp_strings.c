#include "common.h"
#include "lisp_strings.h"
#include "gc/include/gc/ec.h"
//is this too confusing?
typedef struct CORD_stream *CORD_stream
#define buf_pos(stream) (stream->bufptr-stream->buf)
#define BUFSIZE 128
struct CORD_stream{
  CORD stream;
  CORD_pos pos;//expensive to change, faster to use
  int index;//easy to increment, always points to current position
  int len;
  short at_end;
  short at_start;  
  //idea taken from ec.h
  /*  char *bufptr;
      char buf[BUFSIZE]; */
};
CORD_stream CORD_stream_new(){
  CORD_stream retval=xmalloc(sizeof(struct CORD_stream));
  retval->stream=0;
  //  retval->bufptr=retval->buf;
  retval->pos=xmalloc(sizeof(struct CORD_POS));
  retval->len=0;
  retval->at_start=1;
  CORD_set_pos(retval->pos,retval->stream,0);
}

/*//copied from cordxtra.c, only changes are syntatic
void CORD_stream_flush_buf(CORD_stream x)
{
    register size_t len = x->bufptr - x->buf;
    char *s;
    if (len == 0){return};
    s = xmalloc_atomic(len+1);
    memcpy(s, x->buf, len);
    s[len] = '\0';
    x->stream = CORD_cat_char_star(x->stream, s, len);
    x->bufptr = x->buf;
}
# define CORD_stream_append_char(x, c)          \
  {                                             \
    if ((x)->bufptr == (x)->buf + BUFSIZE) {    \
      CORD_stream_flush_buf(x);                 \
    }                                           \
    *((x)->ec_bufptr)++ = (c);                  \
  }
void CORD_stream_append_cord(CORD_stream x, CORD s)
{
    CORD_ec_flush_buf(x);
    x[0].ec_cord = CORD_cat(x[0].ec_cord, s);
    }*/
cookie_read_function CORD_stream_read;
cookie_write_function CORD_stream_write;
cookie_seek_function CORD_stream_seek;
cookie_close_function CORD_stream_close;
struct cookie_io_functions_t CORD_stream_functions = 
  {.read=CORD_stream_read,.write=CORD_stream_write,
   .seek=CORD_stream_seek,.close=CORD_stream_close};
ssize_t CORD_stream_read(void *cookie,char *buffer, size_t size){
  CORD_stream stream=(CORD_stream)cookie;
  //return 0 if there's nothing to read
  if(size==0 || stream->index>stream->len){return 0;}
  //make sure size isn't bigger than the size of the stream
  if(size>stream->len){size=stream->len;}
  //if we're at the start we can do things a bit faster
  if(stream->at_start){
    char *result=memcpy(buffer,CORD_to_const_char_star(stream->stream),size);
    if(!result){return -1;}
    stream->index+=size;
    stream->at_start=0;
    return size;
  } else {
    //otherwize we need to use the positon, which is reset if needed
    if(stream->index>CORD_pos_to_index(stream->pos)){
      CORD_set_pos(stream->pos,stream->stream,stream->index);
    }
    char *result=memcpy
      (buffer,CORD_to_const_char_star(CORD_pos_to_cord(stream->pos)),size);
    if(!result){return -1;}
    stream->index+=size;
    return size;
  }
}
ssize_t CORD_stream_write(void *cookie,char *buffer, size_t size){
  CORD_stream stream=(CORD_stream)cookie;
    //not sure how to deal with nul char(for the CORD_cat bit)
  char* data=xmalloc_atomic(sizeof(size+1));
  data=memcpy(data,buffer,size);
  data[size]='\0';
  if(!data){return 0};
  if(stream->at_start){
    if(size>stream->len){
      stream->stream=CORD_cat_char_star(0,data,size);
      stream->len=size;
    } else {
      stream->stream=
        CORD_cat(data,CORD_substr(stream->stream,size,stream->len-size));
    }
    stream->at_start=0;
    stream->index+=size;
    return size;
  } else {
    if(size>(stream->len-stream->index)){
      stream->stream=CORD_cat_char_star
        (CORD_substr(stream->stream,0,stream->index),data,size);
      stream->len=stream->index+size;
    } else {
      stream->stream=CORD_cat_n
        (3,CORD_substr(stream->stream,0,stream->index),data,
         CORD_substr(stream->stream,stream->index+size,stream->len));
    }
    stream->len+=size;
    stream->index+=size;
    return size;
  }
}
