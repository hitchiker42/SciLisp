#include "common.h"
#include "lisp_strings.h"
char *c_char_to_string(uint64_t lisp_char){
  char str[8]=*(char*)&lisp_char;
  assort(str[7]=='\0');
  return str;
}   
CORD CORD_cat_lisp_string(CORD acc,lisp_string *str){
  if(!str->string[0]){
    return CORD_cat(acc,str->cord);
  } else {
    return CORD_cat_char_star(acc,str->string,str->len);
  }
}
sexp lisp_strcat(int numargs,sexp *strings){
  if(!numargs){
    return "";
  } if (numargs == 1){
    if(!STRINGP(strings[0])){
      raise_simple_error(Etype,format_type_error_opt("concat-str",strings[0].tag));
    }
    return strings[0];
  }//else {
  int i,len=0;
  CORD acc=0;
  for(i=0;i<numargs;i++){
    if(!STRINGP(strings[i])){
      raise_simple_error(Etype,format_type_error("concat-str","string",strings[i].tag));
    }
    acc=CORD_cat_lisp_string(acc,strings[i].str);
    len+=strings[i].str->len;
  }
  lisp_string *retval=xmalloc(sizeof(lisp_string));
  *retval=(lisp_string){.cord=acc,.len=len}
  return string_sexp(retval);
}
sexp lisp_substr(sexp str,sexp start,sexp end){
  if(!STRINGP(lisp_cord)||!INTP(start)||!INTP(end)){
    return format_type_error("substr","string",lisp_cord.tag,
                             "integer",start.tag,"integer",end.tag);
  } else {
    lisp_string *retval=xmalloc(sizeof(lisp_string));
    retval.len=end.val.int64-start.val.int64;
    //the way CORD_substr works is probably a lot better
    //than anything I could write
    retval.cord=(CORD_substr(lisp_cord.val.cord,start.val.int64,
                             end.val.int64-start.val.int64));
    return string_sexp(retval);
  }
}
//convert a string (const char */CORD) to an array of multibyte chars
//stored as 64 bit integres
sexp string_to_array(sexp str){
  //I can't think of an easy way to do this on cords
  const char *str=CORD_to_const_char_star(str.val.string->cord);
  int len=str.val.string->len;
  //makes a pesimistic  guess about the number of characters in str
  sexp *new_array=xmalloc_atomic(sizeof(sexp)*len);
  //test if str is multibyte, but I need to make sure I
  //actually set and propagate the multibyte flag before
  //I can do that  confidently
  mbstate_t state;
  size_t nbytes;
  memset(&state,'\0',sizeof(mbstate_t));
  int index=0,arr_index=0;
  uint64_t mb_char=0;
  while(index<len){
    nbytes=mbrlen(str+index,len-index,&state);
    if(nbytes == (size_t)-1 || nbytes == (size_t)-2){
      //error
      raise_simple_error(Efatal,"Error in string to array");
    }
    memcpy(&mb_char,str+index,nbytes);
    index+=nbytes;
    new_array[arr_index++]=uint64_sexp(mb_char);
    mb_char=0;
  }
  //incomplete
}
struct cord_iter_data {
  union {
    char *result;//maybe this should be an sexp* ?
    sexp *sexp_result;
  };
  sexp(*f)(sexp);
  int index;
};
//these functions map strings -> general arrays
static int cord_map_char_sexp(char c,struct cord_iter_data *data){
  data->sexp_result[data->index]=data->f(c_char_sexp(c));
  return 0;
}
static int cord_map_string_sexp(const char *s,struct cord_iter_data *data){
  int i=0;
  do {
    data->sexp_result[data->index]=data->f(c_char_sexp(s[i]));
  } while (s[++i]);
  return 0;
}
static sexp cord_map(CORD s,sexp(*f)(sexp),int len){
  struct cord_iter_data data;
  data->result=xmalloc_atomic(len);
  CORD_iter5(s,0,cord_map_char_sexp,cord_map_string_sexp,data);
  //make a retval
  return retval;
}
sexp string_map_sexp(sexp string,sexp map_fn){
  assert(0);//fail if actually called, for now
  //get a fn poiner from map_fn somehow
  sexp(*f)(sexp)=map_fn.val.subr->comp.f1;
  const char *str=string.val.string->string;
  int len=string.val.string->len;
  if(str[0]){
    sexp *retval=xmalloc(len*sizeof(sexp));
    int i;
    for(i=0;i<len;i++){
      retval[i]=f(c_char_sexp(str[i]));
    }
  } else {
    return cord_map_string_sexp(str,f,len);
  }
}
//these map strings->strings
//horrible name but eh
#define f_of_sexp_to_char(f,c)                  \
  ((f(c_char_sexp(c))).val.c_char)
static int cord_map_char(char c,struct cord_iter_data *data){
  data->result[data->index++]=f_of_sexp_to_char(data->f,c);
  return 0;
}
static int cord_map_string(const char *s,struct cord_iter_data *data){
  int i=-1;
  while(s[++i]){
    data->result[data->index++]=f_of_sexp_to_char(data->f,s[i]);
  }
  return 0;
}
static lisp_string *cord_map(CORD s,sexp(*f)(sexp),int len){
  struct cord_iter_data data;
  data->result=xmalloc_atomic(len);
  CORD_iter5(s,0,cord_map_char,cord_map_string,data);
  lisp_string retval=xmalloc(sizeof(lisp_string));
  retval->string=data->result;
  retval->len=data->index;
  return retval;
}
sexp string_map(sexp string,sexp map_fn){
  assert(0);//fail if actually called, for now
  //get a fn poiner from map_fn somehow
  sexp(*f)(sexp)=map_fn.val.subr->comp.f1;
  const char *str=string.val.string->string;
  int len=string.val.string->len;
  if(str[0]){
    char *result=xmalloc_atomic(len);
    int i;
    for(i=0;i<len;i++){
      result[i]=f_of_sexp_to_char;
    }
  } else {
    return string_sexp(cord_map(str,f,len));
  }
}
#if 0
#includ "gc/ec.h"
typedef struct CORD_stream *CORD_stream_ptr
#define buf_pos(stream) (stream->bufptr-stream->buf)
#define BUFSIZE 128
struct CORD_stream {
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
   .seek=NULL,.close=NULL};
/*ssize_t CORD_stream_seek(void *cookie,off64_t *POSITION,int WHENCE){
  switch(WHENCE){
    case SEEK_SET://begining of file
    case SEEK_CUR://current position
    case SEEK_END://end position*/
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
      stream->stream=data;
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
#endif
