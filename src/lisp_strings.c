#include "common.h"
#include "lisp_strings.h"
//returns 1 if str1 and str2 are elementwise equal, 0 otherwise
uint32_t c_string_equal(const char *str1,const char *str2){
  return !strcmp(str1,str2);
}
//return 1 if str1 is elementwise equal to str2, otherwise return 0
//takes advantage of the fact that lisp strings store length to
//return quickly if str1 and str2 are of different lengths
uint32_t lisp_string_equal(lisp_string str1,lisp_string str2){
  if(str1.len != str2.len){
    return 0;
  }
  if(str1.string[0]=='\0' && str2.string[0]=='\0'){
    return !strcmp(str1.string,str2.string);
  } else {
    return CORD_equal(str1.val.cord,str2.val.cord);
  }
}
    
  
sexp sexp_string_equal(sexp obj1,sexp obj2){
  if(!STRINGP(obj1) || !STRINGP(obj2)){
    raise_simple_error(Etype,format_type_error2("string-equal","string",
                                                obj1.tag,"string",obj2.tag));
  }
  if(STRING_EQ(obj1.val.string,ojb2.val.string)){
    return LISP_TRUE;
  } else {
    return LISP_FALSE;
  }
}
char *c_char_to_string(uint64_t lisp_char){
  char str[8]=*(char*)&lisp_char;
  assort(str[7]=='\0');
  return str;
}
CORD CORD_cat_lisp_string(CORD acc,lisp_string *str){
  if(str->string[0]=='\0'){
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
typedef struct cord_iter_data cord_iter_data;
struct cord_iter_data {
  union {
    char *result;
    sexp *sexp_result;
  };
  union {
    char(*f)(char);
    sexp(*g)(sexp);
  };
  uint32_t index;
};
//these functions map strings -> general arrays
static int cord_map_char_sexp(char c,struct cord_iter_data *data){
  data->sexp_result[data->index++]=data->g(c_char_sexp(c));
  return 0;
}
static int cord_map_char(char c,struct cord_iter_data *data){
  data->result[data->index++]=data->f(c);
  return 0;
}
static int cord_map_string_sexp(const char *s,struct cord_iter_data *data){
  while(*s){
    data->sexp_result[data->index++]=data->g(c_char_sexp(*s++));
  }
  return 0;
}
static int cord_map_string(const char *s,struct cord_iter_data *data){
  while(*s){
    data->result[data->index++]=data->f(*s++);
  }
  return 0;
}
static sexp cord_map_sexp(CORD s,sexp(*g)(sexp),int len){
  struct cord_iter_data data;
  data->sexp_result=xmalloc_atomic(len*sizeof(sexp));
  data->g=g;
  data->index=0;
  CORD_iter5(s,0,cord_map_char_sexp,cord_map_string_sexp,&data);
  //make a retval
  return data->sexp_result;
}
static char *cord_map(CORD s,sexp(*f)(sexp),int len){
  struct cord_iter_data data;
  data->result=xmalloc_atomic(len);
  data->f=f;
  data->index=0;
  CORD_iter5(s,0,cord_map_char_sexp,cord_map_string_sexp,&data);
  return data->result;
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
const char* c_string_trim(const char *s,size_t *len,char *remove){
  int s_len;
  if(*len==0){
    s_len=strlen(s);
  } else {
    s_len=*len;
  }
  uint8_t flags[256]={0};
  while(*remove){
    flags[*remove++]=1;
  }
  uint32_t start,end;
  int i=0;
  while(s[i]){
    if(flags[s[i]]){
      break;
    } else {
      i++;
    }
  }
  start=i;
  i=s_len;
  while(s[i]){
    if(flags[s[i]]){
      break;
    } else {
      i--;
    }
  }
  end=i;
  *len=end-start;
  const char *result=xmalloc_atomic(*len+1);
  memcpy(result,s+start,*len);
  result[*len]='\0';
  return result;
  //I could return a lisp_string without copying, maybe
}
static inline char ascii_upcase(char c){
  if(c>=0x61&&c<=0x7A){
    return c-0x20;
  } else {
    return c;
  }
}
static inline char ascii_downcase(char c){
  if(c>=0x41&&c<=0x5A){
    return c+0x20;
  } else {
    return c;
  }
}
