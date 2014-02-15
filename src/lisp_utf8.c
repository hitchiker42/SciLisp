#include "lisp_utf8.h"
#include "extra/read_tables.h"
//from libutf8
#define IS_VALID_CONT_BYTE(byte) (((byte) & 0xC0) == 0x80)
#define IS_INVALID_CONT_BYTE(byte) (((byte) & 0xC0) != 0x80)
//using binary to make things a bit more clear
/*#define IS_VALID_CONT_BYTE(byte) ((byte & 0b11000000) == 0b10000000)
  #define IS_INVALID_CONT_BYTE(byte) ((byte & 0b11000000) != 0b10000000)*/
int utf8_isucs4(uint32_t ch){
    return !(ch & (~((wchar_t)0x7FFFFFFF)))
        && (ch < 0xD800 || ch > 0xDFFF)
        && (ch != 0xFFFE) && (ch != 0xFFFF);
}
int utf8_char_len(uint8_t mb_char){
  if(mb_char<0x80){
    return 1;
  } else if (mb_char>=0xFE || (mb_char > 0x80 && mb_char < 0xC0)){//invalid leading byte
    return -1;
  } else if (mb_char<0xE0){
    return 2;
  } else if (mb_char<0xF0){
    return 3;
  } else if (mb_char<0xF8){
    return 4;
  } else if (mb_char<0xFC){
    return 5;
  } else if (mb_char<0xFE){
    return 6;
  } else {//to shut up a warning that isn't even valid
    return 0;
  }
}
//only call on a character known to be a valid non-ascii leading byte in utf8
int utf8_mb_char_len(uint8_t mb_char){
  if (mb_char<0xE0){
    return 2;
  } else if (mb_char<0xF0){
    return 3;
  } else if (mb_char<0xF8){
    return 4;
  } else if (mb_char<0xFC){
    return 5;
  } else if (mb_char<0xFE){
    return 6;
  } else {//to shut up a warning that isn't even valid
    return 0;
  }
}
//internal use only, really unsafe, need to check
//for 0xFE/0xFF and >0x80 yourself
/*static inline int utf8_char_len_unsafe(char mb_char){
  return 1-ffz(mb_char);
  }*/
/*
  modified quite a bit from libutf8, libutf8 takes a parameter giving the length of dest
  and checks it, which I don't, and the value returned is different, it returns a pointer
  to the dest+bytes_used whereas I just return bytes used
 */
size_t utf8_encode_char(uint8_t* dest, uint32_t src){
  if(!dest){
    dest = xmalloc_atomic(utf8_len_max);
  }
  if(!utf8_isucs4(src)) {//test if ch is a valid codepoint
    errno = EILSEQ;
    return (size_t)-1;
  }
  int i=0;
  if(src < 0x80) {
    dest[i++] = src;
  } else if(src < 0x800) {
    dest[i++] = 0xC0 | ((src >> 6) & 0x1F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else if(src < 0x10000) {
    dest[i++] = 0xE0 | ((src >> 12) & 0xF);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else if(src < 0x200000) {
    dest[i++] = 0xF0 | ((src >> 18) & 0x7);
    dest[i++] = 0x80 | ((src >> 12) & 0x3F);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else if(src < 0x4000000) {
    dest[i++] = 0xF8 | ((src >> 24) & 0x3);
    dest[i++] = 0x80 | ((src >> 18) & 0x3F);
    dest[i++] = 0x80 | ((src >> 12) & 0x3F);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  } else {
    dest[i++] = 0xFC | ((src >> 30) & 0x1);
    dest[i++] = 0x80 | ((src >> 24) & 0x3F);
    dest[i++] = 0x80 | ((src >> 18) & 0x3F);
    dest[i++] = 0x80 | ((src >> 12) & 0x3F);
    dest[i++] = 0x80 | ((src >> 6) & 0x3F);
    dest[i++] = 0x80 | (src & 0x3F);
  }
  return i;
}
/*
  modified from libutf8 in a similar way to encode_char
 */
size_t utf8_decode_char(const uint8_t* src, uint32_t *dest, size_t size){
  int i=0;
  wchar_t retval,min;
  //deal with invaid arguments and ascii chars
  if(!size){
    return (size_t)-2;
  } else if(!src && size) {
    errno = EINVAL;
    return (size_t)-1;
  } else if(src[0]>=0XFE){
    errno=EILSEQ;
    return (size_t)-1;
  } else if (src[0]<0x80){
    *dest=src[0];
    return 1;
  }
  int needed=utf8_char_len(src[0]);
  min=utf8_min[needed-1];
  retval=src[0] & utf8_initial_mask[needed-1];
  size=MIN(size,needed);
  while(++ i&& --size){//i is first so we can return it regardless
    if(IS_INVALID_CONT_BYTE(src[i])){
        errno = EILSEQ;
        return (size_t)-1;
    }
    //0x3f == 0b00111111
    /*ex retval = 0b00011111
      retval <<=6 retval = 0b00000111 0b11000000
    //ok I guess this works I'm not 100% sure on how though
     */
    retval <<= 6;
    retval |= src[i] & 0x3F;
  }
  if(retval < min) {
    errno = EILSEQ;
    return (size_t)-1;
  }
  if(i < needed){
    return (size_t)-2;
  }
  *dest=retval;
  return i;
}
#define default_dest_size 64
utf8_encode_state *init_encode_state(uint32_t *src,char *dest,int maxchars,int dest_size){
  utf8_encode_state *state=xmalloc_atomic(sizeof(struct encode_state));
  //xmalloc_atomic doesn't 0 data
  memset(state,'\0',sizeof(struct encode_state));
  if(!dest){
    dest=xmalloc_atomic(default_dest_size);
    state->dest=dest;
    state->resize=1;
    state->dest_size=default_dest_size;
  } else {
    state->dest=dest;
    state->dest_size=dest_size;
    state->resize=0;
  }
  state->src=src;
  state->maxchars=maxchars;
  return state;
}
/*
  given a string str of length len determine if str is a valid utf8 string
  if it is return 0 otherwise return the index of the first invalid character
  or len if str ends on a possibly valid but incomplete sequence
 */
int validate_utf8_string(uint8_t *str,uint32_t len){
  uint64_t i;
  for(i=0;i<len;i++){
    switch(UTF8_table[str[i]]){
      case utf8_null:
      case utf8_ascii:
        continue;
      case utf8_invalid:
        return 0;
        //the following all deliberately fall through
      case utf8_start_6:
        if(++i>=len){
          return i;
        }
        if(UTF8_table[str[i]] != utf8_cont){
          return i;
        }        
      case utf8_start_5:
        if(++i>=len){
          return i;
        }
        if(UTF8_table[str[i]] != utf8_cont){
          return i;
        }        
      case utf8_start_4:
        if(++i>=len){
          return i;
        }
        if(UTF8_table[str[i]] != utf8_cont){
          return i;
        }        
      case utf8_start_3:
        if(++i>=len){
          return i;
        }
        if(UTF8_table[str[i]] != utf8_cont){
          return i;
        }
      case utf8_start_2:
        if(++i>=len){
          return i;
        }
        if(UTF8_table[str[i]] != utf8_cont){
          return i;
        }  
    }
  }
  return 0;
}

#if 0
//INCOMPLETE
//reentrent, i.e interuptable, state contains the infomation needed
//to encode a wide string, and can be interupted and restanted
//cur_maxchars, if not 0 tells the max ammount of characters to encode this call
size_t encode_with_state(utf8_encode_state *state,size_t cur_maxchars){
  if(!cur_maxchars){
    cur_maxchars=(size_t)-1;
  } else if (cur_maxchars >state->maxchars){
    cur_maxchars=state->maxchars;
  }
  union {
    char bytes[8];
    uint8_t mb_val;
  } temp;
  size_t nbytes;
  int initial_offset=state->src_offset;
  while(cur_maxchars){
    temp.mb_val=0;
    nbytes=utf8_encode_char(temp.bytes,state->src[state->src_offset]);
    if(nbytes == (size_t)-1){
      return (size_t)-1;//needs a better way to do this
    }
    //needs work
    if(!temp.mb_val){
      state->complete=1;
      return state->src_offset-initial_offset;
    }
    if(state->dest_offset+nbytes>state->dest_size){
      if(state->resize){
        state->dest_size*=2;
        state->dest=xrealloc(state->dest,state->dest_size);
      } else {
        errno=ERANGE;
        return (size_t)-2;
      }
    }
    cur_maxchars--;
    state->maxchars--;
    //this will probably give and invalid lvalue error
    (uint64_t*)state->dest+dest_offset=temp.mb_val;
    state->dest_offset+=nbytes;
    state->src_offest++;
  }
  if(!state->maxchars){
    state->complete=1;
    return state->src_offset-initial_offset;
  }
}
utf8_decode_state *init_decode_state(char *src,uint32_t *dest,int maxchars,int dest_size);
//encode characters from src untill either a null character is reached
//or maxchars chars have been read from src; if maxchars is 0 keep encoding
//untill a nul character is reached
//if bufsize is not 0 than it specifies teh max number of bytes to
//store in dest, if  more bytes are required (size_t)-2 is returned. if bufsize is 0
//a buffer is allocated which is large enough to hold the result of encoding  src
size_t utf8_encode_string(char *dest,size_t bufsize,const uint32_t *src,size_t maxchars){
  if(!maxchars){
    maxchars=(size_t)-1;//should be equal to SIZE_T_MAX
  }
  int resize_buf=0;
  if(!bufsize){
    dest=xmalloc_atomic(64);//really just an arbitary size
    resize_buf=1;
  }
  int i=0,j=0;
  size_t nbytes;
  while(src[j] && j < maxchars){
    if(i<bufsize){
      if(resize_buf){
        bufsize*=2;
        dest=xrealloc(dest,bufsize);
      } else {
        return (size_t)-2;//needs better error handling
      }
    }
    nbytes=utf8_encode_char(dest+i,src[j]);
    if(nbytes == (size_t)-1){
      return (size_t)-1;//needs better error handling
    }
    i+=nbytes;
    j++;
  }
  return j;
}
//same as above but ignore nuls
size_t utf8_encode_mem(char *dest,size_t bufsize,const uint32_t *src,size_t maxchars){
  if(!maxchars){
    return 0;
  }
  int resize_buf=0;
  if(!bufsize){
    dest=xmalloc_atomic(64);//really just an arbitary size
    resize_buf=1;
  }
  int i=0,j=0;
  size_t nbytes;
  while(j < maxchars){
    if(i<bufsize){
      if(resize_buf){
        bufsize*=2;
        dest=xrealloc(dest,bufsize);
      } else {
        return (size_t)-2;//needs better error handling
      }
    }
    nbytes=utf8_encode_char(dest+i,src[j]);
    if(nbytes == (size_t)-1){
      return (size_t)-1;//needs better error handling
    }
    i+=nbytes;
    j++;
  }
  return j;
}

int utf8_isascii(wchar_t ch){
    return !(ch & ~0x7F);
}
int utf8_isspace(wchar_t ch){
    return((ch >= 0x0009 && ch <= 0x000D)
            || ch == 0x0020
            || ch == 0x0085
            || ch == 0x00A0
            || ch == 0x1680
            || ch == 0x180E
            || (ch >= 0x2000 && ch <= 0x200A)
            || ch == 0x2028
            || ch == 0x2029
            || ch == 0x202F
            || ch == 0x205F
            || ch == 0x3000);
}
int utf8_iseol(wchar_t ch){
    return (ch >= 0x000A && ch <= 0x000D)
        || ch == 0x0085
        || ch == 0x2028
        || ch == 0x2029;
}
int utf8_isutf32(wchar_t ch){
    return ch >= 0 && ch <= 0x10FFFF
        && (ch < 0xD800 || ch > 0xDFFF)
        && (ch != 0xFFFE) && (ch != 0xFFFF);
}
int utf8_isutf16(wchar_t ch){
    return ch >= 0 && ch <= 0xFFFD
        && (ch < 0xD800 || ch > 0xDFFF);
}
#endif

