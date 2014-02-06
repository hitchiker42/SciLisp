//from libutf8
//using binary to make things a bit more clear
#define IS_VALID_CONT_BYTE(byte) ((byte & 0b11000000) == 0b10000000)
#define IS_INVALID_CONT_BYTE(byte) ((byte & 0b11000000) != 0b10000000)
//from the linux kernel
//not sure why I couldn't think to do bsf !val but eh
static inline uint64_t ffz(uint64_t word){
#ifdef __x86_64__ 
  asm("bsf %1,%0"
      : "=r" (word)
      : "r" (~word));
  return word;
#else
  return ffsl(~word);
}
int utf8_char_len(char mb_char){  
  if(mb_char<0x80){
    return 1;
  } else if (mb_char>=0xFE){//invalid leading byte
    return -1;
  } else {
    return ffz(mb_char)-1;
  }
}
//like 
int utf8_char_len_unsafe(char mb_char){
  return 1-ffz(mb_char);
}
/*
  modified quite a bit from libutf8, libutf8 takes a parameter giving the length of dest
  and checks it, which I don't, and the value returned is different, it returns a pointer
  to the dest+bytes_used whereas I just return bytes used
 */
size_t utf8_encode_char(char* dest, wchar_t src){
  if(!dest){
    dest = xmalloc_atomic(utf8_len_max);
  }
  if(!utf8_isucs4(ch)) {//test if ch is a valid codepoint
    errno = EILSEQ;
    return 0;
  }
  int i=0;
  if(src < 0x80) {
    dest[i++] == src;
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
size_t utf8_decode_char(const char* src, wchar_t *dest, size_t size){
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
  int needed=ffz(src[0])-1;
  min=utf8_min[needed];
  retval=src[0] & utf8_initial_mask[needed-1];
  while(i++&& --size){//i is first so we can return it regardless
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
    return (size_t)-1
  }
  if(needed>size){
    return (size_t)-2;
  }
  *dest=retval;
  return i;
}
//encode characters from src untill either a null character is reached
//or maxchars chars have been read from src; if maxchars is 0 keep encoding
//untill a nul character is reached
//if bufsize is not 0 than it specifies teh max number of bytes to 
//store in dest, if  more bytes are required (size_t)-2 is returned. if bufsize is 0
//a buffer is allocated which is large enough to hold the result of encoding  src
size_t utf8_encode_string(char *dest,size_t bufsize,const wchar_t *src,size_t maxchars){
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
size_t utf8_encode_mem(char *dest,size_t bufsize,const wchar_t *src,size_t maxchars){
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
