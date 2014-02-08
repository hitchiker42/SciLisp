#ifndef _LISP_UTF8_H
#define _LISP_UTF8_h
//minimal includes so this can stand on it's own
#include "common.h"
static const int utf8_max[6]={0x7F,0x7FF,0xFFFF,0x1FFFFF,0x3FFFFFF,0x7FFFFFFF};
static const int utf8_min[6]={0x9,0x80,0x800,0x10000,0x200000,0x4000000};
static const uint8_t utf8_initial_mask[6]={0x0,0b00011111,0b00001111,
                                           0b00000111,0b00000011,0b00000001};
static const uint8_t utf8_rest_mask=0b00111111;
static const int utf8_len_max=8;//really it's 6, but setting this to 8 makess life eaiser
#define UTF8_LEN_MAX 8
/*
  Simple UTF-8 spec
  bits of    | First      | Last         | Num Bytes | Leading    |
  code point | code point | code point   |           | byte       |
  ----------------------------------------------------------------|
  |    7     |  0x0       |  0x7F        |    1      | 0b0xxxxxxx |
  |    11    |  0x80      |  0x7FF       |    2      | 0b110xxxxx |
  |    16    |  0x800     |  0xFFFF      |    3      | 0b1110xxxx |
  |    21    |  0x10000   |  0x1FFFFF    |    4      | 0b11110xxx |
  |    26    |  0x200000  |  0x3FFFFFF   |    5      | 0b111110xx |
  |    31    |  0x4000000 |  0x7FFFFFFF  |    6      | 0b1111110x |

  All bytes following the leading byte in a multibyte sequence have the form
  0b10xxxxxx
  in any instance the bytes 0b11111110 and 0b11111111 are illegal
 */
//given the first byte in a possible utf8 sequence
//return 
int utf8_char_len(uint8_t mb_char);
int utf8_mb_char_len(uint8_t mb_char);
//same semantics as mbrtowc
//convert a valid utf-8 sequence of upto size bytes from src
//and store the result in dest, if src contains a possibly valid
//sequence that cannot be resolved in size bytes -2 is returned
//and dest is set to NULL for any other error dest is set to NULL
//errno is set and -1 is returned. otherwise dest is set to the 
//result and the number of bytes used is returned
size_t utf8_decode_char(const uint8_t* src, uint32_t *dest, size_t size);
size_t utf8_encode_char(uint8_t* dest, uint32_t src);
typedef struct encode_state utf8_encode_state;
typedef struct decode_state utf8_decode_state;
struct decode_state_simple {//indivual char state
  wchar_t result;//where to store the partial result
  uint8_t state;//0 if in initial state
  uint8_t total_bytes;//total bytes in current char
  uint8_t bytes_remaning;//bytes left in current char
};
struct encode_state {
  //state for the string
  wchar_t *src;
  char *dest;
  int maxchars;//0 to scan untill null
  int src_offset;//current widechar to decode is src[offset]
  int dest_offset;
  int dest_size;
  int resize;
  int complete;
  //no indivual char state
};
struct decode_state {
  //state for the string 
  char *src;
  wchar_t *dest;
  int maxchars;//0 to scan untill null
  int src_offset;//offset in src to the start of the current mbchar
  int dest_offset;//
  //state for indivual chararcter
  uint8_t state;//0 if in initial state
  uint8_t total_bytes;//total bytes in current char
  uint8_t bytes_consumed;//bytes used so far in current char
  wchar_t result;//where to store the partial result
};
#endif
