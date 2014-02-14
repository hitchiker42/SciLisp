#ifndef _READ_H_
#define _READ_H_
#include "common.h"
//this could be useful elsewhere
#define HEXVALUE(c)                                     \
  (((c) >= 'a' && (c) <= 'f')                           \
   ? (c)-'a'+10                                         \
   : (c) >= 'A' && (c) <= 'F' ? (c)-'A'+10 : (c)-'0')
typedef struct read_input read_input;
struct read_input {
  void *input;
  int input_type;
};
//the main reading function internaly
sexp internal_read(char *input,int *pch,int flags);
sexp lisp_read(sexp input);//the actual lisp read function
//return a read_input struct which can be used to call internal read
//these assume that their input (the CORD,FILE* or char*) will not be
//externally modified, so if you intend to modify the input pass a copy
//of it to these functions
read_input *make_cord_input(CORD input);
read_input *make_stream_input(FILE *input);
read_input *make_string_input(const char *input);
//general entry point for reading from a read_input object
//the input object keep's it's own state so this function is re-entrent
//(acutally I might need to do a bit of work to make it totally reentrent)
sexp start_read(read_input *input);
//non-re-entrent convenience functions 
//these are literally implemented by running make_???_input and passing
//the return value to start_read
sexp read_from_cord(CORD input);
sexp read_from_stream(FILE *stream);
sexp read_from_string(char *string);
#endif
