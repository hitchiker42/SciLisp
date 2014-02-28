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
sexp internal_read(read_input *input,int *pch,int flags);
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
//it also should probably be reneamed, maybe
sexp start_read(read_input *input);//depricated

//read the next sexp from input, mostly for interactive input
sexp read_sexp(read_input *input);
//return a list of all sexps read from input, mostly for non-interactive input
sexp read_full(read_input *input);
//non-re-entrent convenience functions 
//these are literally implemented by running make_???_input and passing
//the return value to start_read
sexp read_from_cord(CORD input);
sexp read_from_stream(FILE *stream);
sexp read_from_string(char *string);
//convience function to call read_full on a file
sexp read_file(FILE *input);
#endif
