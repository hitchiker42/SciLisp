#ifndef LISP_STRINGS_H
#define LISP_STRINGS_H
#include "common.h"
#define STRING_EQ(obj1,obj2) lisp_string_equal(obj1,obj2)
//make a cord out of a stream of bytes
static char* CORD_from_mem(size_t i,void *client_data){
  return ((char*)client_data)[i];
}
sexp lisp_strcat(int numargs,sexp *strings);
sexp lisp_substr(sexp str,sexp start,sexp end);
sexp string_to_array(sexp str);
uint32_t lisp_string_equal(lisp_string str1,lisp_string str2);
sexp sexp_string_equal(sexp obj1,sexp obj2);
#endif
