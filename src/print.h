/* Header file defining different print routines

   Copyright (C) 2013-2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
#ifndef _PRINT_H
#define _PRINT_H
/*translate an enum _tag value into a meaniningful string.
 *usually just translates the tag name into a string(striping the leading _)
 *but for abbreviations expands into the logical string*/
const char *tag_name(enum sexp_tag obj_tag) __attribute__ ((const));
//call tag_name on the tag of obj
lisp_string  type_name(sexp obj) __attribute__((const));
//return a lisp object contatining the value of typeName(obj)
sexp lisp_typeName(sexp obj);
/* print a formatted representation of a number using the format specifier
 * format, or a sane default if format is 0(ie an empty string).
 * returns the empty string is obj is not a double or long value.*/
CORD  print_num_format(sexp obj,CORD format) __attribute__((const));
/* convience function to call print_num_format with an empty string as format*/
CORD  print_num(sexp obj);
/* create a string representation of the lisp object obj.
 * what this means depends on the object, but if possible the returned value
 * should be able to be parsed back into the same object.
 * for type which are unimplemented returns "print error got type $typename".
 * will recursively(actually via a loop) print the values in a cons cell or
 * list*/
CORD print(sexp obj);
//really ought to consolodate these next functions into one w/optional args
/* return a lisp object contating the printed representation of obj*/
sexp lisp_print(sexp obj);
sexp lisp_fprint(sexp obj,sexp file);
/* return a lisp object containing the preinted representation of obj 
 * with an added traling newline */
sexp lisp_println(sexp obj);
sexp lisp_fprintln(sexp obj,sexp file);
//return lambda for a lambda function or the lisp name of a primitive function
const char *subr_name(subr fun);
//return the name of the enum value of token as a string
const char *token_name(TOKEN token) __attribute__((const));
sexp lisp_fputs(sexp string,sexp stream);
//code in format.c
sexp c_format(CORD format,int numargs,sexp *args);
sexp lisp_format(int numargs,sexp *args);
sexp type_of_tag(sexp_tag tag);
#endif
