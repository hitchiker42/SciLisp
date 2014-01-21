/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef _PRINT_H
#define _PRINT_H
/*translate an enum _tag value into a meaniningful string.
 *usually just translates the tag name into a string(striping the leading _)
 *but for abbreviations expands into the logical string*/
lisp_string tag_name(sexp_tag obj_tag) __attribute__ ((const));
//call tag_name on the tag of obj
lisp_string  typeName(sexp obj) __attribute__((const));
//return a lisp object contatining the value of typeName(obj)
sexp lisp_typeName(sexp obj);
/* print a formatted representation of a number using the format specifier
 * format, or a sane default if format is 0(ie an empty string).
 * returns the empty string is obj is not a double or long value.*/
lisp_string  print_num_format(sexp obj,CORD format) __attribute__((const));
/* convience function to call print_num_format with an empty string as format*/
lisp_string  print_num(sexp obj);
/* create a string representation of the lisp object obj.
 * what this means depends on the object, but if possible the returned value
 * should be able to be parsed back into the same object.
 * for type which are unimplemented returns "print error got type $typename".
 * will recursively(actually via a loop) print the values in a cons cell or
 * list*/
lisp_string print(sexp obj);
//really ought to consolodate these next functions into one w/optional args
/* return a lisp object contating the printed representation of obj*/
sexp lisp_print(sexp obj);
sexp lisp_fprint(sexp obj,sexp file);
/* return a lisp object containing the preinted representation of obj 
 * with an added traling newline */
sexp lisp_println(sexp obj);
sexp lisp_fprintln(sexp obj,sexp file);
//return lambda for a lambda function or the lisp name of a primitive function
lisp_string subr_name(subr fun);
//return the name of the enum value of token as a string
lisp_string token_name(TOKEN token);
sexp lisp_fputs(sexp string,sexp stream);
//code in format.c
sexp c_format(CORD format,sexp args);
sexp lisp_format(sexp format,sexp args);
#endif
