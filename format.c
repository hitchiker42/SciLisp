/*****************************************************************
 * Copyright (C) 2013-2014 Tucker DiNapoli                       *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//Code below mostly taken from cordprnt.c with additions and modfications
//for printing lisp objects
#include "common.h"
#include "print.h"
sexp lisp_format(sexp format,sexp args){
  if(!STRINGP(format)){
    return format_type_error("format","string",format.tag);
  }
  return c_format(format.val.cord,args.val.cons);
}
/*
 * Copyright (c) 1993-1994 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#define CONV_SPEC_LEN 50        /* Maximum length of a single   */
                                /* conversion specification.    */
#define CONV_RESULT_LEN 50      /* Maximum length of any        */
                                /* conversion with default      */
                                /* width and prec.              */
static int ec_len(CORD_ec x){
  return(CORD_len(x[0].ec_cord) + (x[0].ec_bufptr - x[0].ec_buf));
}
/* Possible nonumeric precision values. */
# define NONE -1
# define VARIABLE -2
/* Copy the conversion specification from CORD_pos into the buffer buf  */
/* Return negative on error.                                            */
/* Source initially points one past the leading %.                      */
/* It is left pointing at the conversion type.                          */
/* Assign field width and precision to *width and *prec.                */
/* If width or prec is *, VARIABLE is assigned.                         */
/* Set *left to 1 if left adjustment flag is present.                   */
/* Set *long_arg to 1 if long flag ('l' or 'L') is present, or to       */
/* -1 if 'h' is present.                                                */
static int extract_conv_spec(CORD_pos source, char *buf,
                             int * width, int *prec, int *left, int * long_arg){
  register int result = 0;
  register int current_number = 0;
  register int saw_period = 0;
  register int saw_number = 0;
  register int chars_so_far = 0;
  register char current;

  *width = NONE;
  buf[chars_so_far++] = '%';
  while(CORD_pos_valid(source)) {
    if (chars_so_far >= CONV_SPEC_LEN) return(-1);
    current = CORD_pos_fetch(source);
    buf[chars_so_far++] = current;
    switch(current) {
      case '*':
        saw_number = 1;
        current_number = VARIABLE;
        break;
      case '0':
        if (!saw_number) {
          /* Zero fill flag; ignore */
          break;
        } /* otherwise fall through: */
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        saw_number = 1;
        current_number *= 10;
        current_number += current - '0';
        break;
      case '.':
        saw_period = 1;
        if(saw_number) {
          *width = current_number;
          saw_number = 0;
        }
        current_number = 0;
        break;
      case 'l':
      case 'L':
        *long_arg = 1;
        current_number = 0;
        break;
      case 'h':
        *long_arg = -1;
        current_number = 0;
        break;
      case ' ':
      case '+':
      case '#':
        current_number = 0;
        break;
      case '-':
        *left = 1;
        current_number = 0;
        break;
      case 'd':
      case 'i':
      case 'o':
      case 'u':
      case 'x':
      case 'X':
      case 'f':
      case 'e':
      case 'E':
      case 'g':
      case 'G':
      case 'c':
      case 'C':
      case 's':
      case 'S':
      case 'p':
      case 'r':
      case 'a'://added for lisp
        goto done;
      default:
        return(-1);
    }
    CORD_next(source);
  }
  return(-1);
 done:
  if (saw_number) {
    if (saw_period) {
      *prec = current_number;
    } else {
      *prec = NONE;
      *width = current_number;
    }
  } else {
    *prec = NONE;
  }
  buf[chars_so_far] = '\0';
  return(result);
}

#define va_typecheck(args,typecheck,expected) {                         \
  if(!CONSP(args)){                                                     \
    return error_sexp("Not enough arguments for format string");        \
  }                                                                     \
  if(!typecheck(XCAR(args))){                                           \
    return format_type_error_rest("format",expected,XCAR(args));        \
  }                                                                     \
  }
//int CORD_vsprintf(CORD * out, CORD format, va_list args) {
sexp c_format(CORD format,cons *args){
  CORD_ec result;
  register int count;
  register char current;
  CORD_pos pos;
  char conv_spec[CONV_SPEC_LEN + 1];
  CORD_ec_init(result);
  for (CORD_set_pos(pos, format, 0); CORD_pos_valid(pos); CORD_next(pos)) {
    current = CORD_pos_fetch(pos);
    if (current == '%') {
      CORD_next(pos);
      if (!CORD_pos_valid(pos)){return(-1)};
      current = CORD_pos_fetch(pos);
      if (current == '%') {
        CORD_ec_append(result, current);
      } else {
        int width, prec;
        int left_adj = 0;
        int long_arg = 0;
        CORD arg;
        size_t len;

        if (extract_conv_spec(pos, conv_spec,&width,
                              &prec,&left_adj, &long_arg) < 0) {
          return(-1);
        }
        current = CORD_pos_fetch(pos);
        switch(current) {
          //case 'n': lets not allow this in lisp
            /* Assign length to next arg */
          case 'r':
            /* Append cord and any padding  */
            if (width == VARIABLE) {
              va_typecheck(args,INTP,"integer");
              width = XCAR(args).val.int64;
              args = XCDR(args);
            }
            if (prec == VARIABLE) {
              va_typecheck(args,INTP,"integer");
              prec = XCAR(args).val.int64;
              args = XCDR(args);
            }
            va_typecheck(args,STRINGP,"string");
            arg = XCAR(args).val.cord;
            args = XCDR(args);
            len = CORD_len(arg);
            if (prec != NONE && len > (size_t)prec) {
              if (prec < 0) return(-1);
              arg = CORD_substr(arg, 0, prec);
              len = prec;
            }
            if (width != NONE && len < (size_t)width) {
              char * blanks = GC_MALLOC_ATOMIC(width-len+1);

              memset(blanks, ' ', width-len);
              blanks[width-len] = '\0';
              if (left_adj) {
                arg = CORD_cat(arg, blanks);
              } else {
                arg = CORD_cat(blanks, arg);
              }
            }
            CORD_ec_append_cord(result, arg);
            goto done;
          case 'c':
            if (width == NONE && prec == NONE) {
              register char *str;
              register char c;
              va_typecheck(args,CHARP,"character");
              str = c_wchar_to_string(XCAR(args).val.uchar);
              args=XCDR(args);
              while((c = *str++)){
                CORD_ec_append(result, c);
              }
              goto done;
            }
            break;
          case 's':
          case 'a':
            /*
            //code for width and precision from the %r formatting
            if (width == VARIABLE) {width = va_arg(args, int)};
            if (prec == VARIABLE) {prec = va_arg(args, int)};
            if (prec != NONE && len > (size_t)prec) {
              if (prec < 0) return(-1);
              arg = CORD_substr(arg, 0, prec);
              len = prec;
            }
            if (width != NONE && len < (size_t)width) {
              char * blanks = GC_MALLOC_ATOMIC(width-len+1);

              memset(blanks, ' ', width-len);
              blanks[width-len] = '\0';
              if (left_adj) {
                arg = CORD_cat(arg, blanks);
              } else {
                arg = CORD_cat(blanks, arg);
              }
            }
            CORD_ec_append_cord(result, arg);
            */
            if(!CONSP(args)){
              return error_sexp("Not enough arguments for format string");
            }
            CORD_ec_append_cord(result, arg);
            goto done;
          default:
            break;
        }
        /* Use standard sprintf to perform conversion */
        {
          register char * buf;
          int max_size = 0;
          int res;
          if (width == VARIABLE) {
            va_typecheck(args,INTP,"integer");
            width = XCAR(args).val.int64;
            args = XCDR(args);
          }
          if (prec == VARIABLE) {
            va_typecheck(args,INTP,"integer");
            prec = XCAR(args).val.int64;
            args = XCDR(args);
          }
          if (width != NONE) {max_size = width;}
          if (prec != NONE && prec > max_size) {max_size = prec;}
          max_size += CONV_RESULT_LEN;
          if (max_size >= CORD_BUFSZ) {
            buf = GC_MALLOC_ATOMIC(max_size + 1);
          } else {
            if ((CORD_BUFSZ-(result[0].ec_bufptr-result[0].ec_buf))<max_size){
              CORD_ec_flush_buf(result);
            }
            buf = result[0].ec_bufptr;
          }
          switch(current) {
            case 'd':
            case 'i':
            case 'o':
            case 'u':
            case 'x':
            case 'X':
            case 'c':
              va_typecheck(args,INTP_ANYP,"integer");
              break;
              //not sure about these so ignore them              
              //            case 's':
              //            case 'p':
            case 'f':
            case 'e':
            case 'E':
            case 'g':
            case 'G':
              va_typecheck(args,REALP,"real");
              break;
            default:
              return format_error_sexp("Unrecognized format specifier %c",current);
          }
          res=vsprintf(buf, conv_spec, XCAR(args).val.uint64);
          args=XCDR(args);
          len = (size_t)res;
          if (res < 0) {
            return_errno("printf");
          }
          if (buf != result[0].ec_bufptr) {
            register char c;

            while ((c = *buf++)) {
              CORD_ec_append(result, c);
            }
          } else {
            result[0].ec_bufptr = buf + len;
          }
        }
      done:;
      }
    } else {
      CORD_ec_append(result, current);
    }
  }
  //  count = ec_len(result);
  return cord_sexp(CORD_balance(CORD_ec_to_cord(result)));
}
