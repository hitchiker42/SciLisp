/* Various functions and predicates for lisp types and equality

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
#ifndef LISP_TYPES_H
#define LISP_TYPES_H
sexp lisp_arrayp(sexp obj);
sexp lisp_consp(sexp obj);
sexp lisp_numberp(sexp obj);
sexp lisp_bigintp(sexp obj);
sexp lisp_bigfloatp(sexp obj);
sexp lisp_bignump(sexp obj);
sexp lisp_subrp(sexp obj);
sexp lisp_intp(sexp obj);
sexp lisp_integerp(sexp obj);
sexp lisp_nilp(sexp obj);
sexp lisp_sequencep(sexp obj);
sexp lisp_streamp(sexp obj);
sexp lisp_stringp(sexp obj);
sexp lisp_hashtablep(sexp obj);
sexp lisp_realp(sexp obj);
sexp lisp_eq(sexp obj1,sexp obj2);
sexp lisp_identical(sexp obj1,sexp obj2);
sexp lisp_eql(sexp obj1,sexp obj2);
sexp lisp_equal(sexp obj1,sexp obj2);
sexp lisp_string_equal(sexp obj1,sexp obj2);
#endif /*LISP_TYPES_H*/
