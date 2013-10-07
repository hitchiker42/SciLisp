/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
c_string tag_name(_tag obj_tag);
c_string typeName(sexp obj);
CORD print_num_format(sexp obj,CORD format);
CORD print_num(sexp obj);
CORD print(sexp obj);
sexp lisp_print(sexp obj);
