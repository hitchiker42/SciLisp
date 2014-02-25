/* Functions that don't really fit anywhere else

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
//gc functions from lisp
sexp lisp_gc_collect(){
  GC_gcollect();
  return NIL;
}
sexp lisp_minimize_memory_usage(){
  GC_gcollect_and_unmap();
  return NIL;
}
sexp lisp_get_gc_info(){
  CORD gc_info=get_gc_info();
  return string_sexp(make_string(gc_info));
}

  
