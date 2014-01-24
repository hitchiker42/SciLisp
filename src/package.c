/* Functions to create and manipulate packages

   Copyright (C) 2014 Tucker DiNapoli

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
struct package {
  lisp_string name;
  lisp_string documentation;
  package *uses;
  obarray *symbol_table;
};
sexp make_package(sexp args){
  if(!CONSP(args)){
    raise_simple_error(Eargs,"Too few args passed to defpackage");
  }
  if(!SYMBOLP(XCAR(args))){
    raise_simple_error(Etype,"Expected a symbol");//make this better
  }
  package *new_package=xmalloc(sizeof(package));
  symbol *package_sym=POP(args).val.sym;
  if(package_sym->constant){
    raise_simple_error(Econst,"Cannot modify a constant value");
  }
  package_sym->val=new_package;
  while(CONSP(args)){
    //parse keyargs
  }
  return package_sexp(new_package);
}
    
