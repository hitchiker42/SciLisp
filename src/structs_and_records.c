/* Lisp Structures and Records

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
//Perhaps I should have distinct struct and record types
#include "common.h"
#include "cons.h"
//fairly naive implementation of a simple record type
//records are basically just assoicative arrays, but allow
//having a collection of values with names attached
//I could make a reader extension to read #r(record &data ((field [value)*)))

struct lisp_record {
  symbol *fields;
  sexp *values;
  uint32_t num_fields;
};
//this is O(N) asymptotically, but can be translated to an array index
//at complie time(sometimes), and should be fast enough for a low N
//An obviously faster solution would be to use a hash table, which
//would be slower for smaller n and be much larger
sexp get_record_field(sexp record,sexp field_name){
  //typecheck
  lisp_record *r=record.val.opaque;
  int i;
  for(i=0;i<r->num_fields;i++){
    if(EQ(r->fields[i],field_name.val.sym)){
      return r->values[i];
    }
  }
  return NIL;
}
sexp set_record_field(sexp record,sexp field_name,sexp value){
  //typecheck
  lisp_record *r=record.val.opaque;
  int i;
  for(i=0;i<r->num_fields;i++){
    if(EQ(r->fields[i],field_name.val.sym)){
      r->values[i]=value;
      return r->values[i];
    }
  }
  return NIL;
}
//since get_record_field returns NIL on a nonexistant field and on a field
//with an actual value NIL we need this to see weather or not
//a record has a certain field
sexp has_record_field(sexp record,sexp field_name){
  //typecheck
  lisp_record *r=record.val.opaque;
  int i;
  for(i=0;i<r->num_fields;i++){
    if(EQ(r->fields[i],field_name.val.sym)){
      return LISP_TRUE;
    }
  }
  return LISP_FALSE;
}
//(make-record &rest fields); fields are (name [value])
sexp make_record(uint32_t numargs,sexp *args){
  lisp_record *r=xmalloc(sizeof(lisp_record));
  r->fields=xmalloc(sizeof(symbol)*numargs);
  r->values=xmalloc(sizeof(sexp)*numargs);
  r->num_fields=numargs;
  int i;
  for(i=0;i<numargs;i++){
    r->fields[i]=XCAR(args[i]);
    r->values[i]=(CONSP(XCDR(args[i]))?XCADR(args[i]):UNBOUND);
  }
  return record_sexp(r);
}
