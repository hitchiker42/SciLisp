/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "cons.h"
#include <assert.h>
CORD c_codegen(sexp ast);
CORD codegen(FILE* outfile,sexp ast,enum backend backend){
  switch(backend){
    case 0:
      ;//WHAT?!?!, why is this necessary
      CORD c_code=c_codegen(ast);
      return c_code;
    default:
      return "";
  }
}
c_string get_cType(sexp obj){
  switch(obj.tag){
    case _cons:
      return "cons*";
    case _double:
      return "double";
    case _long:
      return "long";
    case _char:
      return "wchar_t";
    case _str:
      return "CORD";
    case _fun:
      return "fxn_proto";
    case _sym:
      return "symref";
  }
}

//not much now, but if I need to expand the header this will make it easy
static inline CORD c_header(){
  register CORD retval="#include \"common.h\"\n";
  return retval;
}
/* This is complicated, unsuprisingly */
#define CORD_append(val,ext) val=CORD_cat(val,ext)
CORD c_codegen_specials(sexp expr,CORD code);
CORD c_codegen_functions(sexp expr,CORD code);
CORD c_codegen_sub(sexp expr);
CORD c_codegen(sexp ast){
  register CORD code=c_header();
  sexp cur_block=car(ast);
  while(CONSP(cur_block)){
    switch(car(cur_block).tag){
      case _special://for now things need to be special forms, either main, or defines
        CORD_append(code,c_codegen_specials(cur_block,code));
    }
  }
  return code;
}
CORD c_codegen_sub(sexp expr){//this is basically eval for codegen
  CORD code="";
  switch(expr.tag){
    case _cons:
      switch(car(expr).tag){
        case _special:
          CORD_append(code,c_codegen_specials(expr,code));
          return code;
        case _sym:
          CORD_append(code,c_codegen_functions(expr,code));
          return code;
      }
    case _double:
      CORD_sprintf(&code,"%16.16lf",expr.val.real64);
      return code;
    case _long:
      CORD_sprintf(&code,"%ld",expr.val.int64);
      return code;
    case _sym:
      CORD_sprintf(&code,"%s",expr.val.var->name);
      return code;
    case _list:
      CORD_append(code,"mklist(");
      while(cdr(expr).tag != _nil){
        CORD_append(code,c_codegen_sub(car(expr)));
        CORD_append(code,",");
        expr=cdr(expr);
      }
      CORD_append(code,c_codegen_sub(car(expr)));
      CORD_append(code,",NIL);\n");
      return CORD_balance(code);
  }
}
CORD c_codegen_specials(sexp expr,CORD code){
  switch(car(expr).tag){
    case _main:
      CORD_append(code,"int main(int argc,char* argv[]){\n");
      assert(CONSP(cadr(expr)));
      CORD_append(code,c_codegen_sub(cdr(expr)));
      return code;
    case _def:;
      register CORD temp;
  }
}
CORD c_codegen_functions(sexp expr,CORD code){}
