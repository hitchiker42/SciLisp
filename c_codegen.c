#include "codegen.h"
int tmp_counter=0;
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

#define get_c_char(c) (isalnum(c)?c:'_') 
CORD to_c_symbol(CORD symName){
  char* c_name=CORD_to_char_star(symName);
  int i;
  for(i=0;i<CORD_len(symName);i++){
    c_name[i]=get_c_char(c_name[i]);
  }
  CORD retval;
  CORD_sprintf(&retval,"%s__%d__",c_name,tmp_counter++);
  return retval;
}
//not much now, but if I need to expand the header this will make it easy
static inline CORD c_header(){
  register CORD retval="#include \"common.h\"\n";
  return retval;
}
/* This is complicated, unsuprisingly */
CORD c_codegen_specials(sexp expr,CORD code);
CORD c_codegen_functions(sexp expr,CORD code);
CORD c_codegen_sub(sexp expr);
CORD c_code;
CORD declarations;
CORD c_codegen(sexp ast){
  initPrims();
  c_code=c_header();
  sexp cur_block=car(ast);
  while(CONSP(cur_block)){
    switch(car(cur_block).tag){
      case _special://for now things need to be special forms, either main, or defines
        CORD_append(c_code,c_codegen_specials(cur_block,c_code));
    }
  }
  return c_code;
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
    case _setq:
    case _def:
      if(!SYMBOLP(cadr(expr))){
        CORD_sprintf(&error_str,"can not define %s, it is not a symbol",
                     print(cadr(expr)));
        handle_error();
      }
      register CORD temp=(cadr(expr).val.var->name);
      register sexp Var=(sexp){.tag=_sym,.val={.var =getSym(&topLevelEnv,temp)}};
      if(Var.val.var){
        //standard lisp behavior, define won't redefine stuff
        if(car(expr).val.special == _def){
          return "";
        }
      }
      //add Var to top level env
      temp=CORD_cat("sexp ",
                    CORD_cat(to_c_symbol(temp),
                             CORD_cat(" = ",c_codegen_sub(cddr(expr)))));      
      return temp;
    default:
      return "";
  }
}
CORD c_codegen_functions(sexp expr,CORD code){
  return "";
}
