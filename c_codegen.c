#include "codegen.h"
int tmp_counter=0;
static function_args* C_getFunctionArgs(sexp arglist,function_args* args);
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
      return "function";
    case _sym:
      return "symref";
    case _stream:
      return "FILE*";
    case _bigint:
      return "mpz_t*";
    case _bigfloat:
      return "mpfr_t*";
    case _array:
      switch(obj.meta){
      }
    default:
      return "void";
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
  register CORD retval="#include \"scilisp.h\"\n";
  return retval;
}
static const char* main_decl = "int main(int argc,char *argv[]){\n";
/* This is complicated, unsuprisingly */
CORD c_codegen_specials(sexp expr,CORD code);
CORD c_codegen_funcall(sexp expr,CORD code);
CORD c_codegen_expr(sexp expr);
CORD c_codegen_definiton(sexp expr);
CORD *c_code;
CORD c_declare;
CORD c_main;
int c_index;
static jmp_buf codegen_err;
static void handle_error(){
 longjmp(codegen_err,c_index);
}
CORD c_codegen(sexp ast){
  c_code=xmalloc(cons_length(ast).val.int64+2*sizeof(CORD));
  c_declare=c_code[0];
  c_main=c_code[1];
  c_declare=c_header();
  c_main=CORD_from_char_star(main_decl);
  c_index=1;
  sexp cur_sexp;
  int error_val;
  if(!(error_val=setjmp(codegen_err))){
    error_str=c_code[error_val];
  }
  while(CONSP(ast)){
    cur_sexp=XCAR(ast);
    ast=XCDR(ast);
    while(CONSP(cur_sexp)){
      if(SPECP(XCAR(cur_sexp))){
        c_codegen_specials(cur_sexp,c_code[1]);
      } else if (FUNCTIONP(XCAR(cur_sexp))){
    }
    return c_code[0];
  }
}
}
  //codegen for atoms
CORD c_codegen_sub(sexp expr){//this is basically eval for codegen
  CORD code="";
  switch(expr.tag){
    case _cons:
      switch(car(expr).tag){
        case _special:
          CORD_append(code,c_codegen_specials(expr,code));
          return code;
        case _sym:
          CORD_append(code,c_codegen_funcall(expr,code));
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
      register sexp Var=(sexp){.tag=_sym,.val={.var =getSym(topLevelEnv,temp)}};
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
CORD c_codegen_funcall(sexp expr,CORD code){
  CORD acc;
  if(!CONSP(expr) || !FUNCTIONP(XCAR(expr))){
    handle_error();
  }
  function *cur_fun=XCAR(expr).val.fun;
  acc=CORD_cat(cur_fun->cname,"(");
  C_getFunctionArgs(XCADR(expr),cur_fun->args);
  int i,maxargs=cur_fun->args->max_args;

  return "";
}
#define setArg()                                        \
  args->args[j++].val=XCAR(arglist);      \
  arglist=XCDR(arglist)
static function_args* C_getFunctionArgs(sexp arglist,function_args* args){
  /*  if(args.tag != _funargs){
    handle_error();
    }*/
  int i,j=0;
  for(i=0;i<args->num_req_args;i++){  
    if(!(CONSP(arglist))){
      format_error_str("not enough args");
      handle_error();
    } else {
      setArg();
    }
  }
  for(i=0;i<args->num_opt_args;i++){
    if(!CONSP(arglist)){
      goto ARGS_END;
    } else {
      setArg();
    }
  }
  for(i=0;i<args->num_keyword_args;i++){
    format_error_str("keyword args unimplemented");
    handle_error();
  }
  if(args->has_rest_arg){
    if(CONSP(arglist)){
      cons* prev_arg;
      cons* cur_arg=args->args[j].val.val.cons=xmalloc(sizeof(cons));
      args->args[j].val.tag=_list;
      while(CONSP(arglist)){
        j++;
        cur_arg->car=XCAR(arglist);
        arglist=XCDR(arglist);
        cur_arg->cdr.val.cons=xmalloc(sizeof(cons));
        prev_arg=cur_arg;
        cur_arg=cur_arg->cdr.val.cons;      
      }
      prev_arg->cdr=NIL;
    }
  }
  if(CONSP(arglist)){
    format_error_str("excess arguments passed");
    handle_error();
  }
 ARGS_END:
  if(j<args->max_args){
    while(j<args->max_args){
      args->args[j++].val=NIL;
    }
  }
  return args;
}
