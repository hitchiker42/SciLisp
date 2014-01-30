#include "repl.c"
sexp read_only(sexp expr,env_ptr env){
  return expr;
}
int main(){
  eval_fun=read_only;
  symbol *lisp_ans_ptr=xmalloc(sizeof(symbol));
  read_eval_print_loop();
}
