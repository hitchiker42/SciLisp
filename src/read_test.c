#include "repl.c"
#include "prim.h"
#ifdef NDEBUG
int quiet_signals=1;
#else
int quiet_signals=0;
#endif
void handle_sigsegv(int signal) __attribute__((noreturn));
void handle_sigsegv(int signal){
  if(!quiet_signals){
#if defined(MULTI_THREADED)
    fprintf(stderr,
            "recieved segfault in thread number %ul, printing bactrace\n",
            pthread_self());
#else
    fprintf(stderr,"recieved segfault, printing bactrace\n");
#endif
    print_trace();
  }
  exit(1);
}
const struct sigaction sigsegv_action={.sa_handler=handle_sigsegv};
const struct sigaction *sigsegv_action_pointer=&sigsegv_action;
sexp read_only(sexp expr,env_ptr env){
  return expr;
}
int main(){
  quiet_signals=0;
  init_signal_handlers();
  sigaction(SIGSEGV,sigsegv_action_pointer,NULL);
  GC_set_handle_fork(1);
  GC_INIT();
  init_prims();
  //current_env=xmalloc(sizeof(struct environment));
  init_environment();
  //setup global lexer
  symbol *lisp_ans_ptr=xmalloc(sizeof(symbol));
  HERE();
  /*#ifdef HAVE_READLINE
  readline_repl(eval);
  #else*/
  simple_repl(eval);
  //#endif
}
