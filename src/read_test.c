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
const struct sigaction action_object={.sa_handler=&handle_sigsegv};
const struct sigaction* restrict sigsegv_action=&action_object;
const struct sigaction sigaction_object={.sa_sigaction=&c_signal_handler};
const struct sigaction* restrict sig_action=&sigaction_object;
//mega hack
sexp eval(sexp expr,env_ptr env){
  return expr;
}
sexp read_only(sexp expr,env_ptr env){
  return expr;
}
int main(){
  #ifdef DEBUG
  debug_printf=default_debug_printf;
  CORD_debug_printf=default_CORD_debug_printf;
  #endif
  sigaction(SIGSEGV,sigsegv_action,NULL);  
  sigaction(SIGUSR1,sig_action,NULL);
  sigaction(SIGUSR2,sig_action,NULL);
  GC_set_all_interior_pointers(1);
  GC_set_handle_fork(1);
  GC_init();
#ifdef GC_REDIRECT_TO_LOCAL
  GC_thr_init();
#endif
  init_prims();
  //current_env=xmalloc(sizeof(struct environment));
  init_environment();
  //setup global lexer
  symbol *lisp_ans_ptr=xmalloc(sizeof(symbol));
  HERE();
  readline_repl(eval);
}
