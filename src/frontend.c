/* Actual SciLisp program (launches repl, or compiles a file)

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
#include "frontend.h"
#include "prim.h"
#include "print.h"
#include "codegen.h"
#incldue "frame.h"
#define DEFAULT_LISP_HIST_SIZE 100
#if defined(MULTI_THREADED)
static void* initPrims_pthread(void*);
static void* SciLisp_getopt_pthread(void *getopt_args);
#define ENSURE_PRIMS_INITIALIZED() (void)pthread_once(&pthread_prims_initialized,initPrims)
#else
#define ENSURE_PRIMS_INITIALIZED()
#endif
#ifdef NDEBUG
int quiet_signals=1;
#else
int quiet_signals=0;
#endif
static struct timespec timer_struct={.tv_sec=0,.tv_nsec=10000};
static FILE *logfile;
static long lisp_hist_size=-1;
static sexp *lisp_history;
static char *line_read;
static void SciLisp_getopt(int argc,char *argv[]);
static sexp eval_log(sexp expr,env *cur_env);
static void inferior_scilisp();// __attribute__((noreturn));
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
static void fastforward(FILE* stream){
  fflush(stream);
  fseeko(stream,0,SEEK_END);
}
static void truncate_and_rewind(FILE* stream,c_string name){
  truncate(name,0);
  rewind(stream);
}
const struct sigaction action_object={.sa_handler=&handle_sigsegv};
const struct sigaction* restrict sigsegv_action=&action_object;
#define reset_line()       free (line_read);    \
  line_read = (char *)NULL
//extern FILE* yyin;
/*read input from file input, parse it into an abstract syntax tree,
 *write c code to c_code file(generate a tmp file if c_code == NULL)
 *and compile the c file with gcc to output file output*/
int compile(FILE* input,const char *output,FILE* c_code) __attribute__((noreturn));
int compile(FILE* input,const char *output,FILE* c_code){
  HERE();
  CORD generated_code;
  //    sexp ast=yyparse(input);
  yyscan_t scanner;
  yylex_init(&scanner);
  sexp *yylval=xmalloc(sizeof(sexp));
  sexp ast=c_read_safe(&scanner,yylval);
  if(NILP(ast)){
    CORD_printf("Reading failed exiting compiler\n");
    exit(1);
  }
  CORD_printf("%r\n",print(XCAR(ast)));

  if(setjmp(error_buf)){
    generated_code=codegen(ast,0);
  } else {
    fprintf(stderr,"compile error, current code:\n");
    CORD_fprintf(stderr,error_str);
    exit(5);
  }
  char tmpFilename[]={'/','t','m','p','/','t','m','p',
                      'X','X','X','X','X','X','.','c','\0'};
  int fd=mkstemps(tmpFilename,2);
  FILE* tmpFile=fdopen(fd,"w");
  CORD_put(generated_code,tmpFile);
  char* cc_command;
  CORD cc_command_CORD;
  //need to fix this eventually
  CORD_sprintf(&cc_command_CORD,
               "gcc -o %s -O2 -g %s -lSciLisp -Wl,-rpath=$PWD",
               output,tmpFilename);
  cc_command=CORD_to_char_star(cc_command_CORD);
  int retval=system(cc_command);
  exit(retval);
}
//kinda an abuse of include, but it makes this more maintainable
#include "repl.c"
struct thread_args {
  int argc;
  char **argv;
};
int main(int argc,char* argv[]){
  //setup handler for sigsegv, so we can exit gracefully on a segfault
  #ifdef DEBUG
  debug_printf=default_debug_printf;
  CORD_debug_printf=default_CORD_debug_printf;
  #endif
  sigaction(SIGSEGV,sigsegv_action,NULL);
  //allocate signal stack before gc init so gc doesn't have to worry about it
  init_sigstk();
  GC_set_all_interior_pointers(1);
  GC_set_handle_fork(1);
  GC_init();
#ifdef GC_REDIRECT_TO_LOCAL
  GC_thr_init();
#endif
  //setup global lexer
  yylex_init(&global_scanner);
#if defined (MULTI_THREADED)
  pthread_t initPrims_thread;
  pthread_t getopt_thread;
  if((pthread_create(&initPrims_thread,NULL,initPrims_pthread,NULL))){
    perror("Program error, exiting:\nthread creation failed");
    exit(4);
  }
  struct thread_args *getopt_args=xmalloc(sizeof(struct thread_args));
  getopt_args->argc=argc;
  getopt_args->argv=argv;
  //  initPrims(); //read primitives into symbol table
  if((pthread_create(&getopt_thread,NULL,SciLisp_getopt_pthread,getopt_args))){
    perror("Program error, exiting:\nthread creation failed");
    exit(4);
  }
  //  PRINT_FMT("thread number %lu, Main Thread",pthread_self());
#else
  init_prims();
  SciLisp_getopt(argc,argv);
#endif
  static char *line_read =(char *)NULL;
  int parens,start_pos;
  char tmpFile[]={'/','t','m','p','/','S','c','i','L','i','s','p','_','P','i','p','e',
                  'X','X','X','X','X','X','\0'};
  int fd=mkstemp(tmpFile);
  FILE* my_pipe=fdopen(fd,"w+");
  yyscan_t scanner;
  yylex_init(&scanner);
  yyset_in(my_pipe,scanner);
  //  yyin=my_pipe;
  #ifdef HAVE_READLINE
  rl_set_signals();
  rl_variable_bind("blink-matching-paren","on");
  #endif
#ifdef MULTI_THREADED
  pthread_join(initPrims_thread,NULL);
  pthread_join(getopt_thread,NULL);
#endif
  if(!no_banner){
    puts(SciLisp_Banner);
  }
  if(!no_copyright){
    puts(banner);
  }
  if(lisp_hist_size==-1){
    lisp_hist_size=DEFAULT_LISP_HIST_SIZE;
  }
  lisp_history=xmalloc(sizeof(sexp)*lisp_hist_size);
  if(!evalFun){
    evalFun=eval;
  }
  read_eval_print_loop();
}
/*
  sexp ast;
  //toplevel handler which catches any invalid nonlocal exit
  //also used to return to after any fatal lisp error (i.e
  //the lisp stack overflows or something
  frame top_level_frame=make_frame(UNWIND_PROTECT_TAG,protect_frame);
  push_frame(&top_level_frame);
 REPL:while(1){
    //if(setjmp(top_level_frame->dest)){}
    if(setjmp(error_buf)){
      PRINT_MSG("jumped to error");
      //printf(error_str);
      //yyrestart(yyin);
      yyrestart(my_pipe,scanner);
    }
    if(evalError){
      truncate_and_rewind(my_pipe,tmpFile);
      evalError=0;
      //yyrestart(yyin);
      yyrestart(my_pipe,scanner);
    }
    //try to parallize this for practice..maybe, maybe not
    //3 threads, one to read, one to parse and one to eval and print
    //read
    #ifdef HAVE_READLINE
    start_pos=lispReadLine(my_pipe,tmpFile);
    #else
    start_pos=lispGetLine(my_pipe,tmpFile);
    #endif
    fseeko(my_pipe,start_pos,SEEK_SET);
    //eval;
    //    ast=yyparse(my_pipe);
    ast=yyparse(my_pipe,scanner);
    //print
    if(!NILP(ast)){
      lisp_ans_ptr->val=evalFun(XCAR(ast),topLevelEnv);
      CORD_printf(CORD_cat(print(lisp_ans_ptr->val),"\n"));
    } else {
      ;
    }
  }
}
*/
#ifdef MULTI_THREADED
static void* initPrims_pthread(void* x __attribute__((unused))){
  //  PRINT_FMT("thread number %lu, initPrims thread",pthread_self());
  (void)pthread_once(&pthread_prims_initialized,initPrims);
  return 0;
}
static void* SciLisp_getopt_pthread(void *getopt_args){
  //  PRINT_FMT("thread number %lu, Getopt thread",pthread_self());
  struct thread_args *args=(struct thread_args *)getopt_args;
  int argc=args->argc;
  char **argv=args->argv;
  SciLisp_getopt(argc,argv);
  return 0;
}
#endif
//aviable command line options, struct args: 1st arg option name
//2nd arg=enum{no_argument=0,required_argument=1,optional_argument=2}
//3rd arg can be set to a variable adress to allow that argument to set it
//for any long argument with a corrsponding short option this must be 0
//4th arg is the value to load into 3rd arg if it is not null otherwise
//it should be set to the equivlant short option
//Ideas for options;
//noprint, run a read-eval loop instead of a read-eval-print loop
//script, ignore #! line, turn off banner/copyright and debugging
static struct option long_options[];
static struct option long_options[] = {
  {"backend"   ,1,0,'b'},
  {"eval"      ,1,0,'e'},
  {"debug-test",0,0,'d'},
  {"help"      ,0,0,'h'},
  {"inferior"  ,0,0,'i'},
  {"load"      ,1,0,'l'},
  {"no-debug"  ,0,0,'n'},
  {"output"    ,1,0,'o'},
  {"quiet"     ,0,0,'q'},
  {"regression",2,0,'r'},
  {"test"      ,2,0,'t'},
  {"version"   ,0,0,'v'},
  {0,0,0,0}
};
static inline int discard_script_header(FILE* file){
  //easy way
  //'#'=0x23,'!'=0x21, read as a unsinged int
  //will be 0x2123 (because of endianess)
  uint16_t shebang=0x2123;
  uint16_t test;
  fread(&test,sizeof(uint16_t),1,file);
  if(test==shebang){
    char newline_test;
    //skip rest of line (I don't know a better way to do this while discarding the line)
    while ((newline_test=getc(file)) && newline_test !='\n');
    return 1;
  } else {
    fseek(file,0,SEEK_SET);
    return 0;
  }
}
static void SciLisp_getopt(int argc,char *argv[]){
  int c;
  while(1){
    c=getopt_long(argc,argv,"b:de:hl:no:qr::vt::",long_options,NULL);
    if(c==-1){break;}
    switch(c){
      case 'o':
        output_file=optarg;
        break;
      case 'v':
        SciLisp_version(0);
      case 'h':
        SciLisp_help(0);
      case 'q':
        no_banner=1;
        no_copyright=1;
        //fallthrough
      case 'n':
        CORD_debug_printf=CORD_ndebug_printf;
        debug_printf=ndebug_printf;
        break;
      case 'e':{
        sexp ast;
        FILE* file;
        if(optarg[0]=='('){
          file=fmemopen(optarg,strlen(optarg),"r");
        } else {
          file=fopen(optarg,"r");
          if(!file){
            perror("failed to open file");
            exit(1);
          }
          discard_script_header(file);//ignore #! line
        }
        ENSURE_PRIMS_INITIALIZED();
        if(setjmp(error_buf)){
          fprintf(stderr,"parsing failed exiting\n");
          exit(1);
        }        
        yyscan_t scanner;
        yylex_init(&scanner);
        //        ast=yyparse(file);
        ast=yyparse(file,scanner);
        error_buf;
        //PRINT_MSG(print(ast));
        while(CONSP(ast)){
          if(setjmp(error_buf)){
            PRINT_MSG("jumped to error");
            ast=XCDR(ast);
            continue;
            //printf(error_str);
          }
          HERE();
          CORD_printf("evaluating: %r\n",print(XCAR(ast)));
          sexp result=eval(XCAR(ast),topLevelEnv);
          CORD_printf("result: %r\n",print(result));
          ast=XCDR(ast);
        }
        exit(0);
      }
      case 'i':{
        inferior_scilisp();
      }
      case 'l':{
        sexp ast;
        FILE* file=fopen(optarg,"r");
        ENSURE_PRIMS_INITIALIZED();
        CORD_debug_printf=CORD_ndebug_printf;
        debug_printf=ndebug_printf;
        ast=yyparse(file,global_scanner);
        //ast=yyparse(file);
        while (CONSP(ast)){
          sexp result=eval(XCAR(ast),topLevelEnv);
          ast=XCDR(ast);
        };
        CORD_debug_printf=default_CORD_debug_printf;
        debug_printf=default_debug_printf;
        break;
      }
      case 'r':{
        int tests_failed=0;
        CORD failed_exprs=0;
        CORD_debug_printf=CORD_ndebug_printf;
        debug_printf=ndebug_printf;
        FILE* file;
        CORD filename;
        if(optarg){
          file=fopen(optarg,"r");
          filename=CORD_from_char_star(optarg);
        } else if (argv[optind] && argv[optind][0] != '-'){
          file=fopen(argv[optind],"r");
          filename=CORD_from_char_star(argv[optind]);
        } else {
          file=fopen("test.lisp","r");
          filename="test.lisp";
        }
        if(!file){
          CORD_fprintf(stderr,"File %r not found, exiting.\n",filename);
          exit(EXIT_FAILURE);
        }
        discard_script_header(file);
        int my_stdout_fd=dup(STDOUT_FILENO);
        int my_stderr_fd=dup(STDERR_FILENO);
        freopen("/dev/null","w",stdout);
        freopen("/dev/null","w",stderr);
        ENSURE_PRIMS_INITIALIZED();
        FILE* my_stderr=fdopen(my_stderr_fd,"w");
        if(setjmp(error_buf)){
          fprintf(my_stderr,"parsing failed exiting\n");
          exit(1);
        }
        yyscan_t scanner;
        yylex_init(&scanner);
        //sexp ast=yyparse(file);
        sexp ast=yyparse(file,scanner);
        sexp result;
        while (CONSP(ast)){
          if(setjmp(error_buf)){
            tests_failed++;
            failed_exprs=CORD_catn(3,failed_exprs,print(result),"\n");
          } else {
            result=eval(XCAR(ast),topLevelEnv);
            if(ERRORP(result)){
              tests_failed++;
              failed_exprs=CORD_catn(5,failed_exprs,
                                     print(XCAR(ast)),"\nwith error:",
                                     print(result),"\n");
            }
          }
          ast=XCDR(ast);
        }
        FILE* my_stdout=fdopen(my_stdout_fd,"w");
        if(!tests_failed){
          fprintf(my_stdout,"all tests passed\n");
          exit(0);
        } else {
          fprintf(my_stderr,"Failed tests:\n");
          CORD_fprintf(my_stdout,failed_exprs);
          exit(tests_failed);
        }
      }
      case 't':{
        CORD_debug_printf=CORD_ndebug_printf;
        debug_printf=ndebug_printf;
        FILE* file;
        CORD filename;
        if(optarg){
          file=fopen(optarg,"r");
          filename=CORD_from_char_star(optarg);
        } else if (argv[optind] && argv[optind][0] != '-'){
          file=fopen(argv[optind],"r");
          filename=CORD_from_char_star(argv[optind]);
        } else {
          file=fopen("test.lisp","r");
          filename="test.lisp";
        }
        if(!file){
          CORD_fprintf(stderr,"File %r not found, exiting.\n",filename);
          exit(EXIT_FAILURE);
        }
        discard_script_header(file);
        ENSURE_PRIMS_INITIALIZED();
        if(setjmp(error_buf)){
          fprintf(stderr,"parsing failed exiting\n");
          exit(1);
        }
        fprintf(stderr,"before parse init\n");
        yyscan_t scanner;
        yylex_init(&scanner);
        //sexp ast=yyparse(file);
        fprintf(stderr,"before parse\n");
        sexp ast=yyparse(file,scanner);
        fprintf(stderr,"after parse\n");
        int exit_value=0;
        puts("Testing:");
        while (CONSP(ast)){
          CORD_printf(CORD_cat("evaluating: ",print(XCAR(ast))));
          puts("");
          sexp result=eval(XCAR(ast),topLevelEnv);
          if(ERRORP(result)){exit_value=99;}
          CORD_printf(CORD_cat("result: ",print(result)));
          puts("");
          ast=XCDR(ast);
        }
        exit(exit_value);
      }
      case 'b':
        switch(optarg[0]){
          case 'l':
            //            evalFun=llvmEvalJIT;
            //initialize_llvm();
          default:
            break;
        }
      default:
        printf("invalid option %c\n",c);
        SciLisp_help(1);
    }
  }
  if(optind < argc){
    output_file = (output_file == NULL?"a.out":output_file);
    FILE* file=fopen(argv[optind],"r");
    if(!file){
      perror("Error in fopen");
      fprintf(stderr,"File %s not found\n",argv[optind]);
      exit(EXIT_FAILURE);
    }
    ENSURE_PRIMS_INITIALIZED();
    compile(file,output_file,NULL);
  } else if (output_file){
    printf("error: -o|--output requires a file of SciLisp code to compile\n");
    exit(2);
  }
}
static sexp eval_log(sexp expr,env *cur_env){
  sexp retval=eval(expr,cur_env);
  CORD_fprintf(logfile,print(retval));
  return retval;
}
static void inferior_scilisp(){}
