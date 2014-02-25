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
static FILE *logfile;
static long lisp_hist_size=-1;
static sexp *lisp_history;
static sexp eval_log(sexp expr,env *cur_env);
static void inferior_scilisp();// __attribute__((noreturn));
static void load_file(const char *filename);
static const char *load_filename=NULL;
/*read input from file input into an abstract syntax tree,
 *write c code to c_code file(generate a tmp file if c_code == NULL)
 *and compile the c file with gcc to output file output*/
void __attribute__((noreturn))
compile(FILE* input,const char *output,FILE* c_code){
  HERE();
  init_environment();
  CORD generated_code;
  sexp ast=start_read(make_stream_input(input));
  if(NILP(ast)){
    fprintf(stderr,"Reading failed exiting compiler\n");
    exit(1);
  }
  PRINT_FMT("%r\n",print(XCAR(ast)));
  if(!setjmp(current_env->protect_frame->dest)){
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
  init_signal_handlers();
  //allocate signal stack before gc init so gc doesn't have to worry about it
  init_sigstk();
  GC_set_handle_fork(1);
  GC_INIT();
#if defined (MULTI_THREADED)
  pthread_t init_prims_thread,getopt_thread;
  pthread_create_checked(&init_prims_thread,NULL,init_prims_pthread,NULL);
  struct thread_args *getopt_args=xmalloc(sizeof(struct thread_args));
  getopt_args->argc=argc;
  getopt_args->argv=argv;
  pthread_create_checked(&getopt_thread,NULL,SciLisp_getopt_pthread,getopt_args);
#else
  init_prims();
  SciLisp_getopt(argc,argv);
#endif
  #ifdef HAVE_READLINE
  rl_set_signals();
  rl_variable_bind("blink-matching-paren","on");
  #endif
#ifdef MULTI_THREADED
  pthread_join(initPrims_thread,NULL);
  pthread_join(getopt_thread,NULL);
#endif
  //use fputs because the way I disable printing these is by setting the
  //variables to the empty string
  fputs(SciLisp_Banner,stdout);
  fputs(banner,stdout);
  if(lisp_hist_size==-1){
    lisp_hist_size=DEFAULT_LISP_HIST_SIZE;
  }
  lisp_history=xmalloc(sizeof(sexp)*lisp_hist_size);
  if(!evalFun){
    evalFun=eval;
  }
  init_environment();
  //delay this untill now to avoid initializing the environment in 
  //another thread(the option parsing thread)
  if(load_filename){
    load_file(load_filename);
  }
  read_eval_print_loop(eval_fun);
}
#ifdef MULTI_THREADED
static void* initPrims_pthread(void* x __attribute__((unused))){
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
  //(so this needs to be fixed to run on  bigendian)
  uint16_t shebang=0x2123;
  uint16_t test;
  fread(&test,sizeof(uint16_t),1,file);
  if(test==shebang){
    char newline_test;
    while ((newline_test=getc(file)) && newline_test !='\n');
    return 1;
  } else {
    fseek(file,0,SEEK_SET);
    return 0;
  }
}
//when loading a file we put off the actual work until we initialize the 
//environment, so we don't have to initialize an environment just 
//to load code
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
        banner="";
        SciLisp_Banner="";
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
        init_environment();
        ENSURE_PRIMS_INITIALIZED();
        if(load_filename){
          load_file(load_filename);
        }
        ast=read_full(make_stream_input(file));
        while(CONSP(ast)){
          if(setjmp(top_level_frame->dest)){
            PRINT_MSG("jumped to error");
            ast=XCDR(ast);
            continue;
            //printf(error_str);
          }
          HERE();
          CORD_printf("evaluating: %r\n",print(XCAR(ast)));
          sexp result=eval(XCAR(ast),current_env);
          CORD_printf("result: %r\n",print(result));
          ast=XCDR(ast);
        }
        exit(0);
      }
      case 'i':{
        inferior_scilisp();
      }
      case 'l':{
        load_filename=optarg;//assuming that optarg won't change
        /*
        sexp ast;
        FILE* file=fopen(optarg,"r");
        init_environment();
        ENSURE_PRIMS_INITIALIZED();
        CORD_debug_printf=CORD_ndebug_printf;
        debug_printf=ndebug_printf;
        ast=read(make_stream_input(file));
        while (CONSP(ast)){
          sexp result=eval(XCAR(ast),current_env);
          ast=XCDR(ast);
        };
        CORD_debug_printf=default_CORD_debug_printf;
        debug_printf=default_debug_printf;*/
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
          fprintf(stderr,"File %r not found, exiting.\n",filename);
          exit(EXIT_FAILURE);
        }
        discard_script_header(file);
        int my_stdout_fd=dup(STDOUT_FILENO);
        int my_stderr_fd=dup(STDERR_FILENO);
        freopen("/dev/null","w",stdout);
        freopen("/dev/null","w",stderr);
        init_environment();
        ENSURE_PRIMS_INITIALIZED();
        FILE* my_stderr=fdopen(my_stderr_fd,"w");
        sexp ast=read_full(make_stream_input(file));
        sexp result;
        top_level_frame=make_frame((uint64_t)UNWIND_PROTECT_TAG,
                                          unwind_protect_frame);
        push_frame(current_env,*top_level_frame);
        while (CONSP(ast)){
          if(setjmp(top_level_frame->dest)){
            tests_failed++;
            //this needs to be changed to use the error string in the top_level_frame
            failed_exprs=CORD_catn(3,failed_exprs,print(result),"\n");
          } else {
            result=eval(XCAR(ast),current_env);
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
        init_environment();
        ENSURE_PRIMS_INITIALIZED();
        sexp ast=read_full(make_stream_input(file));
        int exit_value=0;
        puts("Testing:");
        while (CONSP(ast)){
          if(setjmp(current_env->protect_frame->dest)){
            exit_value=99;
          } else {
            CORD_printf("evaluating: %r\n",print(XCAR(ast)));
            sexp result=eval(XCAR(ast),current_env);
            CORD_printf("result: %r\n",print(result));
          }
          ast=XCDR(ast);
        }
        exit(exit_value);
      }
      case 'b':
        switch(optarg[0]){
          case 'l':
            //evalFun=llvmEvalJIT;
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
static void load_file(const char *filename){
  sexp ast;
  FILE* file=fopen(filename,"r");
  CORD_debug_printf=CORD_ndebug_printf;
  debug_printf=ndebug_printf;
  ast=read_full(make_stream_input(file));
  while (CONSP(ast)){
    sexp result=eval(XCAR(ast),current_env);
    ast=XCDR(ast);
  };
  CORD_debug_printf=default_CORD_debug_printf;
  debug_printf=default_debug_printf;
  return;
}
static void inferior_scilisp(){}
