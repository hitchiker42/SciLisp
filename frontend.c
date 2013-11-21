/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "gc/include/gc/gc.h"
#include "prim.h"
#include "lex.yy.h"
#include "print.h"
#include "codegen.h"
#include <sys/mman.h>
#define HAVE_READLINE
#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif
jmp_buf main_loop,error_buf;
static c_string current_version="0.01";
static c_string banner=
  "SciLisp  Copyright (C) 2013  Tucker DiNapoli\n"
  "SciLisp is free software licensed under the GNU GPL V3+";
static c_string SciLisp_Banner;
static int no_banner=0;
static int no_copyright=0;
static struct timespec timer_struct={.tv_sec=0,.tv_nsec=10000};
#if defined(MULTI_THREADED)
static int pthread_prims_initialized=0;
static pthread_mutex_t init_prims_mutex=PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t init_prims_cond=PTHREAD_COND_INITIALIZER;
static void* initPrims_pthread(void*);
static void ensure_prims_initialized();
static void* SciLisp_getopt_pthread(void *getopt_args);
#define GC_REDIRECT_TO_LOCAL
#define ENSURE_PRIMS_INITIALIZED() ensure_prims_initialized()
#else
#define ENSURE_PRIMS_INITIALIZED()
#endif
#ifdef NDEBUG
int quiet_signals=1;
#else
int quiet_signals=0;
#endif
int evalError=0;
static char *line_read;
static void SciLisp_getopt(int argc,char *argv[]);
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
static void SciLisp_help(int exitCode) __attribute__((noreturn));
static void SciLisp_version(int exitCode) __attribute__((noreturn));
const struct sigaction action_object={.sa_handler=&handle_sigsegv};
const struct sigaction* restrict sigsegv_action=&action_object;
#define reset_line()       free (line_read);    \
  line_read = (char *)NULL
extern FILE* yyin;
/*Scan a line, subtract 1 from parens for each ")"
 *add 1 to parens for each "(", return -1 if we find a close parenteses
 *without an opening one.
 *parens is an int containing the number of currently open parentheses
 *it could probably be a pointer, but I like to keep functions pure
 *(HA, I wrote that a while ago, but then I added the pure attribute)
 */
int parens_matched(const char* line,int parens)__attribute__((pure));
int parens_matched(const char* line,int parens){
  int i=0;
  char cur_char;
  while((cur_char=line[i]) != '\0'){
    i++;
    if(cur_char==';'){return parens;}
    else if(cur_char=='('){parens++;}
    else if(cur_char==')'){
      if(parens>0){parens--;}
      else{return -1;}
    }
    else{continue;}
  }
  return parens;
}
/*read input from file input, parse it into an abstract syntax tree,
 *write c code to c_code file(generate a tmp file if c_code == NULL)
 *and compile the c file with gcc to output file output*/
int compile(FILE* input,const char *output,FILE* c_code) __attribute__((noreturn));
int compile(FILE* input,const char *output,FILE* c_code){
  HERE();
  CORD generated_code;
  if(setjmp(error_buf)){
    PRINT_MSG("jumped to error");
    fputs("compilation error\n",stderr);
    exit(-1);
  } else {
    sexp ast=yyparse(input);
    if(NILP(ast)){
      CORD_printf("parsing failed exiting compiler\n");
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
    char tmpFilename[L_tmpnam];
    tmpnam_r(tmpFilename);
    FILE* tmpFile=fopen(tmpFilename,"w");
    CORD_put(generated_code,tmpFile);
    char* cc_command;
    //need to fix this eventually
    asprintf(&cc_command,
             "gcc -o %s -O2 -g %s libSciLisp.so -wl,-rpath=$PWD -lgc -lm -lcord -lgmp -lmpfr",
             output,tmpFilename);
    int retval=system(cc_command);
    exit(retval);
  }
}
/* Read interactive input using readline.
 * outfile is a temporary file essentally used as a pipe. read scans a line
 * and if that line has an open paren without a matching close paren read
 * will continue to scan lines unitl all parentese are matched, or it finds
 * a syntax error(really just an extra close paren).
 * input is written to outfile as a place to hold the current input. It is
 * assumed that code will be read from outfile and evaluated. Read returns
 * the position in outfile where it started scanning*/
int lispReadLine(FILE* outfile,char* filename){
  FILE* my_pipe=outfile;
  char* tmpFile=filename;
  int parens,start_pos=ftello(my_pipe);
 MAIN_LOOP:while(1){
    parens=0;
    //makesure readline buffer is NULL
    if (line_read){
      free (line_read);
      line_read = (char *)NULL;
    }
    line_read = readline("SciLisp>");
    if (line_read){
      if (*line_read){
        add_history (line_read);
      } else {goto MAIN_LOOP;}
    } else {puts("\n");exit(0);}
    parens=parens_matched(line_read,0);
    fputs(line_read,my_pipe);
    fputc(' ',my_pipe);
    while(parens){
      if(parens<0){
        fprintf(stderr,"Extra close parentheses\n");
        truncate(tmpFile,0);
        goto MAIN_LOOP;
      }
      line_read=readline(">");
      if(line_read == NULL){
        puts("");
        evalError=1;
        goto MAIN_LOOP;
      }
      if (line_read && *line_read){
        add_history (line_read);
      }
      //puts(line_read);
      parens=parens_matched(line_read,parens);
      fputs(line_read,my_pipe);
      fputc(' ',my_pipe);
    }
    fflush(my_pipe);
    break;
  }
  return start_pos;
}
//input w/o readline
int lispGetLine(FILE* outfile,char* filename){
  FILE* my_pipe=outfile;
  char* tmpFile=filename;
  int parens,start_pos=ftello(my_pipe);
  size_t len;
 MAIN_LOOP:while(1){
    parens=0;
    //makesure readline buffer is NULL
    if (line_read){
      free (line_read);
      line_read = (char *)NULL;
    }
    fputs("SciLisp>",stdout);
    len = getline(&line_read,NULL,stdin);
    if (line_read){
      goto MAIN_LOOP;
    } else {puts("\n");exit(0);}
    parens=parens_matched(line_read,0);
    if(line_read[len-1] == '\n'){
      line_read[len-1] = ' ';
    }
    fputs(line_read,my_pipe);
    while(parens){
      if(parens<0){
        fprintf(stderr,"Extra close parentheses\n");
        truncate(tmpFile,0);
        goto MAIN_LOOP;
      }
      fputs(">",stdout);
      len=getline(&line_read,NULL,stdin);
      if(line_read == NULL){
        puts("");
        evalError=1;
        goto MAIN_LOOP;
      }
      parens=parens_matched(line_read,parens);
      if(line_read[len-1] == '\n'){
        line_read[len-1] = ' ';
      }
      fputs(line_read,my_pipe);
    }
    fflush(my_pipe);
    break;
  }
  return start_pos;
}
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
  GC_init();
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
  PRINT_FMT("thread number %lu",pthread_self());
#else
  initPrims();
  SciLisp_getopt(argc,argv);
#endif
  sexp(*evalFun)(sexp,env*)=eval;
  static char *line_read =(char *)NULL;
  int parens,start_pos;
  char tmpFile[L_tmpnam];
  tmpnam_r(tmpFile);
  FILE* my_pipe=fopen(tmpFile,"w+");
  yyin=my_pipe;
  #ifdef HAVE_READLINE
  rl_set_signals();
  rl_variable_bind("blink-matching-paren","on");
  #endif
#ifdef MULTI_THREADED
  pthread_join(initPrims_thread,NULL);
  pthread_join(getopt_thread,NULL);
  pthread_mutex_destroy(&init_prims_mutex);
#endif
  if(!no_banner){
    puts(SciLisp_Banner);
  }
  if(!no_copyright){
    puts(banner);
  }
  sexp ast,result;
 REPL:while(1){
    if(setjmp(error_buf)){
      PRINT_MSG("jumped to error");
      //printf(error_str);
    }
    if(evalError){
      truncate_and_rewind(my_pipe,tmpFile);
      evalError=0;
      yyrestart(yyin);
    }
    //try to parallize this for practice
    //3 threads, one to read, one to parse and one to eval and print
    //read
    #ifdef HAVE_READLINE
    start_pos=lispReadLine(my_pipe,tmpFile);
    #else
    start_pos=lispGetLine(my_pipe,tmpFile);
    #endif
    fseeko(my_pipe,start_pos,SEEK_SET);
    //eval;
    ast=yyparse(my_pipe);
    //print
    if(!NILP(ast)){
      result=evalFun(XCAR(ast),topLevelEnv);
      CORD_printf(print(result));puts("\n");
    } else {
      result=NIL;
    }
  }
}
static CORD Make_SciLisp_verson_string(c_string Version_no){
  CORD version_string;
  CORD_sprintf(&version_string,"SciLisp %s",Version_no);
  return version_string;
}
static CORD Make_SciLisp_Copyright_string(){
  CORD retval;
  CORD_sprintf(&retval,"Copyright %lc 2013 Tucker DiNapoli\n"
               "License GPLv3+: GNU GPL version 3 or "
               "later <http://gnu.org/licenses/gpl.html>.",0x00A9);
  return retval;
}
static CORD Make_SciLisp_help_string(){
  CORD copyright_string=Make_SciLisp_Copyright_string();
  CORD version_string=Make_SciLisp_verson_string(current_version);
  CORD help_string=CORD_catn(4,version_string," ",copyright_string,"\n");
  help_string=CORD_cat
    (help_string,
     "SciLisp [-hqv] [-e|--eval] [-f|--file] [-l|--load] [-o|--output] [file]\n"
     "Options:\n"
     "eval|e [file|string], evaluate code in file or double quote delimited string\n"
     "help|h, print this help and exit\n"
     "load|l [file], eval code from file and start interpreter\n"
     "output|o [file], output compiled code to file, requires a file to compile\n"
     "test|t, run tests contatined in test.lisp and print results\n"
     "version|v, print version number and exit\n");
     // "quiet|q, insure debug messages are not printed (redirect to /dev/null)\n");
  return help_string;
}
static void SciLisp_help(int exitCode){
  CORD_printf(Make_SciLisp_help_string());
  exit(exitCode);
}
static void SciLisp_version(int exitCode){
  puts(Make_SciLisp_verson_string(current_version));
  exit(exitCode);
}
/*just to note I didn't write this I got it from
  http://patorjk.com/software/taag/#p=display&f=Small%20Slant&t=SciLisp*/
static c_string SciLisp_Banner=
"    ____      _  __    _          \n"
"   / __/____ (_)/ /   (_)___  ___ \n"
"  _\\ \\ / __// // /__ / /(_-< / _ \\\n"
" /___/ \\__//_//____//_//___// .__/\n"
"                           /_/     ";
#ifdef MULTI_THREADED
static void* initPrims_pthread(void* x __attribute__((unused))){
  PRINT_FMT("thread number %lu",pthread_self());
  initPrims();
  pthread_mutex_lock(&init_prims_mutex);
  pthread_prims_initialized=1;
  pthread_cond_broadcast(&init_prims_cond);
  pthread_mutex_unlock(&init_prims_mutex);
  pthread_cond_destroy(&init_prims_cond);
  return 0;
}
static void ensure_prims_initialized(){
  pthread_mutex_lock(&init_prims_mutex);
  while(!pthread_prims_initialized){
    HERE();
    pthread_cond_wait(&init_prims_cond,&init_prims_mutex);
  }
  pthread_mutex_unlock(&init_prims_mutex);
}
static void* SciLisp_getopt_pthread(void *getopt_args){
  PRINT_FMT("thread number %lu",pthread_self());
  struct thread_args *args=(struct thread_args *)getopt_args;
  int argc=args->argc;
  char **argv=args->argv;
  SciLisp_getopt(argc,argv);
  return 0;
}
#endif
static void SciLisp_getopt(int argc,char *argv[]){
  int c;
  while(1){
    c=getopt_long(argc,argv,"e:hl:o:qvtb:nr",long_options,NULL);
    if(c==-1){break;}
    switch(c){
      case 'o':
        output_file=optarg;
        break;
      case 'v':
        SciLisp_version(0);
      case 'h':
        SciLisp_help(0);
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
        }
        ENSURE_PRIMS_INITIALIZED();
        if(setjmp(error_buf)){
          printf("parsing failed exiting\n");
          exit(1);
        }
        ast=yyparse(file);
        while(CONSP(ast)){
          if(setjmp(error_buf)){
            PRINT_MSG("jumped to error");
            ast=XCDR(ast);
            continue;
            //printf(error_str);
          }
          CORD_printf("evaluating: %r\n",print(XCAR(ast)));
          sexp result=eval(XCAR(ast),topLevelEnv);
          CORD_printf("result: %r\n",print(result));
          ast=XCDR(ast);
        }
        exit(0);
      }
      case 'l':{
        sexp ast;
        FILE* file=fopen(optarg,"r");
        ENSURE_PRIMS_INITIALIZED();
        ast=yyparse(file);
        while (CONSP(ast)){
          sexp result=eval(XCAR(ast),topLevelEnv);
          ast=XCDR(ast);
        };
        break;
      }
      case 'r':{
        int tests_failed=0;
        CORD failed_exprs="";
        CORD_debug_printf=CORD_ndebug_printf;
        debug_printf=ndebug_printf;
        FILE* file=fopen("test.lisp","r");
        int my_stdout_fd=dup(STDOUT_FILENO);        
        freopen("/dev/null","w",stdout);
        freopen("/dev/null","w",stderr);
        ENSURE_PRIMS_INITIALIZED();
        sexp ast=yyparse(file);
        while (CONSP(ast)){
          sexp result=eval(XCAR(ast),topLevelEnv);
          if(ERRORP(result)){
            tests_failed++;
            failed_exprs=CORD_catn(3,failed_exprs,print(XCAR(ast)),"\n");
          }
          ast=XCDR(ast);
        }
        FILE* my_stdout=fdopen(my_stdout_fd,"w");
        if(!tests_failed){
          fprintf(my_stdout,"all tests passed\n");
          exit(0);
        } else {
          printf("Failed tests:\n");
          CORD_printf(failed_exprs);
          exit(tests_failed);
        }
      }
      case 't':{
        CORD_debug_printf=CORD_ndebug_printf;
        debug_printf=ndebug_printf;
        FILE* file=fopen("test.lisp","r");
        ENSURE_PRIMS_INITIALIZED();
        sexp ast=yyparse(file);
        puts("Testing:");
        while (CONSP(ast)){
          CORD_printf(CORD_cat("evaluating: ",print(XCAR(ast))));
          puts("");
          sexp result=eval(XCAR(ast),topLevelEnv);
          CORD_printf(CORD_cat("result: ",print(result)));
          puts("");
          ast=XCDR(ast);
        }
        exit(0);
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
    ENSURE_PRIMS_INITIALIZED();
    compile(file,output_file,NULL);
  } else if (output_file){
    printf("error: -o|--output requires a file of SciLisp code to compile\n");
    exit(2);
  }
}
