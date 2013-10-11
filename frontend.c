/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "common.h"
#include "prim.h"
#include "lex.yy.h"
#include "print.h"
#include "codegen.h"
#include <readline/readline.h>
#include <readline/history.h>
jmp_buf main_loop,ERROR;
#ifdef NDEBUG
int quiet_signals=1;
#else
int quiet_signals=0;
#endif
int evalError=0;
static char *line_read;
void handle_sigsegv(int signal) __attribute__((noreturn));
void handle_sigsegv(int signal){
  if(!quiet_signals){
    fprintf(stderr,"recieved segfault, printing bactrace\n");
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
  sexp ast=yyparse(input);
  if(NILP(ast)){
    CORD_printf("parsing failed exiting compiler\n");
    exit(1);
  }
  CORD_printf("%r\n",print(XCAR(ast)));
  //codegen(output,c_code,ast);
  exit(0);
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
        HERE();
        exit(0);
      }
      if (line_read && *line_read){
        add_history (line_read);
      }
      puts(line_read);
      parens=parens_matched(line_read,parens);
      fputs(line_read,my_pipe);
      fputc(' ',my_pipe);
    }
    fflush(my_pipe);
    break;
  }
  return start_pos;
}
int main(int argc,char* argv[]){
  //setup handler for sigsegv, so we can exit gracefully on a segfault
  sigaction(SIGSEGV,sigsegv_action,NULL);
  initPrims(); //read primitives into syntax table, this really should just
  // read a binary file containing a prepopulaed syntax table;
  int c;
  while(1){
    c=getopt_long(argc,argv,"o:e:",long_options,NULL);
    if(c==-1){break;}
    switch(c){
      case 'o':
        output_file=optarg;
      case 'e':{
        sexp ast ;
        FILE* file=fopen(optarg,"r");
        if(!file){
          ast=lispRead(optarg);
        } else {
          ast = yyparse(file);
        }
        while (CONSP(ast)){
          sexp result=eval(XCAR(ast),topLevelEnv);
          CORD_printf(print(result));puts("");
          ast=XCDR(ast);
        }
        exit(0);
      }
    }
  }
  if(optind < argc){
    FILE* file=fopen(argv[optind],"r");
    compile(file,output_file,NULL);
  }
  static char *line_read =(char *)NULL;
  int parens,start_pos;
  char tmpFile[L_tmpnam];
  tmpnam_r(tmpFile);
  char test[100];
  FILE* my_pipe=fopen(tmpFile,"w+");
  yyin=my_pipe;
  rl_set_signals();
  puts("SciLisp  Copyright (C) 2013  Tucker DiNapoli");
  sexp ast,result;
 REPL:while(1){
    fastforward(my_pipe);
    if(setjmp(ERROR)){printf(error_str);}
    if(evalError){
      truncate_and_rewind(my_pipe,tmpFile);evalError=0;
      yyrestart(yyin);
    }
    //read
    start_pos=lispReadLine(my_pipe,tmpFile);
    fseeko(my_pipe,start_pos,SEEK_SET);
    //eval;
    ast=yyparse(my_pipe);
    if(!NILP(ast)){
      result=eval(XCAR(ast),topLevelEnv);
    } else {
      result=NIL;
    }
    //print
    CORD_printf(print(result));puts("\n");
  }
}
