#include "common.h"
#include "prim.h"
#include "lex.yy.h"
#include <readline/readline.h>
#include <readline/history.h>
#include <signal.h>
#include <getopt.h>
jmp_buf main_loop;
static char *line_read;
void handle_sigsegv(int signal){
  fprintf(stderr,"recieved segfault, exiting\n");
  exit(1);
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
 */
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
int compile(FILE* input,const char *output,FILE* c_code){
    HERE();
    sexp ast=yyparse(input);
    princ(ast);
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
int lispRead(FILE* outfile,char* filename){
    FILE* my_pipe=outfile;
    char* tmpFile=filename;
    int parens,start_pos;
  MAIN_LOOP:while(1){
      parens=0;
      start_pos=ftello(my_pipe);
      //makesure readline buffer is NULL
      if (line_read){
        free (line_read);
        line_read = (char *)NULL;
      }
      line_read = readline("in>");
      if (line_read && *line_read){      
        add_history (line_read);
      }
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
    }
    return start_pos;
  }
int main(int argc,char* argv[]){
  //setup handler for sigsegv, so we can exit gracefully on a segfault
  sigaction(SIGSEGV,sigsegv_action,NULL);
  initPrims(); //read primitives into syntax table, this really should just 
  // read a binary file containing a prepopulaed syntax table;
  static char *line_read =(char *)NULL;
  //need to replace with proper getopt interface
  if(argv[1]!=NULL){
    FILE* file=fopen(argv[1],"r");
    compile(file,"a.out",NULL);
  }
  int parens,start_pos;
  char tmpFile[L_tmpnam];
  tmpnam_r(tmpFile);
  char test[100];
  FILE* my_pipe=fopen(tmpFile,"w+");
  yyin=my_pipe;
  rl_set_signals();
  //read
  start_pos=lispRead(my_pipe,tmpFile);
  fseeko(my_pipe,start_pos,SEEK_SET);
  HERE_MSG("Lines Read, Calling yyparse");
  //eval
  sexp ast=yyparse(my_pipe);
  sexp result=internal_eval(ast);
  //print
  princ(ast);
}
    /*    while((yytag=yylex(yylval)) != -1){
      switch (yytag){
        case TOK_ID:
          printf("Lexed id %s\n",yylval->string);
          getSym(yylval->string,tmpsym);
          if(tmpsym){
            printf("Symbol %s found in symbol table\n",tmpsym->name);
            sexp tempsexp=tmpsym->val;
            printf("Symbol bytes are %#0lx\n",tempsexp.val);
            if(tempsexp.tag == _fun){
              yytag=yylex(yylval);
              double x,y;
              if(yytag=TOK_REAL){x=yylval->real64;}
              else if(yytag=TOK_INT){x=(double)yylval->int64;}
              else{continue;}
              yytag=yylex(yylval);
              if(yytag=TOK_REAL){y=yylval->real64;}
              else if(yytag=TOK_INT){y=(double)yylval->int64;}
              else{continue;}
              double(*fp)(double,double)=tempsexp.val.fun;
              double result=fp(x,y);
              printf("%f %s %f = %f\n",x,tmpsym->name,y,result);
            } else if(tempsexp.tag==_double){
              printf("Symbol value is %f\n",tempsexp.val.real64);
            } else if(tempsexp.tag == _long){
              printf("Symbol value is %ld\n",tempsexp.val.int64);
            } else {
              printf("Symbol value is nil\n");
            }
          } else {
            symref* newsym = xmalloc(sizeof(symref));
            newsym->name=yylval->string;
            yytag=yylex(yylval);
            if (yytag == TOK_REAL){
              printf("yylval->real64 = %f\n",yylval->real64);
              newsym->val=(sexp){_double,yylval->real64};
            } else if (yytag == TOK_INT){
              printf("yylval->int64 = %ld\n",yylval->int64);
              newsym->val.tag=_long;
              newsym->val.val.int64=yylval->int64;
            } else {
              newsym->val=NIL;
            }
            addSym(newsym);
            printf("added Symbol %s to symbol table\n",newsym->name);
          }
          break;
        case TOK_REAL:
          printf("Lexed real %f\n",yylval->real64);
          break;
        case TOK_INT:
          printf("Lexed int %d\n",yylval->int64);
        case TOK_LPAREN:
          puts("Lexed (\n");
          break;
        case TOK_RPAREN:
          puts("Lexed )\n");
          break;
        case TOK_TYPEINFO:
          printf("Lexed typename %s\n",yylval->string);
          break;
        case TOK_QUOTE:
          printf("Lexed a quote\n");
          break;
        default:
          printf("Lexed Unknown Token\n");
          break;;
      }
    }
    }*/
//  }
//}
