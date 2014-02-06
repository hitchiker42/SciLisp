/* Read eval print loop, parameterized for both read and eval
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
/*Scan a line, subtract 1 from parens for each ")"
 *add 1 to parens for each "(", return -1 if we find a close parenteses
 *without an opening one.
 *parens is an int containing the number of currently open parentheses
 *it could probably be a pointer, but I like to keep functions pure
 */
#include "frontend.h"
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
#ifdef HAVE_READLINE
/* Read interactive input using readline.
 * outfile is a temporary file essentally used as a pipe. read scans a line
 * and if that line has an open paren without a matching close paren read
 * will continue to scan lines unitl all parentese are matched, or it finds
 * a syntax error(really just an extra close paren).
 * input is written to outfile as a place to hold the current input. It is
 * assumed that code will be read from outfile and evaluated. Read returns
 * the position in outfile where it started scanning*/
int lisp_readline(FILE* outfile,char* filename){
  FILE* my_pipe=outfile;
  char* tmp_file=filename;
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
        truncate(tmp_file,0);
        goto MAIN_LOOP;
      }
      line_read=readline(">");
      if(line_read == NULL){
        puts("");
        //        eval_error=1;
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
#endif
//input w/o readline
int lisp_getline(FILE* outfile,char* filename){
  FILE* my_pipe=outfile;
  char* tmp_file=filename;
  int parens,start_pos=ftello(my_pipe);
  size_t len;
 MAIN_LOOP:while(1){
    parens=0;
    //makesure readline buffer is NULL
    HERE();
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
        truncate(tmp_file,0);
        goto MAIN_LOOP;
      }
      fputs(">",stdout);
      len=getline(&line_read,NULL,stdin);
      if(line_read == NULL){
        puts("");
        //        eval_error=1;
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
void __attribute__((noreturn)) read_eval_print_loop(){
  static char *line_read =(char *)NULL;
  int parens,start_pos;
  char tmp_file[]={'/','t','m','p','/','S','c','i','L','i','s','p','_','P','i','p','e',
                  'X','X','X','X','X','X','\0'};
  int fd=mkstemp(tmp_file);
  FILE* my_pipe=fdopen(fd,"w+");
  yyscan_t scanner;
  HERE();
  yylex_init(&scanner);
  yyset_in(my_pipe,scanner);
  HERE();
  //  yyin=my_pipe;
  #ifdef HAVE_READLINE
  rl_set_signals();
  rl_variable_bind("blink-matching-paren","on");
  #endif
  sexp ast;
  sexp *yylval=xmalloc(sizeof(sexp));
  HERE();
  //toplevel handler which catches any invalid nonlocal exit
  //also used to return to after any fatal lisp error (i.e
  //the lisp stack overflows or something
  frame *top_level_frame=make_frame((uint64_t)UNWIND_PROTECT_TAG,unwind_protect_frame);
  push_frame(current_env,*top_level_frame);
 REPL:while(1){
    if(setjmp(top_level_frame->dest)){
      if(STRINGP(top_level_frame->value)){
        //should print to lisp stderr 
        CORD_fprintf(stderr,top_level_frame->value.val.string->cord);
      } else {
        CORD_fprintf(stderr,"Recieved lisp error with value",print(top_level_frame->value));
      }
    }
    start_pos=lisp_readline_fun(my_pipe,tmp_file);
    HERE();
    fseeko(my_pipe,start_pos,SEEK_SET);
    HERE();
    yyrestart(my_pipe,scanner);
    HERE();
    ast=c_read(&scanner,yylval,NULL);
    HERE();
    //print
    if(!NILP(ast)){
      lisp_ans_ptr->val=eval_fun(XCAR(ast),current_env);
      CORD_printf(CORD_cat(print(lisp_ans_ptr->val),"\n"));
    } else {
      ;
    }
  }
}
