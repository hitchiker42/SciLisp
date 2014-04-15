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

#include "frontend.h"
#ifdef HAVE_READLINE
/* While readline is nice to use, it sure isn't nice to program with
 */
CORD get_sexp_readline(){
  int parens=0,i;
  char *prompt="SciLisp>";
  char *line_read=NULL;
  CORD retval=0;
 MAIN_LOOP:while(1){
    line_read = readline(prompt);
    if (line_read){
      if (*line_read){
        add_history (line_read);
      } else {goto MAIN_LOOP;}
    } else {puts("\n");exit(0);}
    for(i=0;i<rl_end;i++){
      if(line_read[i]==';'){break;}
      if(line_read[i]==')'){
        parens--;
      } else if (line_read[i]=='('){
        parens++;
      }
    }
    retval=CORD_catn(3,GC_strdup(line_read)," ",retval);
    free(line_read);
    if(parens){
      if(parens<0){
        fprintf(stderr,"Extra close parentheses\n");
        retval=0;
        prompt="SciLisp>";
        goto MAIN_LOOP;
      } else {
        prompt=">";
        goto MAIN_LOOP;
      }
    } else {
      return retval;
    }
  }
}  
void __attribute__((noreturn)) readline_repl(sexp(*eval_fun)(sexp,env_ptr)){
  read_input *cord_input;
  CORD readline_output;
  sexp ast,ans;
  top_level_frame=make_frame((uint64_t)UNWIND_PROTECT_TAG,unwind_protect_frame);
  push_frame(*top_level_frame,current_env);
 REPL:while(1){
    if(setjmp(top_level_frame->dest)){
      PRINT_MSG("jumped to top level frame");
      if(STRINGP(top_level_frame->value)){
        //should print to lisp stderr 
        CORD_fprintf(stderr,top_level_frame->value.val.string->cord);
      } else {
        CORD_fprintf(stderr,"Recieved lisp error with value",print(top_level_frame->value));
      }
    }
    readline_output=get_sexp_readline();
    cord_input=make_cord_input(readline_output);
    ast=read_sexp(cord_input);
    if(!NILP(ast)){
      //lisp_ans_ptr->val=eval_fun(ast,current_env);
      ans=eval_fun(ast,current_env);
      //CORD_puts(print(lisp_ans_ptr->val));
      CORD_puts(print(ans));
    }
  }
}
#endif /*HAVE_READLINE*/
void __attribute__((noreturn)) simple_repl(sexp(*eval_fun)(sexp,env_ptr)){
  read_input *stdin_input=make_stream_input(stdin);
  sexp ast,ans;
 REPL:while(1){
    if(setjmp(top_level_frame->dest)){
      if(STRINGP(top_level_frame->value)){
        //should print to lisp stderr 
        CORD_fprintf(stderr,top_level_frame->value.val.string->cord);
      } else {
        CORD_fprintf(stderr,"Recieved lisp error with value",print(top_level_frame->value));
      }
    }
    ast=NIL;
    fputs("SciLisp>",stdout);
    ast=read_sexp(stdin_input);
    PRINT_FMT("Read input, %r",print(ast));
    if(!NILP(ast)){
      //lisp_ans_ptr->val=eval_fun(ast,current_env);
      ans=eval_fun(ast,current_env);
      //CORD_printf(CORD_cat(print(lisp_ans_ptr->val),"\n"));
      CORD_puts(print(ans));
    }
  }
}
/* Above I use top_level_frame, which just expands to current_env->protect_frame
    if(setjmp(current_env->protect_frame->dest)){
      if(STRINGP(current_env->protect_frame->value)){
        //should print to lisp stderr 
        CORD_fprintf(stderr,current_env->protect_frame->value.val.string->cord);
      } else {
        CORD_fprintf(stderr,"Recieved lisp error with value",
                     print(current_env->protect_frame->value));
      }
    }
 */
