#include "common.h"
#include "cons.h"
//(defun cat (&rest strings)
sexp lisp_cat(sexp string,sexp rest){
  if(!STRINGP(string)){
    if(NILP(string)){
      return LISP_EMPTY_STRING;
    } else {
      goto CAT_ERR;
    }
  }
  CORD retval = string.val.cord;
  while(CONSP(rest)){
    if(!STRINGP(XCAR(rest))){goto CAT_ERR;}
    retval=CORD_cat(retval,XCAR(rest).val.cord);
    rest=XCDR(rest);
  }
  return (sexp){.tag = _str,.val={.cord=retval}};
  CAT_ERR:
  return 
    format_type_error_rest("cat","strings",XCAR(rest).tag,XCAR(rest));
}
//(defun cwd ())
sexp lisp_getcwd(){
  //probabaly not the most efficent way to do this
  char* temp_cwdname=get_current_dir_name();
  CORD cwdname=CORD_from_char_star(temp_cwdname);
  free(temp_cwdname);
  return cord_sexp(cwdname);
}
sexp lisp_system_simple(sexp command){
  if(!STRINGP(command)){
    return format_type_error("system","string",command.tag);
  } else {
    int retval=system(command.val.cord);//should probably be CORD_as_cstring
    return long_sexp(retval);
  }
}
static void *async_system_helper(void *command){
  char *command_str=(char *)command;
  system(command_str);
  return 0;
}
sexp lisp_system_async(sexp command){
  if(!STRINGP(command)){
    return error_sexp("argument to system must be a string");
  }
  pthread_t new_thread;
  char *pthread_arg=(char*)CORD_to_char_star(command.val.cord);
  pthread_create(&new_thread,NULL,async_system_helper,pthread_arg);
  return NIL;
}

#define SHELL "/bin/bash"
sexp lisp_system(sexp command,sexp args){
  //  HERE();
  if(!STRINGP(command)){
  string_error:
    return error_sexp("arguments to system must be strings");
  } if(NILP(args)){
    //    return lisp_system_simple(command);
  }
  /*allocate arguments, suprisingly complicated */
  char **argv=alloca(16*sizeof(char*));//this should usually be enough
  argv[0]="bash";
  argv[1]="-c";
  argv[2]=(char*)CORD_to_const_char_star(command.val.cord);
  int i=3,maxargs=16;
  while(1){
    //    HERE();
    while(CONSP(args) && i<maxargs){
      //      HERE();
      if(!STRINGP(XCAR(args))){goto string_error;}
      argv[i]=(char*)CORD_to_const_char_star(XCAR(args).val.cord);
      args=XCDR(args);
      i++;
    } if(i<maxargs){break;}
    maxargs*=2;
    char** temp=alloca(maxargs*sizeof(char*));
    *temp=*argv;//? this seems somehow wrong
    argv=temp;
  }
  argv[i]=NULL;
  PRINT_MSG(argv[3]);
  //now to actually do what we came here for
  int status;
  pid_t pid;
  pid=fork();
  if(!pid){
    //we're in the child process now (fork returns 0 in child process)
    execv(SHELL,argv);//doesn't return
    _exit(EXIT_FAILURE);//if we get here something failed
    //we need to termin
  } else if (pid < 0) {//fork failed
    return_errno("fork");
  } else {//this is the parent process
    if(waitpid(pid,&status,0) != pid){
      return error_sexp
        ("the hell, I only forked one process, how'd we get here");
    } else {
      return long_sexp(status);
    }
  }
}
//pretty sure this doesn't actually work
sexp get_pathname(sexp pathname_str,sexp no_expansion){
  if(!STRINGP(pathname_str)){
    return format_type_error("get-pathname","string",pathname_str.tag);
  } if(!NILP(no_expansion)){
    return pathname_str;
  }
  int tilde;
  CORD pathname=pathname_str.val.cord;
  if((tilde = CORD_chr(pathname,CORD_len(pathname)-1,'~'))!= -1){
    HERE();
    char *home_dir=getenv("HOME");
    //not sure the best way to splice a cord into the middle of an existing cord
    CORD_pos pos[1];
    CORD_set_pos(pos[0],pathname,tilde);
    CORD rest=CORD_pos_to_cord(pos[0]);
    pathname=CORD_catn(3,CORD_substr(pathname,0,tilde),CORD_from_char_star(home_dir),rest);
  }
  return cord_sexp(pathname);
}
//I can think of an issue here, if I fail to load a file, the parts I'd already
// evaluated are already loaded, but it does work
sexp lisp_load(sexp pathname){
  pathname=get_pathname(pathname,NIL);
  if(ERRORP(pathname)){return pathname;}
  sexp ast;
  FILE* file=fopen(CORD_to_const_char_star(pathname.val.cord),"r");
  if(!file){    
    return error_sexp("invalid filename passed to load");
  }
  ast=yyparse(file);
  sexp result=NIL;
  while (CONSP(ast)){
    result=eval(XCAR(ast),topLevelEnv);
    if(ERRORP(result)){
      return error_sexp(CORD_cat("error loading file, error was:\n",result.val.cord));
    }
    ast=XCDR(ast);
  }
  return LISP_TRUE;
}
sexp lisp_open(sexp filename,sexp mode){
  if(NILP(mode)){
    mode=string_sexp("r");
  }
  if (!STRINGP(filename) || !(STRINGP(mode))){
    return format_type_error2("open","string",filename.tag,"string",mode.tag);
  }
  FILE* file = fopen(CORD_to_const_char_star(filename.val.cord),
                     CORD_to_const_char_star(mode.val.cord));
  if(file){
    return (sexp){.tag=_stream,.val={.stream=file}};
  } else {
    PRINT_MSG(filename.val.cord);
    PRINT_MSG(mode.val.cord);
    return_errno("fopen");
  }
}
sexp lisp_close(sexp stream){
  if(!STREAMP(stream)){
    return format_type_error("close","stream",stream.tag);
    //    return error_sexp("invalid file descriptor passed to close");
  } else {
    //fclose returns 0 on success and EOF on failure
    if(fclose(stream.val.stream)){
      return_errno("fclose");
    } else {
        return NIL;
    }
  }
}
sexp lisp_fputs(sexp string,sexp stream){
  if(!STREAMP(stream)||!STRINGP(string)){
    format_type_error2("fputs","string",string.tag,"stream",stream.tag);
  } else if (string.tag == _str){
    fputs(CORD_to_const_char_star(string.val.cord),stream.val.stream);
  } else {//string must be a w_char string
    fputws(string.val.ustr,stream.val.stream);
  }
  return NIL;
}
sexp lisp_time(sexp raw){
  time_t cur_time=time(NULL);
  if(isTrue(raw)){
    return int64_sexp(cur_time);
  } else {
    //man page for ctime_r says to allocate a buffer of at least
    //23 characters so allocate 32 to be safe/make allocation eaiser
    char *time_str=xmalloc(32*sizeof(char));
    if(!ctime_r(&cur_time,time_str)){
      return_errno("time");
    } else {
      return cord_sexp(time_str);
    }
  }
}
//(defun exit (&optional status))
sexp lisp_exit(sexp exit_code){
  if(NILP(exit_code)){
    exit(EXIT_SUCCESS);
  } if (!INTP(exit_code)){
    return format_type_error("exit","integer",exit_code.tag);
  } else {
    exit(exit_code.val.int64);
  }
}
