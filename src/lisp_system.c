/* System functions, system calls, shell commmands, io, filesystem stuff,etc...

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
#include "common.h"
#include "cons.h"
#include <fcntl.h>
#ifdef MULTI_THREADED
static pthread_mutex_t getenv_mutex=PTHREAD_MUTEX_INITALIZER;
#endif
//return a string containing the current directory name
//the string is allocated using GC_malloc_atomic
char *gc_getcwd(){
  uint64_t size=128;
  char *buf
  while(1){
    buf=xmalloc_atomic(size);
    if(getcwd(buf,size)==buf){
      //It might be faster to keep track of the last
      //value of size and use that, but probably not
      return buf;
    }
    if(errno != ERANGE){
      raise_simple_error(Esystem,lisp_strerror(errno));
    }
    size*=2;
  }
}
//(defun cwd ())
sexp lisp_getcwd(){
  return make_string_len(gc_getcwd(),0);
}
//just a wrapper around the c system function
sexp lisp_system_simple(sexp command){
  if(!STRINGP(command)){
    raise_simple_error(Etype,format_type_error("system","string",command.tag));
  } else {
    int retval=system(CORD_to_const_char_star(command.val.string->cord));
    return int64_sexp(retval);
  }
}
static void *async_system_helper(void *command){
  char *command_str=(char *)command;
  system(command_str);
  return 0;
}
sexp lisp_system_async(sexp command){
  if(!STRINGP(command)){
    raise_simple_error(Etype,"argument to system must be a string");
  }
  pthread_t new_thread;
  char *pthread_arg=(char*)CORD_to_const_char_star(command.val.string->cord);
  pthread_create(&new_thread,NULL,async_system_helper,pthread_arg);
  return NIL;
}
//it feels like there should be a better way to do this
//but at the same time what I'm doing is quite low level
//so the question is, is this what the shell does
//to implement something like echo or not?
//I'll need to look at the bash source

//when I wrote that I was using streams
//hopefully using a more explicit buffering 
//system geared to what I'm actually using this for
//this will optimize into something using x86_64 fast string operations
//or something similar
void fd_echo(int fd_in,int fd_out){
  char buf[8];
  ssize_t nbytes;
  while((nbytes=read(fd_in,buf,8))){
    if(write(fd_out,buf,nbytes)==-1){
      raise_simple_error(Esystem,lisp_strerror(errno));
    }
  }
}
//the core of this is a wrapper around getpwuid and getpwnam
//which could be reused should I ever need other partst of the passwd struct
char *get_home_dir(const char *name){
  struct passwd user_info[1];
  void *result;
  uint64_t bufsize=sysconf(_SC_GETPW_R_SIZE_MAX);
  if(bufsize<0){
    bufsize=4096;//a page, why not
  }
  int status;
  while(1){
    char *buf=xmalloc_atomic(bufsize);
    if(!name){
      status=getpwuid_r(getuid(),user_info,buf,bufsize,&result);
    } else {
      status=getpwnam_r(name,user_info,buf,bufsize,&result);
    }
    if(!result){
      if(status==ERANGE){
        bufsize*=2;
        continue;
      }
      if(status==0){
        raise_simple_error_fmt(Esystem,"Error no user %s found",name);
      } else {
        raise_simple_error(Esystem,lisp_strerror);
      }
    }
  }
  return user_info->pw_dir;
}


/*
  Effectively the same as getenv, but threadsafe (albeit only because it locks)
  and returns a copy of the value returned by getenv, insuring that it will not
  change and that should the caller so desire, it can be mutated
 */
char *getenv_r(const char *name){
#ifdef MULTI_THREADED
  pthread_mutex_lock(getenv_mutex);
#endif
  char *val=getenv(name);
  char *retval=NULL;
  if(!val){
    goto RETURN;//used to minimize the ammonut of code (less #ifdefs)
  }
  //I'm being kinda lazy here
  retval=GC_strdup(val);
 RETURN:
#ifdef MULTI_THREADED
  pthread_mutex_unlock(getenv_mutex);
#endif
  return retval;
}
  
#define SHELL "/bin/bash"
//equvilent to the shell commands 2>&1 && 1>&new_fd
#define redirect_stdout_stderr(new_fd)          \
  (close(STDOUT_FILENO);                        \
   close(STDERR_FILENO);                        \
   fcntl(new_fd,F_DUPFD,STDOUT_FILENO);         \
   fcntl(new_fd,F_DUPFD,STDERR_FILENO);)
  //add some way to specify an async process
sexp lisp_system(uint64_t numargs,sexp *args){
  int i;
  for(i=0;i<numargs;i++){
    if(!STRINGP(args[i])){
      raise_simple_error(Etype,"Arguments to system must be strings");
    }
  }
  int status;
  //for using a pipe
  int filedes[2];
  if(pipe(filedes)){
    raise_simple_error(Esystem,"Failure opening pipe");
  }
  //if i want to pipe stdout/err back to the parent I can't use vfork
  //pid_t pid=vfork();
  pid_t pid=fork();  
  if(pid<0){
    raise_simple_error(Esystem,lisp_strerror(errno));
  } else if (pid > 0) {
    close(filedes[1]);
    fd_echo(filedes[0],STDOUT_FILENO);
    if(waitpid(pid,&status,0) == (pid_t)-1){
      raise_simple_error(Esystem,lisp_strerror(errno));
    } 
  } else {
    //we're in the child process now (fork returns 0 in child process)
    close(filedes[0]);
    redirect_stdout_stderr(filedes[1]);
    execv(SHELL,args);//doesn't return
    _exit(EXIT_FAILURE);//if we get here something failed
  }
}
sexp expand_pathname(sexp pathname_str,sexp default_directory){
  if(!STRINGP(pathname_str)){
    raise_simple_error(Etype,format_type_error("get-pathname","string",pathname_str.tag));
  } if(!NILP(no_expansion)){
    return pathname_str;
  }
  CORD pathname=pathname_str.val.string->cord;
  CORD_ec expanded_pathname;
  CORD_ec_init(expanded_pathname);
  CORD_pos p;
  CORD_set_pos(p,pathname,0);
  char c=CORD_pos_fetch(p);
  if(c != '/'){
    if(c == '~'){
      CORD_ec_append_cord(expanded_pathname,getenv_r("$HOME"));
    } else if (c == '.'){
      char *current_dir=gc_getcwd;
      CORD_next(p);
      c=CORD_pos_fetch(p);
      if(c=='.'){
      }
    }
  }
  
  for(;CORD_pos_valid(pos); CORD_next(pos)){
    c=CORD_pos_fetch(p);
    switch(c){
      
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
  ast=yyparse(file,global_scanner);
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
/*sexp lisp_fputs(sexp string,sexp stream){
  if(!STREAMP(stream)||!STRINGP(string)){
    raise_simple_error(Etype,format_type_error2("fputs","string",string.tag,
                                                "stream",stream.tag));
  }
  fputs(CORD_to_const_char_star(string.val.string->cord),stream.val.stream);
  return NIL;
  }*/
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
