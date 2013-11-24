#include "common.h"
#include "cons.h"
sexp lisp_cat(sexp string,sexp rest){
  if(!STRINGP(string)){goto CAT_ERR;}
  CORD retval = string.val.cord;
  while(CONSP(rest)){
    if(!STRINGP(XCAR(rest))){goto CAT_ERR;}
    retval=CORD_cat(retval,XCAR(rest).val.cord);
    rest=XCDR(rest);
  }
  return (sexp){.tag = _str,.val={.cord=retval}};
  CAT_ERR:return error_sexp("arguments to cat must be strings");
}
sexp lisp_getcwd(){
  //probabaly not the most efficent way to do this
  char* temp_cwdname=get_current_dir_name();
  CORD cwdname=CORD_from_char_star(temp_cwdname);
  free(temp_cwdname);
  return (sexp){.tag=_str,.val={.cord=cwdname}};
}
sexp lisp_system_simple(sexp command){
  HERE();
  if(!STRINGP(command)){
    return error_sexp("argument to system must be a string");
  } else {
    int retval=system(command.val.cord);//should probably be CORD_as_cstring
    return long_sexp(retval);
  }
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
  argv[2]=CORD_as_cstring(command.val.cord);
  int i=3,maxargs=16;
  while(1){
    //    HERE();
    while(CONSP(args) && i<maxargs){
      //      HERE();
      if(!STRINGP(XCAR(args))){goto string_error;}
      argv[i]=CORD_as_cstring(XCAR(args).val.cord);
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
