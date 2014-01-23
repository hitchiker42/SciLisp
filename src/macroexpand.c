#include "common.h"
#include "cons.h"
sexp expand_in_backquote(sexp expr,env_ptr env){
  sexp retval=expr;
  while(CONSP(expr)){
    if(!CONSP(XCAR(expr))){
      expr=XCDR(expr);
    } else if (SYMBOLP(XCAAR(expr))){
      switch(XCAAR(expr).val.sym){
        case Qbackquote:
          XCAR(expr)=expand_in_backquote(expr);
          continue;//don't move onto XCDR yet
        case Qcomma:
          XCAR(expr)=eval(XCADR(expr),env);
          expr=XCDR(expr);
          continue;
          //this is always non-destructive currently
          //the way it should be is ',.' means append destructively
          //and ,@ means do it non destructively
        case Qsplice:{
          //expr=((,@(x,rest...)) y,...)
          sexp cur_tail=XCDR(expr);
          sexp ls_splice=eval(XCDAR(expr),env);
          if(!CONSP(ls_splice)){
            raise_simple_error(Etype,",@ requires proper list");
          }
          ls_splice=copy_cons(ls_splice);
          sexp ls_last=last(ls_splice);
          if(!NILP(XCDR(ls_last))){
            raise_simple_error(Etype,",@ requires proper list");
          }

          XCAR(expr)=XCAR(ls_splice);
          XCDR(expr)=XCDR(ls_splice);
          SET_CDR(ls_last,cur_tail);
          expr=cur_tail;
          continue;
        }
      }
      //any non special symbol will fall throught to here
      //and all special symbols (, ,@ `) continue from
      //the switch and so won't reach here
      XCAR(expr)=expand_in_backquote(expr);
      expr=XCDR(expr);
    }
  }
  return retval;
}
