/* Macro and backquote/quasiquote expansion

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
/*
  after reading the common lisp hyperspec here's how backquote expansion
  should work theoretically:
  `atom -> atom
  `,form -> form ;unless form starts with an @ or a .
  `(a . b) -> (append (list `b) (list `b))
  `(a) -> (append (list `a));implicit nil at the end
  `(,a) -> (append (list a))
  `(,@a) -> (append a)
  `(,.a) -> (append a) ;a modified
  `[a] -> (list->vector `(a))
  ;exended example
  `(a b ,c ,@d ,.e . f) ->
  (append (list `a) (list `b) (list c) d e . (list `f)) ;e might be modified
 */
#include "common.h"
#include "cons.h"
sexp expand_quasiquote(sexp expr,env_ptr env){
  sexp retval=expr;
  while(CONSP(expr)){
    if(!CONSP(XCAR(expr))){
      expr=XCDR(expr);
    } else if (SYMBOLP(XCAAR(expr))){
      switch(XCAAR(expr).val.sym){
        case Qquasiquote:
          XCAR(expr)=expand_quasiquote(expr);
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
sexp internal_macroexpand_1(subr *sub,sexp expr,env_ptr env){//assume typechecking is done for sub
  cons *body=sub->lambda_body;
  sexp fun_env=NIL;
  lambda_list arglist=*(sub->lambda_arglist);
      
  /*
    cons *opt_arg_names= opt_args->cdr.sym;
    if(num_optargs){
    //push the defaulats, if any optional arguments
    //get passed they get pushed on top of these
    //and the defaults never get seen
    PUSH(cons_sexp(opt_arg_names),fun_env);
    }*/
  int i;
  for(i=0;i<num_reqargs;i++){
    if(!CONSP(args)){
      raise_simple_error_fmt(Eargs,"Too few args passed to %r",sub->lname->cord);
    } else {
      PUSH(Fcons(req_arg_names[i],eval_arg(POP(args),env)));
    }
  }
  for(i=0;i<num_optargs;i++){
    if(!CONSP(args)){
      while(i<num_optargs){
        PUSH(cons_sexp(&opt_arg_names[i++]),fun_env);
      }
      break;
    } else {
      PUSH(Fcons(opt_arg_names[i].car,eval_arg(POP(args),fun_env)));
    }
  }
}
