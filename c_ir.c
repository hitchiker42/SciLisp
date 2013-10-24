#include "common.h"
#include "cons.h"
#include "prim.h"
//we need to do something to the ast before we translate it to c
//because lisp doesn't translate to C very well
//so I need some kind of ir, or I could just translate it to c I guess
//from common.h CORD error_str;
static long symbol_counter=0;
#define lambda_name(cord) CORD_sprintf(&cord,"#<lambda%06d>",(++symbol_counter))
struct astToC_ir{
  sexp main;//main function(128)
  sexp* vars;//top level variables(64)
  CORD var_declarations;//64
  sexp* funs;//top level functions(64)  
  CORD fun_declarations;//(64)
  sexp* exprs;//top level expressions(64)
  int num_vars;
  int num_funs;
  int num_exprs;
};
struct astToC_ir* astToC_ir;
void gen_ir(sexp ast){
  astToC_ir=xmallloc(sizeof(c_ir_code));
  astToC_ir->vars=xmalloc(8*sizeof(sexp));
  astToC_ir->funs=xmalloc(8*sizeof(sexp));
  astToC_ir->exprs=xmalloc(8*sizeof(sexp));
  int err_check;
  err_check=ast_to_array(ast);
  if(err_check){goto ERROR;}
 ERROR:
  CORD_fprintf(stderr,error_str);
  longjmp(error_buf,-1);
}
#define check_ri_size(size,array,index)                    \
  if(index>astToC_ir->size){                               \
    size*=2;                                                        \
    astToC_ir->array=xrealloc(astToC_ir->array,astToC_ir->size);    \
  }
int ast_to_arrays(sexp ast){
  int v=0,f=0,e=0;
  while(CONSP(ast)){
    if(!CONSP(XCAR(ast))){}
    if(!SPECP(XCAAR(ast))){
      check_size(num_exprs,exprs,e);
      astToC_ir->exprs[e++]=XCAR(ast);
      astToC_ir->num_exprs++;
    } else {
      switch(XCAAR(ast).val.special){
        case _def:{
          //is this a variable or a function
          //(def var lambda_or_defn)
          sexp defn=cddar(ast);
          if(defn.tag==_error){
            error_str=defn.val.cord;
            return 1;
          }
          if(defn.tag==_cons){
            if(XCAR(defn).tag == _special){
              if(XCAR(defn).val.special==_lambda){                
                check_size(num_funs,funs,f);
                //assume function parameters get done during parsing
                //generate a lambda object
                //set val field of cadar(ast) to lambda(cadar(ast) is the
                //symbol we're defining
                //then set astToC_ir->funs[f++] to cadar(ast)
              }
            }
          } else {
            check_size(num_vars,vars,v);
            //this just sets the value of the symbol we're defining
            //to the atom in XCDDAR(ast)
            XCADAR(ast).val.var->val=XCDDAR(ast)
        }
      }
        case _defun:{
          //we have (defun var (args) body)
          //change it to (def var (lambda (args) body))
          //so the actual function is (lambda (args) body))
          //and evaluate it the same way as for a lambda
          //in def above
        }
        default:
          check_size(num_exprs,exprs,e);
          astToC_ir->exprs[e++]=XCAR(ast);
          astToC_ir->num_exprs++;
      }
    }
  }
}
