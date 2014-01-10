sexp lisp_setq(sexp expr,env *cur_env){
  expr=XCDR(expr);//drop the setq
  if(!cur_env){
    sexp val=eval(XCDR(expr),cur_env);
    XCAR(expr).val.sym->val=val;
    return XCAR(expr);
  } else {
    sexp cell;
    env *search_env=cur_env;
    while(search_env){
      cell=c_assq(*(sexp*)cur_env->head,XCAR(expr));
      if(!NILP(cell)){
        XCDR(cell)=eval(XCDR(expr),cur_env);
        return XCAR(expr);
      } else {
        search_env=cur_env->enclosing;
      }
    }
      
    
