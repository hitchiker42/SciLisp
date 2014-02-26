//(do (var init [step])(end-test) body..)
sexp lisp_do_expander(sexp args){
  if(!consp(args) || !consp(XCDR(args))){
    return error_sexp("too few args passed to do");
  }
  sexp binding=xcar(args);
  if(!consp(binding)){
    return error_sexp("malformed bindings list in do expression");
  }
}
sexp lisp_dotimes_expander(sexp var,sexp times,sexp body,sexp cur_env_sexp,int expand){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp test=cons(function_sexp(&lisp_numlt_call),cons(var,cons(times,nil)));
  sexp do_parameters=
    cons(var,cons(long_sexp(0),cons(long_sexp(1),cons(test,nil))));
  sexp code=cons(spec_sexp(_do),
                 cons(do_parameters,body));
  if(expand){
    return code;
  } else {
    return eval(code,cur_env);
  }
}
sexp lisp_dolist_expander(sexp var,sexp list,sexp body,sexp cur_env_sexp,int expand){
  env *cur_env=cur_env_sexp.val.cur_env;
  sexp test=cons(function_sexp(&lisp_consp_call),cons(list,nil));
  sexp var_step=Fcons(spec_sexp(_setq),
                     Fcons(var,cons(function_sexp(&car_call),cons(list,nil))));
  sexp list_step=Fcons(spec_sexp(_setq),
                      cons(list,cons(function_sexp(&cdr_call),cons(list,nil))));
  sexp step=Fcons(spec_sexp(_progn),cons(var_step,cons(list_step,nil)));
  sexp loop=Fcons(spec_sexp(_while),cons(test,cons(step,cons(body,nil))));
  if(expand){
    return loop;
p  } else {
    return eval(loop,cur_env);
  }
}
sexp lisp_dec_ref(sexp sym_sexp,sexp cur_env_sexp){
  if(!symbolp(sym_sexp)){
    return format_type_error("decf","symbol",sym_sexp.tag);
  }
  env *cur_env=cur_env_sexp.val.cur_env;
  symref sym=getsym(cur_env,sym_sexp.val.var->name);
  if(!sym){
    return format_error_sexp("undefined variable %r",sym->name);
  }
  sexp temp=lisp_dec(sym->val);
  if(errorp(temp)){
    return temp;
  } else {
    sym->val=temp;
    return temp;
  }
}
/*sexp lisp_defconst(sexp sym_sexp,sexp val_sexp,sexp cur_env_sexp){
  sexp code=cons(spec_sexp(_def),cons(sym_sexp,cons(val_sexp,nil)));
  eval(code,cur_env_sexp.val.cur_env);
  }*/
sexp lisp_inc_ref(sexp sym_sexp,sexp cur_env_sexp){
  if(!SYMBOLP(sym_sexp)){
    return format_type_error("incf","symbol",sym_sexp.tag);
  }
  env *cur_env=cur_env_sexp.val.cur_env;
  symref sym=getsym(cur_env,sym_sexp.val.var->name);
  if(!sym){
    return format_error_sexp("undefined variable %r",sym->name);
  }
  sexp temp=lisp_inc(sym->val);
  if(ERRORP(temp)){
    return temp;
  } else {
    sym->val=temp;
    return temp;
  }
}
sexp lisp_incf_expander(sexp sym_sexp,env *cur_env){
  /* (incf <var>) ->
     `(setq ,var (++ ,var))
     (setq . (var . ((++ . (var . nil)) . nil)))
  */
  symref inc_symbol=xmalloc(sizeof(symbol));
  inc_symbol->name="++";
  inc_symbol->val=unbound;
  sexp body=Fcons(symref_sexp(inc_symbol),
                 Fcons(eval_sub(sym_sexp,cur_env),nil));
  sexp code=Fcons(spec_sexp(_setq),Fcons(sym_sexp,cons(body,nil)));
}
