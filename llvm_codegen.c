#include "llvm.h"
LLVMModuleRef SL_Module;
LLVMContexRef SL_Context;
LLVMBuilderRef InstrBuilder;
LLVMTypeRef LispDouble;
LLVMTypeRef LispLong;
LLVMTypeRef LispInt;
LLVMTypeRef LispShort;
LLVMTypeRef LispChar;
LLVMTypeRef LispWChar;
LLVMTypeRef LispData;
LLVMTypeRef LispSexp;
LLVMTypeRef LispCons;
LLVMTypeRef LispArray;
LLVMValueRef LispNIL;
LLVMValueRef LispTRUE;
LLVMValueRef LispFALSE;
LLVMTypeRef LispArgs[8];
LLVMTypeRef LispFxnTypes[9];
LLVMExecutionEngineRef SL_Engine;//I suppose this could be jit or an interpreter
static handle_error(){
  CORD_fprintf(stderr,error_str);fputs("\n",stderr);
  return LispNIL;
}
void initialize_llvm(int engine){
  int i;
  SL_Context=LLVMGetGlobalContext();
  SL_Module=LLVMModuleCreateWithNameInContext("Main_Module",SL_Context);
  InstrBuilder=LLVMCreateBuilderInContext(SL_Context);
  LLVMSetTarget(SL_Module,"x86_64-unknown-linux-gnu");
  LispDouble=LLVMDoubleTypeInContext(SL_Context);
  LispLong=LLVMInt64TypeInContext(SL_Context);
  LispInt=LLVMInt32TypeInContext(SL_Context);
  LispShort=LLVMInt16TypeInContext(SL_Context);
  LispChar=LLVMInt8TypeInContext(SL_Context);
  LispData=LLVMStructCreateNamed(SL_Context,"data");
  LispSexp=LLVMStructCreateNamed(SL_Context,"sexp");
  LispCons=LLVMStructCreateNamed(SL_Context,"cons");
  LispVoid=LLVMVoidTypeInContext(SL_Context);
  LLVMStructSetBody(LispData,&LispDouble,1,1);
  LLVMTypeRef* SexpTypes={LispData,LispInt,LispShort,LispShort};
  LLVMTypeRef* ConsTypes={LispSexp,LispSexp};
  LLVMStructSetBody(LispSexp,SexpTypes,4,1);
  LLVMStructSetBody(LispCons,ConsTypes,2,1);
  LispArgs={LispSexp,LispSexp,LispSexp,LispSexp,
            LispSexp,LispSexp,LispSexp,LispSexp};
  LispFxnTypes[0]=LLVMFunctionType(LispSexp,&LispVoid,0,0};
  for(i=1;i<9;i++){
    LispFxnTypes[i]=LLVMFunctionType(LispSexp,LispArgs,i,0};
  }
  switch(engine){
    case 1:
      LLVMCreateJITCompiler(SL_Engine,SL_Module,2,&error);
      break;
    case 2:
      LLVMCreateJITCompiler(SL_Engine,SL_Module,0,0,&error);
      break;
    case 3:
      LLVMCreateInterpreterForModule(SL_Engine,SL_Module,&error);
      break;
  }
}
#ifdef EVAL
#undef EVAL
#define EVAL(expr,env,builder) LLVM_Codegen(expr,cur_env,builder)
sexp LLVMEval(sexp expr,env cur_env);
LLVMValueRef LLVM_Codegen(sexp expr,env cur_env,LLVMBuilderRef builder);
LLVMValueRef LLVM_Codegen(sexp expr,env cur_env,LLVMBuilderRef builder){
  switch(expr.tag){
    case _cons:
      if(SYMBOLP(car(expr))){
        sexp curFun=XCAR(expr).val.var->val;
        if(LAMBDAP(curFun)){
          //this probably needs to be changed
          return LLVM_Call_Lambda(expr,cur_env,builder);//not sure about this
        }
        if(!FUNP(curFun)){
          CORD_fprintf(stderr,"tag = %s\n",typeName(curFun));
          CORD_sprintf(&error_str,"%r is not a function or special form",
                       print(curFun));
          goto ERROR;
        } else {
          return LLVM_Call_Builtin(expr,cur_env);
        }
      } else if(SPECP(car(expr))){        
        return LLVM_Codegen_Special(expr,cur_env);
      } else {
        format_error_str("car of unquoted list is not a function or special form"
                         "\ncar is %s",print(car(expr)));
        
        goto ERROR;
      }
    case _sym:
      tempsym = getSym(cur_env,expr.val.var->name);
      if(tempsym){
        return EVAL(tempsym->val,cur_env,builder);
      } else {
        CORD_sprintf(&error_str,"undefined variable %r used",expr.val.var->name);
        goto ERROR;
      }
    case _fun:
      return expr;
    default:
      return expr;
  }
 ERROR:
  return handle_error();
}
static inline sexp LLVM_Codegen_Special(sexp expr,env cur_env,
                                        LLVMBuilderRef builder){
  //this is an internal only inline function, ie this function itself
  //won't be in the generated code, it's just used to git the source code
  //a bit more clarity
  //this is always called on a cons, no need to check
  sexp special_sexp=car(expr);
  symref newSym;
  switch(special_sexp.val.special){
    //for now focus on def,defun,if and do
    case _def:
      return LLVM_Codegen_def(expr,cur_env,builder);
    case _setq: 
      newSym = getSym(cur_env,cadr(expr).val.var->name);
      sexp symVal=LLVM_Codegen(caddr(expr),cur_env,builder);
      if(!newSym){
        //NEED TO MAKE GENERIC
        newSym=xmalloc(sizeof(global_symbol));
        newSym->name=(cadr(expr).val.var->name);
        newSym=addSym(cur_env,newSym);
        newSym->val=symVal;
      } else {
        newSym->val=symVal;
      }
      //FIX
      //return (sexp){.tag = _sym,.val={.var = newSym}};
    case _lambda: 
      return LLVM_Codegen_lambda(expr,cur_env,builder);
    case _if: 
      return LLVM_Codegen_if(expr,cur_env,builder);
    case _do: return LispNIL;
    case _while:
      return LLVM_Codegen_while(expr,cur_env,builder);
    case _defun:
      return LLVM_Codegen_defun(expr,cur_env,builder);
    case _progn:
      return LLVM_Codegen_progn(expr,cur_env,builder);
    case _prog1:
      return LLVM_Codegen_prog1(expr,cur_env,builder);
    default:
      goto error;
  }
 error:
  return handle_error();
}

static LLVMValueRef LLVM_Codegen_if(sexp expr,env cur_env,
                                    LLVMBuilderRef builder){
  if(cdr(cdddr(expr)).tag != _nil){
    CORD_sprintf(&error_str,"excess arguments to if expression\n");
    return handle_error();
  } else {
    return LLVMBuildCondBr(builder,EVAL(cadr(expr),cur_env,builder),
                           EVAL(caddr(expr),cur_env,builder),
                           EVAL(car(cdddr(expr),cur_env,builder)));
  }
}
static LLVMValueRef LLVM_Call_Function(sexp expr,env cur_env,
                                       LLVMBuilderRef builder){
  sexp curFun=car(expr).val.var->val;
}
static LLVMValueRef* get_args(sexp arglist,function fun,env cur_env,
                              LLVMBuilderRef builder){
  //arglist is (sexp . (sexp . (sexp ....()...)))
  int minargs=fun.min_args;int maxargs=fun.max_args;
  int i=0;
  sexp *args;
  if(maxargs < 0){
    args=xmalloc(sizeof(LLVMValueRef)*(minargs+1));
    while(i<minargs && CONSP(arglist)){
      args[i++]=EVAL(XCAR(arglist),cur_env);
      arglist=XCDR(arglist);
    }
    args[i]=arglist;
    return args;
  }
  args=xmalloc(sizeof(maxargs));
  //fill in args
  while(CONSP(arglist) && (i < maxargs)){
    args[i++]=EVAL(XCAR(arglist),cur_env);
    arglist=XCDR(arglist);
  }
  //check to make sure all required arguments are filled
  if (i < minargs){
    format_error_str("Too few arguments given to %s, minimum %d args",
                     function_name(fun),minargs);
    handle_error();
    return 0;
  }
  //fill in any unfilled optional arguments
  while(i<maxargs){
    args[i++]=LispNIL;
  }
  return args;
}
  
#undef EVAL
