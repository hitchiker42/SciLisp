#include "llvm_c.h"
LLVMModuleRef SL_Module;
LLVMContextRef SL_Context;
LLVMBuilderRef SL_Builder;
LLVMPassRegistryRef SL_Registry;
LLVMValueRef LispNIL;
LLVMValueRef LispTRUE;
LLVMValueRef LispFALSE;
LLVMTypeRef LispArgs[8];
LLVMTypeRef LispFxnTypes[9];
LLVMExecutionEngineRef SL_Engine;//I suppose this could be jit or an interpreter
int gensym_counter=0;
jmp_buf jmp_to_error;
sexp error_val;
#define GENSYM() (++gensym_counter)
#define void_fxn_type LispFxnTypes[0]
char *error;
#define PRIM_BC_SIZE 170640//update this whenever prim.bc is recompiled
static LLVMValueRef handle_error(){
  CORD_fprintf(stderr,error_str);fputs("\n",stderr);
  return LispNIL;
}
LLVMModuleRef* Parse_Prim_bc(const char* name){
  /*  int bitcode_file=fopen(name,O_RDONLY);
  int pgsize=getpagesize();
  char* bc_data=(char *)mmap(NULL,PRIM_BC_SIZE,PROT_READ,MAP_PRIVATE,
  bitcode_file,0);*/
  LLVMMemoryBufferRef *memBuff=xmalloc(PRIM_BC_SIZE*sizeof(char));
  LLVMModuleRef* retval=xmalloc(sizeof(memBuff));
  char* out_msg;
  LLVMCreateMemoryBufferWithContentsOfFile(name,memBuff,&out_msg);
  LLVMGetBitcodeModule(*memBuff,retval,&out_msg);
  xfree(memBuff);
  return retval;
}
void dump_bc_mod(){
  SL_Module = *(Parse_Prim_bc("prim.bc"));
  LLVMDumpModule(SL_Module);
  return;
}
//um, well I don't really need most of this now
void initialize_llvm(int engine){
  int i;
    SL_Registry=LLVMGetGlobalPassRegistry();
  //not sure which or how many of these routiens I need to call
  //so call fucking all of them
  LLVMInitializeCore(SL_Registry);
  LLVMInitializeTransformUtils(SL_Registry);
  LLVMInitializeScalarOpts(SL_Registry);
  LLVMInitializeObjCARCOpts(SL_Registry);
  LLVMInitializeInstCombine(SL_Registry);
  LLVMInitializeIPO(SL_Registry);
  LLVMInitializeInstrumentation(SL_Registry);
  LLVMInitializeAnalysis(SL_Registry);
  LLVMInitializeIPA(SL_Registry);
  LLVMInitializeCodeGen(SL_Registry);
  LLVMInitializeTarget(SL_Registry);

  SL_Module=*(Parse_Prim_bc("prim.bc"));
  SL_Context=LLVMGetModuleContext(SL_Module);
  SL_Builder=LLVMCreateBuilderInContext(SL_Context);
  LLVMSetTarget(SL_Module,"x86_64-unknown-linux-gnu");
  LispDouble=LLVMDoubleTypeInContext(SL_Context);
  LispLong=LLVMInt64TypeInContext(SL_Context);
  LispInt=LLVMInt32TypeInContext(SL_Context);
  LispShort=LLVMInt16TypeInContext(SL_Context);
  LispChar=LLVMInt8TypeInContext(SL_Context);
  LispVoid=LLVMVoidTypeInContext(SL_Context);
  LispData=LLVMGetTypeByName(SL_Module,"union.data");
  LispSexp=LLVMGetTypeByName(SL_Module,"struct.sexp");
  LispCons=LLVMGetTypeByName(SL_Module,"struct.cons");
  LispNIL=LLVMGetNamedGlobal(SL_Module,"NIL");
  if(!LispNIL){
    HERE();
    LispNIL=LLVMGetNamedGlobal(SL_Module,"NIL");
    if (!LispNIL){
      HERE();
    }
  }
  for(i=0;i<8;i++){
    LispArgs[i]=LispSexp;
  }
  LispFxnTypes[0]=LLVMFunctionType(LispSexp,&LispVoid,0,0);
  for(i=1;i<9;i++){
    LispFxnTypes[i]=LLVMFunctionType(LispSexp,LispArgs,i,0);
  }
  SL_Engine=xmalloc(sizeof(LLVMExecutionEngineRef)); 
  switch(engine){
    case 1:
      LLVMLinkInJIT();
      LLVMInitializeNativeTarget();
      LLVMCreateJITCompilerForModule(&SL_Engine,SL_Module,2,&error);
      break;
    case 2:
      LLVMLinkInMCJIT();
      LLVMInitializeNativeTarget();
      LLVMCreateMCJITCompilerForModule(&SL_Engine,SL_Module,0,0,&error);
      break;
    case 3:
      LLVMLinkInInterpreter();
      LLVMInitializeNativeTarget();
      LLVMCreateInterpreterForModule(&SL_Engine,SL_Module,&error);
      break;
  }
}
#ifdef EVAL
#undef EVAL
#endif
#define EVAL(expr,env,builder) LLVM_Codegen(expr,cur_env,builder)
sexp LLVMEval(sexp expr,env cur_env);
LLVMValueRef LLVM_Codegen(sexp expr,env cur_env,LLVMBuilderRef builder);
sexp LLVMEval(sexp expr,env cur_env){
  if(setjmp(jmp_to_error)){
    return error_val;
  }
  CORD toplevel_fun_name;
  CORD_sprintf(&toplevel_fun_name,"#<toplevel_expr%d>",GENSYM());
  char* toplevel_llvm_name=CORD_to_char_star(toplevel_fun_name);
  LLVMValueRef toplevel_fun=LLVMAddFunction(SL_Module,toplevel_llvm_name,void_fxn_type);
  LLVMBasicBlockRef toplevel_start=LLVMGetFirstBasicBlock(toplevel_fun);
  LLVMPositionBuilderAtEnd(SL_Builder,toplevel_start);
  LLVMValueRef codeVal=LLVM_Codegen(expr,cur_env,SL_Builder);
  //do something
  return NIL;
}
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
          return LLVM_Call_Builtin(expr,cur_env,builder);
        }
      } else if(SPECP(car(expr))){
        return LLVM_Codegen_Special(expr,cur_env,builder);
      } else {
        format_error_str("car of unquoted list is not a function or special form"
                         "\ncar is %s",print(car(expr)));

        goto ERROR;
      }
    case _sym:{
      symref tempsym;
      tempsym = getSym(cur_env,expr.val.var->name);
      if(tempsym){
        return EVAL(tempsym->val,cur_env,builder);
      } else {
        CORD_sprintf(&error_str,"undefined variable %r used",expr.val.var->name);
        goto ERROR;
      }
    }
    case _fun:
      //here down are all values which don't create a new basic block
    case _double:
      return LLVMConstReal(LispDouble,expr.val.real64);
    case _long:
      return LLVMConstInt(LispLong,expr.val.int64,0);
    case _char:
      return LLVMConstInt(LispWChar,expr.val.utf8_char,0);
    case _nil:
      return LispNIL;
    case _str:
      return LLVMConstString(CORD_to_char_star(expr.val.cord),CORD_len(expr.val.cord),0);
    case _array:
    case _error:
      error_val=expr;
      longjmp(jmp_to_error,-1);
    default:
      return LispNIL;
  }
 ERROR:
  return handle_error();
}
LLVMValueRef LLVM_Codegen_Special(sexp expr,env cur_env,
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
      LLVMValueRef symVal=LLVM_Codegen(caddr(expr),cur_env,builder);
      if(!newSym){
        //NEED TO MAKE GENERIC
        newSym=xmalloc(sizeof(global_symbol));
        newSym->name=(cadr(expr).val.var->name);
        newSym=addSym(cur_env,newSym);
        /*        newSym->val=symVal;
      } else {
        newSym->val=symVal;
        }*/
      //FIX
      //return (sexp){.tag = _sym,.val={.var = newSym}};
        return LispNIL;
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
}
LLVMValueRef LLVM_Codegen_if(sexp expr,env cur_env,
                                    LLVMBuilderRef builder){
  if(cdr(cdddr(expr)).tag != _nil){
    CORD_sprintf(&error_str,"excess arguments to if expression\n");
    return handle_error();
  } else {
    return LLVMBuildCondBr
      (builder,EVAL(cadr(expr),cur_env,builder),
       LLVMValueAsBasicBlock(EVAL(caddr(expr),cur_env,builder)),
       LLVMValueAsBasicBlock(EVAL(car(cdddr(expr)),cur_env,builder)));
  }
}
LLVMValueRef LLVM_Call_Function(sexp expr,env cur_env,
                                       LLVMBuilderRef builder){
  sexp curFun=car(expr).val.var->val;
  return LispNIL;
}
LLVMValueRef* get_args(sexp arglist,function fun,env cur_env,
                              LLVMBuilderRef builder){
  //arglist is (sexp . (sexp . (sexp ....()...)))
  int minargs=fun.min_args;int maxargs=fun.max_args;
  int i=0;
  LLVMValueRef *args;
  if(maxargs < 0){
    args=xmalloc(sizeof(LLVMValueRef)*(minargs+1));
    while(i<minargs && CONSP(arglist)){
      args[i++]=EVAL(XCAR(arglist),cur_env,builder);
      arglist=XCDR(arglist);
    }
    args[i]=EVAL(arglist,cur_env,builder);
    return args;
  }
  args=xmalloc(sizeof(maxargs));
  //fill in args
  while(CONSP(arglist) && (i < maxargs)){
    args[i++]=EVAL(XCAR(arglist),cur_env,builder);
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
LLVMValueRef LLVM_Codegen_progn(sexp expr,env cur_env,
                                       LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Codegen_do(sexp expr,env cur_env,
                                    LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Codegen_prog1(sexp expr,env cur_env,
                                       LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Codegen_while(sexp expr,env cur_env,
                                       LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Codegen_defun(sexp expr,env cur_env,
                                       LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Codegen_def(sexp expr,env cur_env,
                                     LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Codegen_lambda(sexp expr,env cur_env,
                                        LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Call_Builtin(sexp expr,env cur_env,
                                      LLVMBuilderRef builder){
  return LispNIL;
}
LLVMValueRef LLVM_Call_Lambda(sexp expr,env cur_env,
                                     LLVMBuilderRef builder){
  return LispNIL;
}
#undef EVAL
union hack{
  sexp as_sexp;
  long as_longs[2];
};
#ifdef _LLVM_TEST_
 int main(){
  initialize_llvm(1);
  LLVMValueRef hello_world_fn=LLVMGetNamedFunction(SL_Module,"hello_world");
  if(!hello_world_fn){
    HERE();
    return 0;
  }
  LLVMValueRef* hello_world_2=xmalloc(sizeof(LLVMValueRef));
  HERE();
  if(!LLVMFindFunction(SL_Engine,"hello_world",hello_world_2)){
    HERE();
    PRINT_FMT("%#0x",hello_world_2);
    HERE();
    LLVMRunFunction(SL_Engine,*hello_world_2,0,0);
    HERE();
    void(*f)()=LLVMGetPointerToGlobal(SL_Engine,*hello_world_2);
    HERE();
    f();
    HERE();

  }
  HERE();
  LLVMValueRef lispadd_fn=LLVMGetNamedFunction(SL_Module,"lisp_add");
  union hack two_sexp = {.as_sexp = {.tag=_long,.len=0,.meta=0,.val={.int64=2}}};
  LLVMGenericValueRef argval[2]={LLVMCreateGenericValueOfInt(LispLong,two_sexp.as_longs[0],0),
                               LLVMCreateGenericValueOfInt(LispLong,two_sexp.as_longs[1],0)};
  LLVMGenericValueRef args[4]={argval[0],argval[1],argval[0],argval[1]};
  HERE();
  LLVMGenericValueRef four_sexp=LLVMRunFunction(SL_Engine,lispadd_fn,4,args);
  HERE();
  long result=LLVMGenericValueToInt(four_sexp,0);
  void* ptr_result=LLVMGenericValueToPointer(four_sexp);
  printf("LLVMGenericValueRef = %#0x\nlong result = %#0x\npointer result = %#0x\n",
  four_sexp,result,ptr_result);
  return 0;
}
#endif
