/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//this file exists outside of scilisp itself, it is a tool
//to parse a c header file (using libclang) and generate a file
//of SciLisp functions
#include <clang-c/index.h>
#include <stdlib.h>
#include <stdio.h>
#include "gc/incldue/gc/gc.h"
#include "gc/incldue/gc/cord.h"
#define clang_isFunctionDecl(_cursor)                           \
  (clang_getCursorKind(_cursor) == CXCursor_FunctionDecl)
#define xmalloc GC_MALLOC
CORD clangStringToCORD(CXString clangString){
  CORD retval=CORD_from_char_star(clang_getCString(clangString));
  clang_disposeString(clangString);
  return retval;
}
typedef struct SciLisp_Data SciLisp_Data;
typedef struct SciLisp_Function SciLisp_Function;
typedef struct SciLisp_Type SciLisp_Type;
//still need to make all of these
struct SciLisp_Type {
  enum CXTypeKind clang_type;
  CORD ctype;
  CORD typecheck;
  CORD fieldName;
  CORD tagName;
  CORD fxnAccessor;//function to get ctype (ie getDoubleVal for long/doubles)
  int useFxnAccessor;
};
struct SciLisp_Data {
  int num_fxns;
  SciLisp_Function *head;
  SciLisp_Function *tail;
}
struct SciLisp_Function {
  SciLisp_Function *prev;
  CXCursor function_decl;
  SciLisp_Function *next;
};
//will need to write a function to turn a CXTypeKind into an array offest
//and keep all SciLisp_Type structs in an array
SciLisp_Type *clangTypeToSciLispType(enum CXTypeKind ctype);
//I don't know how to do this yet, so I'm making it a function
//to fill in later
CORD clang_Cursor_getFunctionName(CXCursor);
CORD clang_getFunctionName(CXType);
char* inFileName;
FILE* outFie;
const char *SciLisp_Header=
"/*This is an automatically generated file, do not edit"
  "This file was created using SciLisp_gen_cffi*/"
  "#include \"scilisp.h\"\n"
//collect a list of all function definations in the ast pointed to by cursor
enum CXChildVisitResult
clang_find_functions(CXCursor cursor,CXCursor parent,CXClientData data){
  if(!clang_isFunctionDecl(cursor)){
    return CXChildVisit_Continue;
  } else {
    SciLisp_Data* SL_data=(SciLisp_Data*)data;
    SciLisp_Function *curFun=xmalloc(sizeof(SciLisp_Function));
    curFun->function_decl=cursor;
    if(SL_data->num_fxns==0){
      SL_data->head=curFun;
      SL_data->tail=curFun;
      SL_data->num_fxns++;
      return CXChildVisit_Continue;
    }
    SL_data->num_fxns++;
    SL_data->tail->next=curFun;//set next pointer for current tail;
    curFun->prev=SL_data->tail;//set prev pointer for curFun
    SL_date->tail=curFun;//set tail to curFun
    return CXChildVisit_Continue;
  }
}
//NOTE: function arguments start at 1 not 0
CORD makeSciLispFunction(SciLisp_Function *data){
  CORD lfun;//lisp_function
  CORD declarations="";
  CORD typechecks="";
  CORD funcall="";
  CXType funType=clang_getCursorType(data->function_decl);
  int numArgs=clang_getNumArgTypes(data->function_decl);
  int i;
  SciLisp_Type* curType;
  CXType argType;
  lfun=buildSciLispName(fun);
  //everything should work fine if we have no args, I think
  for(i=1;(i<=numArgs || (numArgs==0 && i==1)),i++){
    argType=clang_getArgType(funType,i);
    curType=clangTypeToSciLispType(argType);
    buildTypeChecks(typechecks,curType,i,numArgs,funType);
    buildDeclarations(declarations,curType,i);
    buildFunCall(funcall,i,numArgs,funType);
  }
  lfun=CORD_catn(4,lfun,typechecks,declarations,funcall);
  return lfun;
}
//build a cord of the form:
//return rettype_sexp(lisp_funname(obj<argnum>*));
CORD buildFunCall(CORD funcall,int curArg,
                  int maxArg,CXType funType){
  if(curArg==0){
    SciLisp_Type *rettype=clangTypeToSciLispType(clang_getResultType(funType));
    funcall=CORD_catn(6,funcall,"return ",rettype->ctype,"_sexp(",
                      clang_getFunctionName(funType),"(");
    if(maxArg==0){
      funcall=CORD_cat(funcall,"));\n}\n");
      return funcall;
    }
  }
  funcall=CORD_cat(funcall,"c_obj");
  CORD_cat_char(funcall,(char)(curArg+48));
  if(curArg==maxArg){
    funcall=CORD_cat(funcall,"));\n}\n");
    return funcall;
  } else {
    funcall=CORD_cat(", ");
    return funcall;
  }
}
//build a cord of the form:
//ctype c_obj<argnum> = (obj<argnum>.val.fieldname|fxnAccessor(obj<argnum>));
CORD buildDeclarations(CORD decls,SciLisp_Type *curType,int curArg){
  //this one's a bit easy, theres nothing special about the first or last
  //argument

  decls=CORD_catn(3,decls,curType->ctype," c_obj");
  decls=CORD_cat_char(decls,(char)(curArg+48));
  if(curType->useFxnAccessor){
    decls=CORD_catn(decls,"= ",curType->fxnAccesor,"(obj");
    decls=CORD_cat_char(decls,(char)(curArg+48));
    decls=CORD_cat(");\n");
    return decls;
  } else {
    decls=CORD_cat(decls,"= obj");
    decls=CORD_cat_char(decls,(char)(curArg+48));
    decls=CORD_catn(4,decls,".val.",curType->fieldName,";\n");
    return decls;
  }
}
//create a CORD with the final form:
//if (TypeCheck(obj<argname>)||* TypeCheck(obj<maxArg>)){
//return error_sexp("...");\n}
CORD buildTypeChecks(CORD checks,SciLisp_Type *curType,int curArg,
                     int maxArg,CXType funType){
  if(curArg==1 && maxArg){
    checks="if (";
  }
  checks=CORD_catn(3,checks,curType->typecheck,"(obj");
  checks=CORD_cat_char(checks,(char)(curArg+48));
  checks=CORD_cat_char(checks,')');
  if(curArg==maxArg){
    checks=CORD_catn(4,checks,"){\n return error_sexp(\"type error in ",
                    clang_getFunctionName(funType),"\");\n}");
    return checks;//not that there's much of a point to this;
  } else {
    checks=CORD_cat(checks,")|| ");
    return checks;
  }
}

CORD buildSciLispName(CXType function){
  CORD retval;
  int numArgs=clang_getNumArgTypes(function);
  // variadic arguments not yet implemented
  //  int isVariadic=clang_isFunctionTypeVariadic(function);
  //  int maxArgs=numArgs+isVariadic;
  int i;
  retval=CORD_catn(4,"sexp ","lisp_",clang_getFunctionName(function),"(");
  if(numArgs==0){
    retval=CORD_cat(retval,"){\n");
    return retval;
  }
  for(i=1;i<numArgs;i++){
    retval=CORD_cat(retval,"sexp obj");
    retval=CORD_cat_char(retval,(char)(i+48));
    retval=CORD_cat_char(retval,", ");
  }
  retval=CORD_cat(retval,"sexp obj");
  retval=CORD_cat_char(retval,(char)(i+48));
  retval=CORD_cat(retval,"){\n");
  return retval;
}
int main(int argc,char *argv[]){
  //get filenames, if only inputfile name output file lisp_filename.c
  inFileName=argv[1];//placeholder for argument parsing
  //get AST from Clang
  CXIndex clang_index;
  CXTranslationUnit clang_trans_unit=clang_parseTranslationUnit
    (clang_index,inFileName,NULL,0,NULL,0,CXTranslationUnit_SkipFunctionBodies);
  CXCursor entry_point=clang_getTranslationUnitCursor(clang_trans_unit);
  SciLisp_Data *function_list=xmalloc(sizeof(SciLisp_Data));
  SciLispFunction *curFun;
  CORD SciLisp_Code;
  CORD_sprintf(&SciLisp_Code,
               "%s//this file was created using %s\n",SciLisp_Header,inFileName);  
  //collect up the function definations
  clang_visitChildren(entry_point,clang_find_functions,function_list);
  if(function_list->num_args==0){
    //print some error message;
    return 1;
  }
  int i=0;
  curFun=function_list->head;
  SciLisp_Code=CORD_cat(SciLisp_Code,makeSciLispFunction(curFun));
  while(curFun->next) {
    curFun=curFun->next;
    SciLisp_Code=CORD_cat(SciLisp_Code,makeSciLispFunction(curFun));
  }
  CORD_put(SciLisp_Code,outFile);
  return 0;
}
  
