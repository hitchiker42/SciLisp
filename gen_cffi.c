/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//this file exists outside of scilisp itself, it is a tool
//to parse a c header file (using libclang) and generate a file
//of SciLisp functions
#include "gen_cffi.h"
#include <assert.h>
CORD clangStringToCORD(CXString clangString){
  CORD retval=CORD_from_char_star(clang_getCString(clangString));
  clang_disposeString(clangString);
  return retval;
}
//will need to write a function to turn a CXTypeKind into an array offest
//and keep all SciLisp_Type structs in an array
SciLisp_Type *clangTypeToSciLispType(CXType ctype){
  switch(ctype.kind) {
    case CXType_Char:
      return &SciLisp_Int8;
    case CXType_UChar:
      return &SciLisp_UInt8;
    case CXType_Short:
      return &SciLisp_Int16;
    case CXType_UShort:
      return &SciLisp_UInt16;
    case CXType_Int:
      return &SciLisp_Int32;
    case CXType_UInt:
      return &SciLisp_UInt32;
    case CXType_Long:
      return &SciLisp_Int64;
    case CXType_ULong:
      return &SciLisp_UInt64;
    case CXType_Float:
      return &SciLisp_Float;
    case CXType_Double:
      return &SciLisp_Double;
    case CXType_Pointer:
      return Make_SciLisp_Pointer1(build_pointer_val(ctype));
    case CXType_ConstantArray:
      return Make_SciLisp_Array1(build_array_val(ctype));
    case CXType_Record:
      //I'll implement this in a bit
      return NULL;
    default:
      return NULL;
  }
}
//I don't know how to do this yet, so I'm making it a function
//to fill in later
CORD clang_Cursor_getFunctionName(CXCursor cursor){
  return clangStringToCORD(clang_getCursorSpelling(cursor));
}
CORD clang_getFunctionName(SciLisp_Function curFun){
  return clangStringToCORD(clang_getCursorSpelling(curFun.function_decl));
}
enum CXChildVisitResult
clang_test(CXCursor cursor,CXCursor parent,CXClientData data){
  puts("Cursor Spelling");
  CORD_printf
    (clangStringToCORD
     (clang_getCursorSpelling(cursor)));
  printf("\nType Kind: %d\n",clang_getCursorType(cursor).kind);
  puts("\nLiteral type");
  (CORD_printf
   (clangStringToCORD
    (clang_getTypeSpelling
     (clang_getCursorType(cursor)))));
  puts("\nCanonical type");
  (CORD_printf
   (clangStringToCORD
    (clang_getTypeSpelling
     (clang_getCanonicalType
      (clang_getCursorType(cursor))))));
    if(clang_isPointerType(cursor)){
      puts("\npointee type");
        (CORD_printf
         (clangStringToCORD
          (clang_getTypeSpelling
           (clang_getPointeeType
            (clang_getCursorType(cursor))))));
    }
  puts("\n---------------------");
  return CXChildVisit_Recurse;
}
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
      (CORD_printf
       (clangStringToCORD
        (clang_getTypeSpelling
         (clang_getCanonicalType
          (clang_getCursorType(cursor))))));
      return CXChildVisit_Continue;
    }
    SL_data->num_fxns++;
    SL_data->tail->next=curFun;//set next pointer for current tail;
    curFun->prev=SL_data->tail;//set prev pointer for curFun
    SL_data->tail=curFun;//set tail to curFun
    (CORD_printf
     (clangStringToCORD
      (clang_getTypeSpelling
       (clang_getCanonicalType
        (clang_getCursorType(SL_data->tail->function_decl))))));
    return CXChildVisit_Continue;
  }
}
//NOTE: function arguments start at 1 not 0
CORD makeSciLispFunction(SciLisp_Function *data){
  CORD lfun;//lisp_function
  CORD declarations="";
  CORD typechecks="";
  CORD funcall="";
  cur_function_name=clang_getFunctionName(*data);
  CXType funType=clang_getCursorType(data->function_decl);
  int numArgs=clang_getNumArgTypes(funType);
  int i;
  CXType argType;
  SciLisp_Type* curType;
  lfun=buildSciLispName(funType);
  CORD_fprintf(stderr,lfun);
  //everything should work fine if we have no args, I think
  for(i=1;(i<=numArgs || (numArgs==0 && i==1));i++){
    argType=clang_getArgType(funType,i);
    curType=clangTypeToSciLispType(argType);
    if(!curType){HERE();
      continue;}
    typechecks=buildTypeChecks(typechecks,curType,i,numArgs,funType);
    CORD_fprintf(stderr,typechecks);
    declarations=buildDeclarations(declarations,curType,i);
    CORD_fprintf(stderr,declarations);
    funcall=buildFunCall(funcall,i,numArgs,funType);
    CORD_fprintf(stderr,funcall);
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
                      cur_function_name,"(");
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
    funcall=CORD_cat(funcall,", ");
    return funcall;
  }
}
//build a cord of the form:
//ctype c_obj<argnum> = (obj<argnum>.val.fieldname|fxnAccessor(obj<argnum>));
CORD buildDeclarations(CORD decls,SciLisp_Type *curType,int curArg){
  //this one's a bit easy, theres nothing special about the first or last
  //argument
  decls=CORD_cat(decls,curType->ctype);
  decls=CORD_cat(decls," c_obj");
  decls=CORD_cat_char(decls,(char)(curArg+48));
  if(curType->useFxnAccessor){
    HERE();
    decls=CORD_catn(4,decls,"= ",curType->fxnAccessor,"(obj");
    HERE();
    decls=CORD_cat_char(decls,(char)(curArg+48));
    HERE();
    decls=CORD_cat(decls,");\n");
    return decls;
  } else {
    decls=CORD_cat(decls,"= obj");
    decls=CORD_cat_char(decls,(char)(curArg+48));
    decls=CORD_cat(decls,".val.");
    decls=CORD_cat(decls,curType->fieldName);
    decls=CORD_cat(decls,";\n");
    HERE();
    CORD_fprintf(stderr,decls);
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
                     cur_function_name,"\");\n}");
    return checks;//not that there's much of a point to this;
  } else {
    checks=CORD_cat(checks,"|| ");
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
  retval=CORD_catn(4,"sexp ","lisp_",cur_function_name,"(");
  if(numArgs==0){
    retval=CORD_cat(retval,"){\n");
    return retval;
  }
  for(i=1;i<numArgs;i++){
    retval=CORD_cat(retval,"sexp obj");
    retval=CORD_cat_char(retval,(char)(i+48));
    retval=CORD_cat(retval,", ");
  }
  retval=CORD_cat(retval,"sexp obj");
  retval=CORD_cat_char(retval,(char)(i+48));
  retval=CORD_cat(retval,"){\n");
  return retval;
}
struct compound_type_info  build_pointer_val(CXType pointer_type){
  struct compound_type_info retval;
  int acc=0;
  while(pointer_type.kind==CXType_Pointer){
    pointer_type=clang_getPointeeType(pointer_type);
    acc++;
  }
  //I'll check for 1 level of inderection on structs elsewhere
  //so if we get here it means we have at least 2 *'s
  if(pointer_type.kind=CXType_Record){acc--;}
  retval.depth=acc;
  retval.subtype=get_std_type(pointer_type);
  return retval;
}
struct compound_type_info  build_array_val(CXType array_type){
  struct compound_type_info retval;
  int len=clang_getArraySize(array_type);
  CORD subtype=
    clangStringToCORD
    (clang_getTypeSpelling
     (clang_getCanonicalType
      (clang_getArrayElementType(array_type))));
  return (struct compound_type_info){.length=len,.subtype=subtype};
}
SciLisp_Type *Make_SciLisp_Array(int length,CORD subtype){
  CORD cname,typecheck_name,typecheck_macro;
  SciLisp_Type *retval=xmalloc(sizeof(SciLisp_Type));
  CORD_sprintf(&cname,"%s[%d]",subtype,length);
  CORD_sprintf(&typecheck_name,"array_typecheck_%d_%s",
                 length,subtype);
  CORD_sprintf(&typecheck_macro,
               "#define array_typecheck_%d_%s(_arr_)"
               " array_typecheck(%d,_arr_,_%s_array)",
               length,subtype,length,subtype);
  *retval=(SciLisp_Type)make_SciLisp_Type(ConstantArray,cname,typecheck_name,
                                          "arr","_array",typecheck_macro,0);
  return retval;
}
SciLisp_Type *Make_SciLisp_Array1(struct compound_type_info info){
  return Make_SciLisp_Array(info.length,info.subtype);
}
SciLisp_Type *Make_SciLisp_Pointer(int depth,CORD subtype){
  CORD cname,typecheck_name,typecheck_macro;
  SciLisp_Type *retval=xmalloc(sizeof(SciLisp_Type));
  cname=subtype;
  int i;
  for(i=0;i<depth;i++){
    cname=CORD_cat_char(cname,'*');
  }
  CORD_sprintf(&typecheck_name,"pointer_typecheck_%d_%s",
                 depth,subtype);
  CORD_sprintf(&typecheck_macro,
               "#define pointer_typecheck_%d_%s(_ptr_)"
               " pointer_typecheck(%d,_ptr_,_ctype_%s)",
               depth,subtype,depth,subtype);
  *retval=(SciLisp_Type)make_SciLisp_Type(Pointer,cname,typecheck_name,
                                          "c_ptr","_cptr",typecheck_macro,0);
  return retval;
}
SciLisp_Type *Make_SciLisp_Pointer1(struct compound_type_info info){
  return Make_SciLisp_Pointer(info.depth,info.subtype);
}
CORD get_std_type(CXType type){
  switch(clang_getCanonicalType(type).kind){
    case CXType_SChar:
      return "int8";
    case CXType_Short:
      return "int16";
    case CXType_Int:
      return "int32";
    case CXType_Long:
      return "int64";
    case CXType_UChar:
      return "uint8";
    case CXType_UShort:
      return "uint16";
    case CXType_UInt:
      return "uint32";
    case CXType_ULong:
      return "uint64";
    case CXType_Float:
      return "real32";
    case CXType_Double:
      return "real64";
    default:
      return clangStringToCORD
        (clang_getTypeSpelling
         (clang_getCanonicalType(type)));
  }
}
int main(int argc,char *argv[]){
  //get filenames, if only inputfile name output file lisp_filename.c
  inFileName=argv[1];//placeholder for argument parsing
  CORD outFile_cord;
  CORD_sprintf(&outFile_cord,"lisp_%s.c",
               CORD_substr(inFileName,0,CORD_len(inFileName)-2));
  outFile=fopen(CORD_to_char_star(outFile_cord),"w");
  //get AST from Clang
  CXIndex clang_index=clang_createIndex(1,0);
  CXTranslationUnit clang_trans_unit=clang_parseTranslationUnit
    (clang_index,inFileName,NULL,0,NULL,0,CXTranslationUnit_SkipFunctionBodies);
  if(!clang_trans_unit){
    fprintf(stderr,"clang failed\n");
    return 1;
  }
  CXCursor entry_point=clang_getTranslationUnitCursor(clang_trans_unit);
  SciLisp_Data *function_list=xmalloc(sizeof(SciLisp_Data));
  SciLisp_Function *curFun;
  CORD SciLisp_Code;
  CORD_sprintf(&SciLisp_Code,
               "%s//this file was created using %s\n",SciLisp_Header,inFileName);
  CORD_printf
    (clangStringToCORD
     (clang_getCursorDisplayName(entry_point)));
  //test code
  //  clang_visitChildren(entry_point,clang_test,NULL);
  //  return 0;
  //collect up the function definations
  clang_visitChildren(entry_point,clang_find_functions,function_list);
  if(!function_list || function_list->num_fxns==0){
    fprintf(stderr,"We Failed somehow\n");
    return 1;
  }
  HERE();
  int i=0;
  curFun=function_list->head;
  HERE();
  CORD_fprintf(stderr,SciLisp_Code);
  SciLisp_Code=CORD_cat(SciLisp_Code,makeSciLispFunction(curFun));
  while(curFun->next) {
    CORD_fprintf(stderr,SciLisp_Code);
    curFun=curFun->next;
    SciLisp_Code=CORD_cat(SciLisp_Code,makeSciLispFunction(curFun));
  }
  CORD_put(SciLisp_Code,outFile);
  return 0;
}
