/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#ifndef GEN_CFFI_H
#define GEN_CFFI_H
#include <clang-c/Index.h>
#include <stdlib.h>
#include <stdio.h>
#include "gc/include/gc/gc.h"
#include "gc/include/gc/cord.h"
#define clang_isFunctionDecl(_cursor)                           \
  (clang_getCursorType(_cursor).kind== CXType_FunctionProto)
#define clang_isPointerType(_cursor)                    \
  (clang_getCursorType(_cursor).kind == CXType_Pointer)
#define xmalloc GC_MALLOC
#define CXType_Char CXType_SChar
#define mk_int_n_type(clang_name,length)                                \
  make_SciLisp_Type(clang_name,int##length##_t,INT##length##P,              \
                    int##length,_int##length,"",0)
#define mk_uint_n_type(clang_name,length)                               \
  make_SciLisp_Type(clang_name,uint##length##_t,UINT##length##P,            \
                    uint##length,_uint##length,"",0)
//generate types for both signed and unsigned integers of length length
#define mk_both(clang_name,length)                                      \
  SciLisp_Type SciLisp_Int##length=mk_int_n_type(clang_name,length);    \
  SciLisp_Type SciLisp_UInt##length=mk_uint_n_type(U##clang_name,length)
#define make_SciLisp_Type(_clang_type,_ctype,_typecheck,_fieldName      \
                          ,_tagName,_fxnAccessor,_useFxnAccessor)       \
  {.clang_type=CXType_##_clang_type,.ctype=#_ctype,.typecheck=#_typecheck, \
      .fieldName=#_fieldName,.tagName=#_tagName,.fxnAccessor=_fxnAccessor, \
      .useFxnAccessor=_useFxnAccessor}
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
typedef struct SciLisp_Data SciLisp_Data;
typedef struct SciLisp_Function SciLisp_Function;
typedef struct SciLisp_Type SciLisp_Type;
struct compound_type_info;
CORD get_std_type(CXType type);
/*
enum    CXTypeKind {
  CXType_Invalid = 0, CXType_Unexposed = 1, CXType_Void = 2, CXType_Bool = 3,
  CXType_Char_U = 4, CXType_UChar = 5, CXType_Char16 = 6, CXType_Char32 = 7,
  CXType_UShort = 8, CXType_UInt = 9, CXType_ULong = 10,
  CXType_Char_S = 13, CXType_SChar = 14, CXType_WChar = 15,
  CXType_Short = 16, CXType_Int = 17, CXType_Long = 18, 
  CXType_Float = 21, CXType_Double = 22, CXType_NullPtr = 24
  CXType_Pointer = 101, CXType_BlockPointer = 102, CXType_LValueReference = 103,
  CXType_RValueReference = 104, CXType_Record = 105, CXType_Enum = 106, CXType_Typedef = 107,
  CXType_ObjCInterface = 108, CXType_ObjCObjectPointer = 109, CXType_FunctionNoProto = 110, CXType_FunctionProto = 111,
  CXType_ConstantArray = 112, CXType_Vector = 113, CXType_IncompleteArray = 114, CXType_VariableArray = 115,
  CXType_DependentSizedArray = 116, CXType_MemberPointer = 117
  }*/
#define NUM_SCILISP_TYPES 13
#define SCILISP_TYPES_ARRAY_SIZE 16
struct SciLisp_Type {
  enum CXTypeKind clang_type;
  CORD ctype;
  CORD typecheck;
  CORD fieldName;
  CORD tagName;
  CORD fxnAccessor;//function to get ctype (ie getDoubleVal for long/doubles)
  int useFxnAccessor;
};

SciLisp_Type SciLisp_Double=make_SciLisp_Type
  (Double,double,NUMBERP,real64,_double,"getDoubleValUnsafe",1);
SciLisp_Type SciLisp_Float=make_SciLisp_Type
  (Double,double,FLOATP,real32,_float,"",0);
SciLisp_Type SciLisp_Long=make_SciLisp_Type
  (Long,long,INTP,int64,_long,"",0);
mk_both(Char,8);
mk_both(Short,16);
mk_both(Int,32);
mk_both(Long,64);
struct compound_type_info {
  union {
    int length;
    int depth;
  };
  CORD subtype;
};
struct SciLisp_Data {
  int num_fxns;
  SciLisp_Function *head;
  SciLisp_Function *tail;
};
struct SciLisp_Function {
  SciLisp_Function *prev;
  CXCursor function_decl;
  SciLisp_Function *next;
};
CORD clangStringToCORD(CXString clangString);
SciLisp_Type *clangTypeToSciLispType(CXType ctype);
CORD clang_getFunctionName(SciLisp_Function);
CORD buildSciLispName(CXType function);
CORD buildDeclarations(CORD decls,SciLisp_Type *curType,int curArg);
CORD buildFunCall(CORD funcall,int curArg,int maxArg,CXType funType);
CORD buildTypeChecks(CORD checks,SciLisp_Type *curType,int curArg,
                     int maxArg,CXType funType);
enum CXChildVisitResult
clang_test(CXCursor cursor,CXCursor parent,CXClientData data);
enum CXChildVisitResult
clang_find_functions(CXCursor cursor,CXCursor parent,CXClientData data);
struct compound_type_info  build_pointer_val(CXType pointer_type);
struct compound_type_info  build_array_val(CXType array_type);
SciLisp_Type *Make_SciLisp_Pointer1(struct compound_type_info info);
SciLisp_Type *Make_SciLisp_Array1(struct compound_type_info info);
SciLisp_Type *Make_SciLisp_Pointer(int depth,CORD subtype);
SciLisp_Type *Make_SciLisp_Array(int depth,CORD subtype);
SciLisp_Type *SciLisp_Types;
static const char *SciLisp_Header=
"/*This is an automatically generated file, do not edit\n"
  "This file was created using SciLisp_gen_cffi*/\n"
  "#include \"scilisp.h\"\n";
static char* inFileName;
static FILE* outFile;
static int counter=0;
static CORD cur_function_name;
#endif
