/* -*- c-syntactic-indentation: nil; electric-indent-inhibit: t -*- */
/* The reader, uses flex

Copyright (C) 2014 Tucker DiNapoli
This file is part of SciLisp.

SciLisp is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SciLisp is distributed in the hope that it will be useful,
35but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SciLisp.  If not, see <http://www.gnu.org*/
%{
/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#define IN_LEXER
#include "common.h"
/*#include "prim.h"*/
#include "unicode.h"
#include "read.h"
#include "cons.h"
#ifdef YY_DECL
#undef YY_DECL
#endif
#define YY_DECL TOKEN yylex(sexp *yylval,yyscan_t yyscanner)
#define YYSTYPE sexp
static int comment_depth=0;
static thread_local int backtick_flag=0;
#define FORMAT_READ_ERR(format,args...)       \
  ({CORD read_err_string;                     \
    CORD_sprintf(&read_err_string,            \
    format,##args);                           \
    read_err_string;})
%}
 /*FLEX COMMENT RULES:
   Comment needs to start after a space on a line;
   Comments can't be on the same line as a defination
   Need to use / * * / comments  */
 /*backslashes and quotes are refered to using ascii escapes
  \x5c == \ & \x22 == " & \x27 == '
  mostly so unmatched quotes don't fuck up syntax highlighing*/
 /*Unicode support, basic idea borrowed from the lexer for
  *the txr by kaz kylheku language*/
ASC     [\x00-\x7f]{-}[?\\] 
ASC_FULL [\x00-\x7f]
ASCSTR  [\x00-\x21\x23-\x7f]
 /*explaination of special characters:
  * : reserved for keywords, pacakge access and type annotations
  * ; reserved for comments
  * (){}[] reserved delimiers, () should be obvious, [] for arrays and {} for future use
  * ' " ' quotes, obviously
  * # for quoting and delimiting special symbols(#t,#f) and deliberately unreaable symbols
  * . reserved for pairs and package access
  * | nothing now but reserved for later (quoting ids?)
  * @ for ,@ in macros
  * , for macros
  * Starting special characters ? for characters, +-[0-9] for numbers
  */
DIGIT [0-9]
XDIGIT [0-9a-fA-F]
 /*                                  '   "    [  \   ]        space*/
ASC_ID  [\x00-\x7f]{-}[#|,.;:`(){}@\x27\x22\x5b\x5c\x5d\x20\t\n]
ASCN    [\x00-\t\v-\x7f]
U       [\x80-\xbf]
U2      [\xc2-\xdf]
U3      [\xe0-\xef]
U4      [\xf0-\xf4]
UANY    {ASC}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UANYN   {ASCN}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UONLY   {U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UCHAR "?"("\\"[?nt]|"\\\\"|"\\x"([[:xdigit:]]{1,2})|"\\u"([[:xdigit:]]{1,4})|"\\U"([[:xdigit:]]{1,8})|{UANY})
USTR    {ASCSTR}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UID {ASC_ID}|{UONLY}
ID ({UID}|("\\".))+
QUOTED_ID ("|"({UANY}|'\\\\'|'\\|')+"|")
KEYSYM ":"{ID}
TYPENAME "::"{ID}
QUALIFIED_ID {ID}":"{ID}
QUALIFIED_ID_ALL {ID}":."{ID}
%option noyywrap
   /*start condition for scanning nested comments*/
%x comment
%option noyyalloc noyyrealloc noyyfree
%option 8bit reentrant
%option header-file="read_lex.h" outfile="read.c"
%%   /*Literals*/
 /*numbers need to come first so something like +1 gets read as an integer
  but something like 1+ gets read as a symbol*/
[+\-]?{DIGIT}+ {LEX_MSG("lexing int");
  *yylval=long_sexp((long)strtol(yytext,NULL,0));return TOK_INT;}
[+\-]?"0"[xX][[:xdigit:]]+ {LEX_MSG("lexing hex int");
  *yylval=long_sexp((long)strtol(yytext,NULL,0));return TOK_INT;}
[+\-]?{DIGIT}+"."{DIGIT}* {LEX_MSG("lexing real");
  *yylval=real64_sexp(strtod(yytext,NULL));return TOK_REAL;}
[+\-]?{DIGIT}+"."?{DIGIT}*[eE][+-]?{DIGIT}+ {LEX_MSG("lexing real");
  *yylval=real64_sexp(strtod(yytext,NULL));return TOK_REAL;}
    /*String Literal a quote, followed by either a literal \"
   or anything that isnt a " repeated 1 or more times, followed by another quote.*/
"\""([\x00-0x7f]|"\\""\"")+"\"" {LEX_MSG("Lexing string");
  *yylval=cord_sexp(CORD_strdup(CORD_substr(yytext,1,CORD_len(yytext)-2)));
  return TOK_STRING;}
"\""([\x00-\x21\x23-\x7f]|{UONLY}|"\\""\"")+"\"" {LEX_MSG("Lexing multibyte string");
  *yylval=cord_sexp(CORD_strdup(CORD_substr(yytext,1,CORD_len(yytext)-2)));
  return TOK_STRING;}
"\"\"" {*yylval=cord_sexp(0);return TOK_STRING;}
{UCHAR} {LEX_MSG("lexing char");
    wint_t new_char;
    if(lex_char(yytext+1,&new_char)<0){
      return TOK_UNKN;
    } else {
      *yylval=uchar_sexp(new_char); return TOK_CHAR;
    }
  }
{ID} {LEX_MSG("lexing symbol");
    symbol *sym=c_intern(yytext,yyleng,NULL);
    *yylval=symref_sexp(sym);
    return TOK_SYMBOL;
   }
{KEYSYM} {LEX_MSG("lexing keyword symbol");CORD name=CORD_from_char_star(yytext);
   symbol *sym=c_intern(yytext,yyleng,NULL);
   *yylval=symref_sexp(sym);
    return TOK_KEYSYM;}
{QUALIFIED_ID} {LEX_MSG("lexing qualified id");
  /*know we've got a colon, so we can use rawmemchr*/
  char *colon=rawmemchr(yytext,':');
  symbol *package=c_intern(yytext,(uint32_t)(colon-yytext),NULL);
  if(!ENVP(package->val)){
    /*yylval=format_error_sexp("unknown package %s",package->name.name);*/
    return TOK_ERR;
  }
  symbol_name *sym_name=make_symbol_name(colon+1,(uint32_t)((yytext+yyleng)-(colon+1)),0);
  symbol *sym=obarray_lookup_sym(sym_name,package->val.val.ob);
  if(!sym){
    raise_simple_error(Eundefined,FORMAT_READ_ERR
      ("value %s not found in package %s",sym_name->name,package->name->name));
  }
  if(sym->visibility == 2){
    /*use Evisibility once I define it*/
    raise_simple_error(Eundefined,FORMAT_READ_ERR
    ("value %s not externally visable in package %s (use :. to override visibility)",
      sym_name->name,package->name->name));
   }
   *yylval=symref_sexp(sym);
   return TOK_ID;
   }
{QUALIFIED_ID_ALL} {LEX_MSG("lexing qualified id");
  /*know we've got a colon, so we can use rawmemchr*/
  char *colon=rawmemchr(yytext,':');
  assert(*(colon+1) == '.');
  symbol *package=c_intern(yytext,(uint32_t)(colon-yytext),0);
  if(!ENVP(package->val)){
    raise_simple_error(Eundefined,
    FORMAT_READ_ERR("unknown package %s",package->name->name));
  }
  symbol_name *sym_name=make_symbol_name(
    colon+2,(uint32_t)((yytext+yyleng)-(colon+2)),0);
  symbol *sym=obarray_lookup_sym(sym_name,package->val.val.ob);
  if(!sym){
    raise_simple_error(Eundefined,FORMAT_READ_ERR("value %s not found in package %s",
      sym_name->name,package->name->name));
  }
  *yylval=symref_sexp(sym);
  return TOK_ID;
  }
{TYPENAME} {LEX_MSG("lexing typename");
  symbol *type_sym=lookup_symbol_global(yytext+2);
  if(!TYPEP(type_sym->val)){
    raise_simple_error_fmt(Etype,"Unknown type %s",yytext+2);
  }
  *yylval=type_sym->val;
  return TOK_TYPEINFO;
  }
  /*
{ID}{TYPENAME} {LEX_MSG("lexing typed id");
    char *colon=rawmemchr(yytext,':');
    assert(*(colon+1) == ':');
    symbol sym=c_intern(yytext,(uint32_t)(colon-yytext),NULL);
    }
    */
<comment,INITIAL>"#|" {LEX_MSG("lexing open comment");
  if(YY_START != comment){BEGIN(comment);}comment_depth+=1;}
<comment,INITIAL>"|#" {LEX_MSG("lexing close comment"); comment_depth-=1;
  if(comment_depth == 0){BEGIN(0);}}
<comment>[^#|]+
<comment>"#"[^|}|"|"[^#]
"quasiquote"|"`" {LEX_MSG("lexing backquote");return TOK_BACKQUOTE;}
"#" {LEX_MSG("HASH");return TOK_HASH;}
"#t" {*yylval=LISP_TRUE; return TOK_SYMBOL;}
"#f" {*yylval=LISP_FALSE; return TOK_SYMBOL;}
 /*uninterned symbol*/
"#:"{ID} {struct symbol_name *name=make_symbol_name(yytext+2,yyleng-2,0);
          symbol *sym=xmalloc(sizeof(struct symbol));
          sym->name=name;
          sym->val=UNBOUND; /*define this at some point*/
          sym->next=NULL;
          sym->plist=Fcons(Tuninterned_sexp,NIL);
          *yylval=symref_sexp(sym);
          return TOK_SYMBOL;
         }
 /*"#"{ID} {LEX_MSG("HASH-ID");return TOK_HASH;}*/
"(" {LEX_MSG("lexing (");return TOK_LPAREN;}
")" {LEX_MSG("lexing )");return TOK_RPAREN;}
  /*Maybe use the syntax #('['|'[['|'[|') for something*/
"[" {LEX_MSG("lexing [");return TOK_LBRACE;}
"[[" {LEX_MSG("lexing [[");return TOK_DBL_LBRACE;}
"[|" {LEX_MSG("lexing [|");return TOK_MAT_OPEN;}
"]" {LEX_MSG("lexing ]");return TOK_RBRACE;}
"]]" {LEX_MSG("lexing ]]");return TOK_DBL_RBRACE;}
"|]" {LEX_MSG("lexing |]");return TOK_MAT_CLOSE;}
"{" {LEX_MSG("lexing {");return TOK_LCBRACE;}
"}" {LEX_MSG("lexing }");return TOK_RCBRACE;}
"." {LEX_MSG("lexing .");return TOK_DOT;}
":" {LEX_MSG("lexing :");return TOK_COLON;}
"@" {LEX_MSG("lexing @");return TOK_STRUDEL;}
"'" {LEX_MSG("Lexing quote");return TOK_QUOTE;}
"," {LEX_MSG("Lexing comma");return TOK_COMMA;}
[ \t\n]+ /*whitespace*/
";"[^\n]* /*one line comments*/
<<EOF>> return -1;
. {LEX_MSG("unknown token");
  PRINT_FMT("Error, unknown token, value %d, character %c",
            yytext[0],yytext[0]);
  *yylval=uchar_sexp(btowc(yytext[0]));
  return TOK_UNKN;}

%%
#include "lex_read.c"
