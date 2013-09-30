%{
  #include "common.h"
#define YY_DECL TOKEN yylex(void)
%}
DIGIT [0-9]
/*Identifiers are normal id chars and start %,+,*,!,?,-,^,$,/,&,<,> end*/
ID [A-Za-z%+*!?\-_^$/&<>][A-Za-z%+*!?\-_^$&<>0-9]*
TYPENAME "::"[A-z_a-z][A-Z_a-z0-9]*
QUOTE "'"|quote
/*
union data {
  double real64;
  long int64;
  wchar utf8_char;
  char* string;
  cons* pair;
  symref* var;//incldues functions
};
*/
/*%option bison-bridge*/
%option header-file="lex.yy.h"
%option noyywrap
%%
   /*Literals*/
{DIGIT}+ {LEX_MSG("lexing int");yylval->tag=_long;
         yylval->val.int64 = (long)strtol(yytext,NULL,10);
           fprintf(stderr,"value of int is %ld\n",yylval->val.int64);return TOK_INT;}
{DIGIT}+"."{DIGIT}* {LEX_MSG("lexing real");yylval->tag=_double;
  yylval->val.real64 = strtod(yytext,NULL);
  return TOK_REAL;}
    /*String Literal a quote, followed by either a literal \" 
   or anything that isnt a " repeated 1 or more times, followed by another quote.*/
"\""([^\"]|\/"\"")+"\"" {LEX_MSG("Lexing string");yylval->tag=_str;
  yylval->val.string=strdup(yytext);return TOK_STRING;}
"#"("\\|"|"\\#"|"\\\""|[^|#]) {LEX_MSG("lexing char");yylval->tag=_char;
    yylval->val.utf8_char=(wchar_t)(yytext[1]);return TOK_CHAR;}
 /*Special forms, generating function at end of file*/
def(ine)? {LEX_MSG("lexing define");
  yylval->tag=_special;yylval->val.string="define";
   return TOK_DEF;}
defun {LEX_MSG("lexing defun");
  yylval->tag=_special;yylval->val.string="defun";
   return TOK_DEFUN;}
setq {LEX_MSG("lexing setq");
  yylval->tag=_special;yylval->val.string="setq";
   return TOK_SETQ;}
datatype {LEX_MSG("lexing datatype");
  yylval->tag=_special;yylval->val.string="datatype";
   return TOK_DATATYPE;}
union {LEX_MSG("lexing union");
  yylval->tag=_special;yylval->val.string="union";
   return TOK_UNION;}
enum {LEX_MSG("lexing enum");
      yylval->tag=_special;yylval->val.string="enum";
   return TOK_ENUM;}
struct {LEX_MSG("lexing struct");
  yylval->tag=_special;yylval->val.string="struct";
   return TOK_STRUCT;}
go {LEX_MSG("lexing go");
  yylval->tag=_special;yylval->val.string="go";
   return TOK_GO;}
tagbody {LEX_MSG("lexing tagbody");
  yylval->tag=_special;yylval->val.string="tagbody";
   return TOK_TAG;}
lamdba {LEX_MSG("lexing lambda");
  yylval->tag=_special;yylval->val.string="lambda";
   return TOK_LAMBDA;}
progn {LEX_MSG("lexing progn");
  yylval->tag=_special;yylval->val.string="progn";
   return TOK_PROGN;}
if {LEX_MSG("lexing if");
  yylval->tag=_special;yylval->val.string="if";
   return TOK_IF;}
let {LEX_MSG("lexing let");
  yylval->tag=_special;yylval->val.string="let";
   return TOK_LET;}
do {LEX_MSG("lexing do");
  yylval->tag=_special;yylval->val.string="do";
   return TOK_DO;}
quasiquote|"`" {LEX_MSG("lexing quasiquote");
  yylval->tag=_special;yylval->val.string="quasiquote";
   return TOK_QUASI;}
eval {LEX_MSG("lexing eval");
  yylval->tag=_special;yylval->val.string="eval";
   return TOK_EVAL;}
defmacro {LEX_MSG("lexing defmacro");
  yylval->tag=_special;yylval->val.string="defmacro";
   return TOK_DEFMACRO;}
{QUOTE} {LEX_MSG("Lexing quote");
  yylval->tag=_special;yylval->val.string="quote";
  return TOK_QUOTE;}
{TYPENAME} {LEX_MSG("lexing typename");yylval->tag=_str;
  yylval->val.string=strdup(&yytext[2]);
  return TOK_TYPEINFO;}
"#|" {LEX_MSG("lexing open comment");return TOK_COMMENT_START;}
"|#" {LEX_MSG("lexing close comment");return TOK_COMMENT_END;}
{ID} {LEX_MSG("lexing ID");yylval->tag=_str;
  yylval->val.string=strdup(yytext);return TOK_ID;}
"(" {LEX_MSG("lexing (");return TOK_LPAREN;}
")" {LEX_MSG("lexing )");return TOK_RPAREN;}
"[" {LEX_MSG("lexing [");return TOK_LBRACE;}
"]" {LEX_MSG("lexing ]");return TOK_RBRACE;}
"{" {LEX_MSG("lexing {");return TOK_LCBRACE;}
"}" {LEX_MSG("lexing }");return TOK_RCBRACE;}
[ \t\n]+ /*whitespace*/
";"[^\n]* /*one line comments*/
<<EOF>> return -1;

 /*(defun special (name) (insert (format "\n%s {LEX_MSG(\"lexing %s\");
   yylval->tag=_special;yylval->val.string=\"%s\"
   return TOK_%s;}" (downcase name) (downcase name) (downcase name) (upcase name))))
(dolist (name '("define" "defun" "setq" "datatype" "union" "enum" "struct" "go" "tagbody" "lamdba" "progn" "if" "let" "do" "quasiquote" "eval" "defmacro")) (special name))*/
