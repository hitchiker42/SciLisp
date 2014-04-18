/*(let ((i 0))
  (while (<= i 256)
    (if (-contains? '(?\" ?\' ?\; ?\( ?\) ?\[ ?\] ?\{ ?\} ?\` ?\, ?\# ) i)
        (insert "1,")
      (insert "0,"))
      (incf i)))*/
//invalid characters in a symbol
//any ascii char > 0x20 (all characters < space, contains control chars and ws)
//;or one of " ' ; ( ) [ ] { } ` , # 
static const uint8_t invalid_symbol_char[256]=
  {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1};

//static void *default_readtable[128]=//only dispatch on ascii chars
//  {
enum utf8_char_catagory {
  utf8_null,
  utf8_ascii,
  utf8_start_2,
  utf8_start_3,
  utf8_start_4,
  utf8_start_5,
  utf8_start_6,
  utf8_cont,
  utf8_invalid,
};
static const uint8_t UTF8_table[256]=
  {utf8_null,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   utf8_ascii,utf8_ascii,utf8_ascii,utf8_ascii,
   
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,utf8_cont,
   utf8_cont,
   
   utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,
   utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,
   utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,
   utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,
   utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,
   utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,utf8_start_2,
   utf8_start_2,utf8_start_2,
   
   utf8_start_3,utf8_start_3,utf8_start_3,utf8_start_3,utf8_start_3,
   utf8_start_3,utf8_start_3,utf8_start_3,utf8_start_3,utf8_start_3,
   utf8_start_3,utf8_start_3,utf8_start_3,utf8_start_3,utf8_start_3,
   utf8_start_3,

   utf8_start_4,utf8_start_4,utf8_start_4,utf8_start_4,utf8_start_4,
   utf8_start_4,utf8_start_4,utf8_start_4,
 
   utf8_start_5,utf8_start_5,utf8_start_5,utf8_start_5,

   utf8_start_6,utf8_start_6,

   utf8_invalid,utf8_invalid
  };
  
#if 0
//this is stuff for reading via a dispatch table
/*    
 (progn
  (insert "/*read table template\nchar read_table[128]=\n{")
(dotimes (c 256)
  (if (zerop (mod c 8)) (insert "\n"))
  (insert (format "0 /*%c*%s" "/," c)))
  (insert "}"))*/
//read table template 
//control characters use abbreviation as given on the ascii man page
//assume any byte >128 is a multibyte character that is part of a symbol 
//(I may still need to give then entries in any actual readtable but 
//they don't represent any specific character)
char read_table[128]= 
  {0 /*\0*/,0 /*SOH*/,0 /*STX*/,0 /*ETX*/,
   0 /*EOT*/,0 /*ENQ*/,0 /*ACK*/,0 /*\a*/,
   0 /*\b*/,0 /*\t*/,0 /*\n*/,0 /*\v*/,
   0 /*\f*/,0 /*\r*/,0 /*SO*/,0 /*SI*/,
   0 /*DLE*/,0 /*DC1*/,0 /*DC2*/,0 /*DC3*/,
   0 /*DC4*/,0 /*NAK*/,0 /*SYN*/,0 /*ETB*/,
   0 /*CAN*/,0 /*EM*/,0 /*SUB*/,0 /*ESC*/,
   0 /*FS*/,0 /*GS*/,0 /*RS*/,0 /*US*/,
   0 /*SPACE*/,0 /*!*/,0 /*"*/,0 /*#*/,0 /*$*/,0 /*%*/,0 /*&*/,0 /*'*/,
   0 /*(*/,0 /*)*/,0 /***/,0 /*+*/,0 /*,*/,0 /*-*/,0 /*.*/,0 /*/*/,
   0 /*0*/,0 /*1*/,0 /*2*/,0 /*3*/,0 /*4*/,0 /*5*/,0 /*6*/,0 /*7*/,
   0 /*8*/,0 /*9*/,0 /*:*/,0 /*;*/,0 /*<*/,0 /*=*/,0 /*>*/,0 /*?*/,
   0 /*@*/,0 /*A*/,0 /*B*/,0 /*C*/,0 /*D*/,0 /*E*/,0 /*F*/,0 /*G*/,
   0 /*H*/,0 /*I*/,0 /*J*/,0 /*K*/,0 /*L*/,0 /*M*/,0 /*N*/,0 /*O*/,
   0 /*P*/,0 /*Q*/,0 /*R*/,0 /*S*/,0 /*T*/,0 /*U*/,0 /*V*/,0 /*W*/,
   0 /*X*/,0 /*Y*/,0 /*Z*/,0 /*[*/,0 /*\*/,0 /*]*/,0 /*^*/,0 /*_*/,
   0 /*`*/,0 /*a*/,0 /*b*/,0 /*c*/,0 /*d*/,0 /*e*/,0 /*f*/,0 /*g*/,
   0 /*h*/,0 /*i*/,0 /*j*/,0 /*k*/,0 /*l*/,0 /*m*/,0 /*n*/,0 /*o*/,
   0 /*p*/,0 /*q*/,0 /*r*/,0 /*s*/,0 /*t*/,0 /*u*/,0 /*v*/,0 /*w*/,
   0 /*x*/,0 /*y*/,0 /*z*/,0 /*{*/,0 /*|*/,0 /*}*/,0 /*~*/,0 /*DEL*/};
typedef sexp(read_dispatch)(uint8_t,read_input*);
//assume tail recursion (gcc will optimize tail recursion at -O2)
//assume read-table is kept somewhere accessable from these functions
//also assume a read table entry is a function pointer sexp(*)(uint8_t,read_input *)
sexp skip_char(uint8_t c,read_input *input){
  c=read_char(input);
  return readtable[c](c,input);
}
//skip line already taken 
sexp comment_line(uint8_t c,read_input *input){
  skip_line(input);
  c=read_char(input);
  return readtable[c](c,input);
#endif
