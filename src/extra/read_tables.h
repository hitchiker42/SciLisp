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
  
     
