/*Charmap format:
  spaces are potentially more than one space
  <U(NNNN){1,2}>(\.\.<U(NNNN){1,2}>)? (/xNN){1,6} [A-Z ]|<[a-zA-Z ]>
  Codepoint (potentially a range)     UTF8 bytes  Character name

  Simplified format
  all spaces are one space
  (NNNN){1,2}(..(NNNN){1,2})? NN(-NN){0,5} [A-Z ]+|<[a-zA-Z ]+>

  maybe expand ranges, this would require work
*/
#define HEXVALUE(c)                                     \
  ((c) >= 'A' && (c) <= 'F' ? (c)-'A'+10 : (c)-'0')
#define hexvalue(c)                                     \
  ((c) >= 'a' && (c) <= 'f' ? (c)-'a'+10 : (c)-'0')
