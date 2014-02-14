//assumes an initialized CORD_ec buf, a uint8_t c, and ints len and mb
#define read_symbol_string(input)               \
  while((c==read_char(input))){                 \
  if(invalid_symbol_char[c]){                   \
    break;                                      \
  }                                             \
    if(c=='\\'){                                \
      CORD_ec_append(read_char(input));         \
    } else {                                    \
      CORD_ec_append(c);                        \
    }                                           \
    len++;                                      \
  }
//read a number in base 2^lg2_base using functions test to test if
//a character is valid in that base, and extract to get a number from
//a character, raise an error with the string err if the first char fails test
//test and extract can be macros
#define read_pow_of_two_base_number(input,test,extract,lg2_base,err)    \
      ({uint8_t c;                                                      \
      uint64_t result;                                                  \
      c=read_char(input);                                               \
      if(!(test(c))){                                                   \
        raise_simple_error(Eread,err);                                  \
      }                                                                 \
      do {                                                              \
        result = (result<<lg2_base)+extract(c);                         \
      } while((c=read_char(input)) && test(input));                     \
      result;})
#define binary_char_test(c) (c-0x30 <= 1)
#define binary_char_extract(c) (c-0x30)
#define oct_char_test(c) (c-0x30 <= 8)
#define oct_char_extract(c) (c-0x30)
#define READ_CHAR_MULTIBYTE(input,mb_ptr)       \
  ({uint8_t c= read_char(input);                \
    if(c>=0x80){                                \
      *mb_ptr=1;                                \
    }                                           \
    c;})
#define is_valid_symbol_char(c)                 \
  (!invalid_symbol_char[(uint8_t)c])
#define is_invalid_symbol_char(c)               \
  (invalid_symbol_char[(uint8_t)c])
//forward declarations of internal functions
sexp read_0(read_input *input,int flags);
static sexp read_bigint(read_input *input,int radix);
static sexp read_sharp(char *input);
static char parse_simple_escape(char escape_char);
static int parse_escape_internal(read_input *input,char** output);
static char parse_hex_escape(read_input *input);
static uint32_t parse_unicode_escape(read_input *input,int udigits);
static sexp read_symbol_verbatim(read_input *input);
static sexp symbol_from_ec_cord(CORD_ec buf);
static sexp read_double_quoted_string(read_input *input);
static sexp string_to_number(char *str);
static sexp read_symbol_or_number(read_input *input);
static sexp read_keyword_symbol(read_input *input);
static sexp read_list(read_input *input);
static sexp read_array(read_char *input);
