#define format_arg_error(name,got,expected)                             \
  ({CORD err_str;                                                       \
    CORD_sprintf(&err_str,"invalid number of arguments passed to %r,"   \
                 "expected %s but got %s",name,got,expected);           \
    err_str;})
#define format_type_error_va(fun,format,args...)                        \
  ({CORD type_error_str;                                                \
    CORD_sprintf(&type_error_str,"type error in %r, ",fun);             \
    type_error_str=CORD_cat(type_error_str,format);                     \
    CORD_sprintf(&type_error_str,type_error_str,args);                  \
    raise_simple_error(Etype,make_string(type_error_str);})
#define format_type_error(fun,expected,got)                             \
  ({CORD type_error_str;                                                \
    CORD_sprintf(&type_error_str,"type error in %r, expected %r but got %r", \
                 fun,expected,tag_name(got));                           \
    type_error_str;})
//  raise_simple_error((uint64_t)Etype,make_string(type_error_str));})
#define format_type_error_named(fun,name,expected,got)                  \
  ({CORD type_error_str;                                                \
    CORD_sprintf(&type_error_str,                                       \
                 "type error in %r, expected a(n) %r for %r but got a(n) %r", \
                 fun,expected,name,tag_name(got)),                      \
      raise_simple_error(Etype,make_string(type_error_str));})
#define format_type_error2(fun,expected1,got1,expected2,got2)           \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r and %r"   \
               ", but got %r and %r",fun,expected1,expected2,           \
               tag_name(got1),tag_name(got2)),                          \
    type_error_str;})
#define format_type_error3(fun,expected1,got1,expected2,got2,expected3,got3) \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r,%r and %r" \
               ", but got %r,%r and %r",fun,expected1,expected2,expected3, \
               tag_name(got1),tag_name(got2),tag_name(got3)),           \
    type_error_str;})
#define format_type_error_opt(fun,expected,got)                         \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r or nil" \
               ", but got %r",fun,expected,tag_name(got)),              \
    type_error_str;})
#define format_type_error_opt_named(fun,name,expected,got)              \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r,expected %r or nil for argument %r" \
               ", but got %r",fun,expected,name,tag_name(got)),         \
    type_error_str;})
#define format_type_error_key(fun,named,expected,got)   \
  format_type_error_opt_named(fun,named,expected,got)
#define format_type_error_opt2(fun,expected1,expected2,got)             \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r or %r"    \
               ", but got %r",fun,expected1,expected2,tag_name(got)),   \
    type_error_str;})
#define format_type_error_opt2_named(fun,name,expected1,expected2,got)  \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r,%r or nothing" \
               "for %r, but got %r",fun,expected1,expected2,name,tag_name(got)), \
    type_error_str;})
#define format_type_error_rest(fun,expected,failed_arg)                 \
  ({CORD type_error_str;                                                \
  CORD_sprintf(&type_error_str,"type error in %r, expected %r for the rest argument," \
               "but received an %r (value was %r)",                     \
               fun,expected,tag_name(failed_arg.tag),print(failed_arg)), \
    type_error_str;})
