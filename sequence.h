#define make_sequence_function(name)                    \
  sexp sequence_##name(sexp seq){                       \
  if(!SEQUENCEP(seq)){                                  \
    return format_type_error(#name,"sequence",seq.tag); \
  }                                                     \
    switch(seq.tag){                                    \
    case _array:                                        \
      return array_##name(seq);                         \
    case _list:                                         \
    case _cons:                                         \
      return cons_##name(seq);                          \
    }                                                   \
    }
#define make_sequence_function2(name)                    \
  sexp sequence_##name(sexp seq,sexp obj2){              \
    if(!SEQUENCEP(seq)){                                 \
    return format_type_error(#name,"sequence",seq.tag); \     
    }                                                   \
    switch(seq.tag){                                    \
    case _array:                                        \
      return array_##name(seq,obj2);                    \
    case _list:                                         \
    case _cons:                                         \
      return cons_##name(seq,obj2);                     \
    }                                                   \
    }
#define make_sequence_function3(name)                    \
  sexp sequence_##name(sexp seq,sexp obj2,sexp obj3){             \
    if(!SEQUENCEP(seq)){                                 \
    return format_type_error(#name,"sequence",seq.tag); \     
    }                                                   \
    switch(seq.tag){                                    \
    case _array:                                        \
      return array_##name(seq,obj2,obj3);                   \
    case _list:                                         \
    case _cons:                                         \
      return cons_##name(seq,obj2,obj3);                    \
    }                                                   \
    }
make_sequence_function2(qsort);
make_sequence_function(reverse);
make_sequence_function(nreverse);
