;; -*- lexical-binding: t; -*-
;;translate lisp function/variable names to c and vice-versa
;;Rules (some of these are borrowed from the guile manual)
;;'-' -> '_' (seperator (not minus sign))
;;'?' -> '_p' (predicate)
;;'!' -> '_x' (destructive operation)
;;'->' -> '_to_' (type conversion)
;;'>' -> '_lt'
;;'<' -> '_gt'
;;'<=' -> '_le'
;;'>=' -> '_ge'
;;'!=' -> '_ne'
;;'*...*' -> ? (global variable)
;;'+...+' -> ? (global constant? maybe?)
;;Next four are only when used in a mathmatical context
;;'+' -> 'plus'
;;'-' -> 'minus'
;;'/' -> 'div'
;;'*' -> 'mul'
;;Everything is prefaced with lisp_, however there may be aliases that are used
;;in the C code in order to keep names short

;;types are suffixed by _t, This will probably require a lot of replacing things

;;Lisp symbol names (in C) are formed by adding the suffix _sym to the c names
;;Pointers to lisp symbols are formed by adding the suffix _ptr to the c names
;;Function objects are formed by adding _fun to the function name
