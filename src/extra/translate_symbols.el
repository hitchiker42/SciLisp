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
;;Generally preface with lisp, with some exceptions (i.e cons because I use it so much)
