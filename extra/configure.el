#!/usr/bin/emacs --script ;-*- lexical-binding: t; -*-
(require 'dash)
(require 'cl)
(defmacro define (var defn)
  (progn
    `(defvar ,var)
    `(setq ,var ,defn)))
(defconst cc-default
  (getenv "CC"))
(defconst cxx-default
  (getenv "CC"))
(defconst cflags-default
  (getenv "CFLAGS"))
(defconst SciLisp-common-headers
  (list "common.h" "debug.h" "env.h" "types.h" "cffi.h" "print.h"))
(defconst SciLisp-headers
  (append
   SciLisp-common-headers
   (list "cons.h" "array.h" "sequence.h" "bignum.h" "lisp_math.h"
         "prim.h" "regex.h" "unicode.h" "hash.h" "lex.yy.h")))
(defconst SciLisp-frontend
  (list "frontend.c" "scilisp.h"))
(defconst SciLisp-backend
  (list "codegen.c" "llvm_codegen.c"))
(defconst SciLisp-Lib
  (list "array.c" "cons.c" "print.c" "parser.c" "eval.c"
        "lisp_math.c" "env.c" "regex.c" "lisp_system.c"
        "unicode.c" "sequence.c"))
(defconst SciLisp-Warning-Flags
  (concat 
   (mapcar (lambda (x) (concat "-W" x " "))
           (list "parentheses" "sequence-point" "array-bounds"
                 "enum-compare" "missing-field-initializers"
                 "implicit" "strict-aliasing" "missing-braces"
                 "comment"))
   '("-fmax-errors=30")))
(defconst SciLisp-req-libs
  (mapcar (lambda (x) (concat "-l" x))
          (list "gc" "m" "readline" "cord" "pthread" "gmp" "mpfr" "dl")))
(defconst SciLisp-common-cflags 
  '("-std=gnu99" "-D_GNU_SOURCE" "-foptimize-sibling-calls" "-rdynamic"
    "-fshort-enums"))
(defun SciLisp-std-rule (obj-file &rest headers)
  (cons (concat obj-file ": ")
        `(,(o-to-c obj-file) ,@SciLisp-common-headers 
          ,(if (-contains? SciLisp-headers (o-to-h obj-file))
               (o-to-h obj-file)
             "")
          ,@headers)))
(defun SciLisp-lib-compile (c-file)
  (format "$(LIBSCILISP_CC) -o lib_files/libSciLisp_%s -c %s"
          (c-to-o c-file) c-file))
(defvar opt-flags)
(defvar cc)
(defmacro replace-ext (init-ext new-ext)
  `(fset 
    ',(intern (concat init-ext "-to-" new-ext))
    (lambda (str)
      (if (not (string-match (concat "\\(.+\\.\\)" ,init-ext) str))
          ""
        (concat (match-string 1 str) ,new-ext)))))
(replace-ext "c" "h")
(replace-ext "c" "o")
(replace-ext "o" "c")
(replace-ext "o" "h")
(replace-ext "lisp" "sl")
(defun set-cc ()
  (let ((cc (if (string-equal cc-default "cc")
                "gcc"
              cc-default)))
    (if (string-equal cc "gcc")
        (setq opt-flags (list "-Og -flto -g"))
      (setq opt-flags (list "-O1 -fno-lto -g")))
    (concat cc " -fPIC ")))
