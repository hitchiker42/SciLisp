#!/usr/bin/emacs --script
;;-*- lexical-binding: t -*-
(require 'cl)
(assert (eq t lexical-binding))
;;utitily functions
(defvar current-dir-name
  (if load-in-progress
      (file-name-directory load-file-name)
    (file-name-directory default-directory)))
(defvar SciLisp-prims nil "a list containing builtin functions, compilier macros and special forms")
(defvar SciLisp-types nil "a list of builtin scilisp types")
(defvar SciLisp-keywords nil "a list of builtin self quoting symbols")
(defun indent-buffer ()
  "Indent entire buffer using indent-region"
  (interactive)
  (indent-region (point-min) (point-max)))
;;functions to make primitives
;;in elisp a primitive is represented as alist
(cl-defun mk-prim (lname cname minargs &key (optargs 0) (keyargs 0)
                         (restarg 0) (sig "()") (doc ""))
  (let ((maxargs (+ minargs optargs keyargs restarg)))
    `((:lname . ,lname) (:cname . ,cname) (:minargs . ,minargs) (:maxargs . ,maxargs)
    (:optargs . ,optargs) (:keyargs . ,keyargs) (:restarg . ,restarg)
    (:sig . ,sig) (:doc . ,doc))))
(cl-defun mk-global (lname cname &key (const 0) (type 0) (doc ""))
  `((:lname . ,lname) (:cname . ,cname) (:const . ,const)
    (:type . ,type) (:doc . ,doc)))
(defun intern-prims ()
  (let* ((prims (append SciLisp-prims SciLisp-types SciLisp-keywords)))
    (with-output-to-string
      (princ "void init_global_obarray(){\n")
      (dolist (prim prims)
        (princ (format "  cintern_unsafe(global_obarray,%s_val);\n" prim)))
      (princ "}\n"))))
(setq SciLisp-keywords
(setq SciLisp-Types
  (list "int8" "int16" "int32" "int64" "uint8" "uint16" "uint32" "uint64" "error"
    "real32" "real64" "bigint" "bigfloat" "char" "string" "array" "stream"
    "fun" "symbol" "macro" "type" "keyword" "hashtable" "regex"
    "nil" "dpair" "lenv" "env" "obarray" "funargs" "true" "false" "uninterned"))





