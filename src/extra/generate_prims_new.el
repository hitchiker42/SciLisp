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
(defvar SciLisp-special-forms)
(defalias 'define 'setq);because emacs indents things begining with def
                        ;in a way that makes more sense
(defun indent-buffer ()
  "Indent entire buffer using indent-region"
  (interactive)
  (indent-region (point-min) (point-max)))
;;functions to make primitives
;;in elisp a primitive is represented as alist
(cl-defun mk-prim-subr
    (lname cname fname minargs &key (optargs 0) (keyargs 0)
           (restarg 0) (sig "()") (doc ""))
  (let ((maxargs (+ minargs optargs keyargs restarg)))
    `((:lname . ,lname) (:cname . ,cname) (:fname . ,fname)
      (:minargs . ,minargs) (:maxargs . ,maxargs)
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
(define SciLisp-special-forms
  (list "lambda" "nil" "let" "let_star" "while" "tagbody" "go"
        "throw" "catch" "setq" "unwind-protect" "if" "progv" "progn"
        "return-from" "block"))
(define SciLisp-errors
  (list "type" "bounds" "file" "read" "args" "key" "fatal" ;stack overflow,c error
        "undefined" "unbound" "math" "eof" "io" "overflow"))
(define SciLisp-keywords
  (mapcar (lambda (x) (concat "Q" x))());special things..?/reserved words  
  (mapcar (lambda (x) (concat "E" x))()));builtin error types
(define SciLisp-Types
  (mapcar (lambda (x) (concat "T" x)
            (list "int8" "int16" "int32" "int64" "uint8" "uint16" "uint32" 
                  "uint64" "error" "real32" "real64" "bigint" "bigfloat" 
                  "char" "string" "array" "stream" "fun" "symbol" "macro" 
                  "type" "hashtable" "regex" "nil"
                  "env" "obarray" "true" "false" "uninterned"))))
(define SciLisp-predicates
  (append
   (mapcar (lambda (x) (list (concat x "?")
                             (concat "lisp_" x "p")
                             1
                             :sig
                             "(object)"))
           '("array" "cons" "number" "integer" "function"
             "string" "stream" "sequence" "real")))
  (list 
   ("eq" "lisp_eq" 2 :sig "(obj1 obj2)")
   ("eql" "lisp_eql" 2 :sig "(obj1 obj2)")
   ("equal" "lisp_equal" 2 :sig "(obj1 obj2)")
   ("even?" "lisp_evenp" 1 :sig "(integer)")
   ("odd?" "lisp_oddp" 1 :sig "(integer)")
   ("zero?" "lisp_zerop" 1 :sig "(number)")))
(define SciLisp-math-funs
  (list
   ("!=" "Fne" "lisp_numne" 2 :sig "(num1 num2)")
   ("*"  "Fmul" "lisp_mul_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("+"  "Fadd" "lisp_add_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("1+" "Finc" "lisp_inc" 1 :sig "(number)")
   ("-"  "Fsub" "lisp_sub_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("1-" "Fdec" "lisp_dec" 1 :sig "(number)")
   ("/" "Fdiv" "lisp_div_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("<" "lisp_numlt" 2 :sig "(num1 num2)")
   ("<=" "lisp_numle" 2 :sig "(num1 num2)")
   ("=" "lisp_numeq" 2 :sig "(num1 num2)")
   (">" "lisp_numgt" 2 :sig "(num1 num2)")
   (">=" "lisp_numge" 2 :sig "(num1 num2)")
   ("abs" "lisp_abs" 1 :sig "(number)")
   ("cos" "lisp_cos" 1 :sig "(number)")
   ("exp" "lisp_exp" 1 :sig "(number)")
   ("expt" "lisp_pow" 2 :sig "(num1 num2)")
   ("log" "lisp_log" 1 :sig "(number)")
   ("mod" "lisp_mod" 2)
   ("pow" "lisp_pow_driver" 1 :restarg 1)
   ("sin" "lisp_sin" 1 :sig "(number)")
   ("tan" "lisp_tan" 1 :sig "(number)")))
(define SciLisp-cons-funs
  (list
    ("assoc" "lisp_assoc" 2 :sig ("key list"))    
    ("assq" "lisp_assq" 2 :sig ("key list"))
    ("cons" "Cons" 2 :sig "(car cdr)")
    ("copy-tree" "copy_tree" 1 :sig "(cell)")
    ("drop" "cons_drop" 2 :sig "(list n)")
    ("last" "lisp_last" 1 :sig "(list)")
    ("rand-list" "rand_list" 1 :optargs 1)
    ("rassoc" "lisp_rassoc" 2 :sig ("key list"))
    ("rassq" "lisp_rassq" 2 :sig ("key list"))
    ("reduce" "cons_reduce" 2 :optargs 1 :sig "(seq function)")
    ("reverse!" "cons_nreverse" 1 :sig "(seq)")
    ("reverse" "cons_reverse" 1 :sig "(seq)")
    ("set-car!" "set_car" 2 :sig "(cell new-val)")
    ("set-cdr!" "set_cdr" 2 :sig "(cell new-val)")
    ("split" "cons_split" 1 :optargs 1)
    ("take" "cons_take" 2 :sig "(list n)")))
(define SciLisp-array-funs
  (list 
   ("aref" "aref" 2 :sig "(array index)")
    ("array-map!" "array_nmap" 2 :sig "(array map-fn)")
    ("array-map" "array_map" 2 :sig "(array map-fn)")
    ("array-qsort" "array_qsort" 2 :optargs 1 
     :sig "(array predicate &optional in-place)")
    ("array-reduce" "array_reduce" 2 :optargs 1 
     :sig "(array function &optional init)")
    ("array-reverse!" "array_nreverse" 1 :sig "(array)")
    ("array-reverse" "array_reverse" 1 :sig "(array)")))
(define SciLisp-io-funs)
(define SciLisp-sequence-funs
  (list
   ("map")
   ("reduce")
   ("sort")
   ("stable-sort")
   ("copy-seq")
   ("length")
   ("reverse")
   ("reverse!")
   ("elt")
   ;search a sequence for the first element
   ;which matches some condition
   ;(<search> test-or-value sequence &key reverse start end test)
   ;if test-or-value is a value search sequence for the first
   ;value eq/eql/equal to that element (test detemines equality, defaults
   ;to eq if test-or-value is a function find the first element
   ;which causes test to return true
   ("find");returns the element (or nil)
   ("position");returns the position relative to begining
   ("exists?");just return #t or #f
   ;same semantics for the search functions (ie function or value as test)
   ("filter")("remove");non destructive need to pick a name
   ("filter!")("delete!");destructive
   ("make-sequence");(make-sequence type size &optional (element nil))
   ("delete-duplicates")
   ("subseq")
   ("rand-seq");(rand-seq type size &optional (element-type 'int))
   )
(define SciLisp-hash-funs)
(define SciLisp-subrs
  '(("addhash" "hashtable_add_entry" 3 :optargs 1 
     :sig "(hash-table key value &optional option)")
    ("apply" "lisp_apply" 2 :optargs 1 :sig "(fun arglist &optional env)")
    ("array->list" "array_to_list" 1 :sig "(array)")
    ("ash" "ash" 2 :sig "(value count)")
    ("assert" "lisp_assert" 1 :sig "(expr)")
    ("bigfloat" "lisp_bigfloat" 1 :optargs 2 
     :sig "(number-or-string &optional prec rnd)")
    ("bigint" "lisp_bigint" 1 :sig "(number)")
;    ("c-ptr-val" "lisp_dereference_c_ptr" 1 :sig "(pointer)")
    ("concat" "lisp_concat" 0 :restarg 1 :sig "(&rest seqs)")
    ("ccall" "ffi_ccall" 5 :optargs 1 
     :sig "(function-name libname return-type argtypes args &optional thread)")

    ("copy" "Fcopy" "lisp_copy" 1 :sig "(obj)");maybe call this duplicate
    ("char->string" "lisp_char_to_string" 1 :sig "(character)")
    ("documentation" "lisp_get_docstring" 1 :sig "(obj)")
    ("eval" "lisp_eval" 1 :optargs 1 :sig "(expr &optional env)")
    ("exit" "lisp_exit" 0 :optargs 1 :sig "(&optional exit-code)")
    ("fclose" "lisp_close" 1 :sig "(stream)")
    ("fopen" "lisp_open" 1 :optargs 1 :sig "(file &optional mode)")
    ("format" "lisp_format" 1 :restarg 1 :sig "(format &rest objects)")
    ("fprint" "lisp_fprint" 2 :sig "(obj stream)")
    ("fprintln" "lisp_fprintln" 2 :sig "(obj stream)")
    ("fputs" "lisp_fputs" 2 :sig "(obj stream)")
    ;("ge" "lisp_cmp_ge" 2 :sig "(num1 num2)")
    ("gensym" "lisp_gensym" 0)
    ("get-type" "getKeywordType" 1 :sig "(obj)")
    ("gethash" "hashtable_get_entry" 2 :sig "(hash-table key)")
    ;("gt" "lisp_cmp_gt" 2 :sig "(num1 num2)")
    ("hash-table-entries" "hashtable_num_entries" 1 :sig "(hash-table)")
    ("hash-table-growth-factor" "hashtable_growth_threshold" 1 
     :sig "(hash-table)")
    ("hash-table-growth-size" "hashtable_growth_factor" 1 
     :sig "(hash-table)"p)
    ("hash-table-size" "hashtable_size" 1)
    ("iota" "lisp_iota" 1 :optargs 4 
     :sig "(start &optional stop step seq-type round)")
    ("identity" "lisp_identity" 1 :sig "(form)")
    ("length" "lisp_length" 1 :sig "(sequence)")
    ("list" "lisp_list" 0 :restarg 1 :sig "(&rest objects)")
    ("list->array" "array_from_list" 1 :sig "(list)")
    ("list-qsort" "cons_qsort" 2 :sig "(list predicate)")
    ("load" "lisp_load" 1 :sig "(file)")
;these need to be made into arith-driver functions
    ("logand" "lisp_logand" 2 :sig "(int1 int2)")
    ("logandn" "lisp_logandn" 2 :sig "(int1 int2)")
    ("logior" "lisp_logior" 2 :sig "(int1 int2)")
    ("logxor" "lisp_xor" 2 :sig "(int1 int2)")
    ("lognot" "lisp_lognot" 1 :sig "(integer)")
    ("make-c-ptr" "make_c_ptr" 1 :optargs 1)
    ("make-hash-table" "makeHashtable" 0 :keyargs 7)
    ("make-string-input-stream" "make_string_input_stream" 1)
    ("make-tree" "make_tree" 1 :optargs 1 :restarg 1)
    ("mapcar" "mapcar" 2 :sig "(map-fn list)")
    ("max" "lisp_max_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("merge-sort" "sequence_merge_sort" 2 :sig "(seq predicate)")
    ("min" "lisp_min_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("not" "lisp_not" 1)
    ("nth" "lisp_nth" 2 :sig "(list n)")
    ("pop!" "pop_cons" 1 :sig "(place)")
    ("pprint" "lisp_pprint" 1 :sig "(object)")
    ("print" "lisp_print" 1 :sig "(object)")
    ("print-to-string" "lisp_print_to_string" 1 :sig "(object)")
    ("println" "lisp_println" 1 :sig "(object)")
    ("push!" "push_cons" 2 :sig "(new-val place)")
    ("pwd" "lisp_getcwd" 0 :sig "()")
    ("qsort" "sequence_qsort" 2 :sig "(seq predicate)")
    ;raises a simple error
    ("raise" "lisp_error" 1 :optargs 1 :sig "(tag &optional value)")
    ("rand-array" "rand_array" 1 :optargs 1)
    ("seed-rand" "lisp_init_rand" 0 :optargs 1 :sig "(seed-val)")
    ("seed-rand-r" "lisp_init_rand_r" 0 :optargs 1 :sig "(seed-val)")
    ("rand-float" "lisp_randfloat" 0 :optargs 2 :sig "(&optional state scale)")
    ("rand-int" "lisp_randint" 0 :optargs 2 :sig "(&optional state unsigned)")
    ("re-compile" "lisp_re_compile" 1 :optargs 1)
    ("re-match" "lisp_re_match" 2 :optargs 3)
    ("re-subexpr" "lisp_get_re_backref" 2)
    ("read-string" "lisp_read_string" 1 :sig "(string)")
    ("read" "lisp_read" 1 :sig "(stream)")
;need to make generic

    ("round" "lisp_round" 1 :optargs 1)
    ("set-aref!" "set_aref" 3 :sig "(array index new-val)")
    ("arglist" "lisp_get_signature" 1 :sig "(function)")
    ("sort" "lisp_sort" 2 :sig "(seq sort-fn)")
    ("sqrt" "lisp_sqrt" 1 :sig "(number)")
    ("string->char" "lisp_string_to_char" 1 :sig "(string)")
    ("sxhash" "lisp_hash_sexp" 1 :sig "(object)")
    ("system" "lisp_system" 1 :restarg 1)
    ("time" "lisp_time" 0 :optargs 1 :restarg 1)
    ("type-of" "type_of" 1 :sig "(object)")
    ("typeName" "lisp_typeName" 1 :sig "(object)")))
