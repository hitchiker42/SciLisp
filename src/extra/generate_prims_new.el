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
;;man, this is excessive, but it should get the job done
;;because it has happened more that a few times that 
;;I didn't actually have the hash function program
(unless (file-executable-p (expand-file-name "../get_hash"))
  (if (file-exists-p (expand-file-name "../get_hash"))
      (error "Error, file get_hash exists and is not executable")
    (call-process (or (getenv "CC")
                      (locate-file "gcc" exec-path)
                      (locate-file "cc" exec-path)
                      (error (concat "Error, no C compiler found in"
                                     "default search directories and"
                                     "CC not defined in the environment")))
                  nil t nil (concat "-o" current-dir-name "../get_hash")
                  "../get_hash.c" "-O2")))
(defun indent-buffer ()
  "Indent entire buffer using indent-region"
  (interactive)
  (indent-region (point-min) (point-max)))
;;functions to make primitives
;;in elisp a primitive is represented as alist
;;primitive function,special form, or macro
;;lname = name in lisp
;;cname = name of symbol in c (generally <type-prefix><lisp_name>)
;;after any non identifier symbols in lisp_name are translated to _
;;value = name of function in c (i.e Fcons,car)(no real pattern to this)
;;minargs,optargs,keyargs,restarg = number of arguments the function takes
;;sig = function arglist (i.e for cons sig = "(obj1 obj2)" or for list "(&rest objects)"
;;const = is this an immutable form (generally no,but it should at least warn
;;but for special forms yes)
(defun digit-name (digit)
  (case digit
    (0 "zero")(1 "one")(2 "two")(3 "three")(4 "four")
    (5 "five")(6 "six")(7 "seven")(8 "eight")(9 "nine")))
(- (aref "83" 0) #x30)
(defun lisp-name->c_name (lisp_name)
  (if (string-match "^[0-9]" lisp_name)
      (setq lisp_name (concat (digit-name (- (aref lisp_name 0) #x30))
                              (substring lisp_name 1))))
  (setq lisp_name (replace-regexp-in-string "+" "_plus_" lisp_name))
  (setq lisp_name (replace-regexp-in-string "*" "_star_" lisp_name))
  (replace-regexp-in-string "[^A-Za-z_0-9]" "_" lisp_name))
(cl-defun mk-prim-subr
    (lname cname value minargs &key (optargs 0) (keyargs 0)
           (restarg 0) (sig "()") (doc "") (const 2))
  (let ((maxargs (+ minargs optargs keyargs restarg)))
    `((:lname . ,lname) (:cname . ,cname) (:value . ,value)
      (:minargs . ,minargs) (:maxargs . ,maxargs)
      (:optargs . ,optargs) (:keyargs . ,keyargs) (:restarg . ,restarg)
      (:sig . ,sig) (:doc . ,doc) (:const . ,const))))
;;primitive global constant or variable
(cl-defun mk-global (lname cname val &key (const 0) (type 0) (doc ""))
  `((:lname . ,lname) (:cname . ,cname) (:val . ,val) (:const . ,const)
    (:type . ,type) (:doc . ,doc)))
(define SciLisp-special-forms ;prefix Q
  (list "lambda" "closure" "nil" "let" "let_star" "while" "tagbody" "go"
        "throw" "catch" "setq" "unwind_protect" "if" "progv" "progn"
        "return_from" "block" "quote" "comma" "backquote"))
(define SciLisp-errors;prefix E
  (list "type" "bounds" "file" "read" "args" "key" "fatal" ;stack overflow,c error
        "undefined" "unbound" "math" "eof" "io" "overflow" "range" "const"))
(define SciLisp-types;prefix T
  (list "int8" "int16" "int32" "int64" "uint8" "uint16" "uint32"
        "uint64" "error" "real32" "real64" "bigint" "bigfloat"
        "char" "string" "array" "stream" "fun" "symbol" "macro"
        "type" "hashtable" "regex" "nil"
        "env" "obarray" "true" "false" "uninterned"))
(define SciLisp-ampersand-keywords;prefix A
  (list "rest" "body" "environment" "optional" "key"))
(define SciLisp-keywords;prefix K (lisp prefix ':")  
  (list "end" "start1" "count" "documentation" "element-type" "end1"
        "end2" "export" "import" "import-from" "initial-contents" "test"
        "initial-element" "key" "size" "start" "start2" "test" "use"))
(define SciLisp-globals;prefix G (lisp prefix/postfix *)
  ("stdin" "stdout" "stderr"))
;subroutines prefix S
(define SciLisp-predicates
  (append
   (mapcar (lambda (x) (list (concat x "?") (concat "lisp_" x "p")
                             1 :sig "(object)"))
           '("array" "cons" "number" "integer" "function"
             "string" "stream" "sequence" "real" "bignum" "bigint" "bigfloat"
             "hashtable" "macro" "special-form")))
  (list
   ("eq" "lisp_eq" 2 :sig "(obj1 obj2)")
   ("eql" "lisp_eql" 2 :sig "(obj1 obj2)")
   ("equal" "lisp_equal" 2 :sig "(obj1 obj2)")
   ("even?" "lisp_evenp" 1 :sig "(integer)")
   ("odd?" "lisp_oddp" 1 :sig "(integer)")
   ("zero?" "lisp_zerop" 1 :sig "(number)")))
(define SciLisp-math-funs
  (list
   ("!=" "Sne" "lisp_numne" 2 :sig "(num1 num2)")
   ("*"  "Smul" "lisp_mul_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("+"  "Sadd" "lisp_add_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("1+" "Sinc" "lisp_inc" 1 :sig "(number)")
   ("-"  "Ssub" "lisp_sub_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("1-" "Sdec" "lisp_dec" 1 :sig "(number)")
   ("/" "Sdiv" "lisp_div_driver" 1 :restarg 1 :sig "(num1 num2)")
   ("<" "Slt" "lisp_numlt" 2 :sig "(num1 num2)")
   ("<=" "Sle" "lisp_numle" 2 :sig "(num1 num2)")
   ("=" "Seq" "lisp_numeq" 2 :sig "(num1 num2)")
   (">" "Sgt" "lisp_numgt" 2 :sig "(num1 num2)")
   (">=" "Sge" "lisp_numge" 2 :sig "(num1 num2)")
   ("abs" "Sabs" "lisp_abs" 1 :sig "(number)")
   ("cos" "Scos" "lisp_cos" 1 :sig "(number)")
   ("exp" "Sexp" "lisp_exp" 1 :sig "(number)")
   ("expt" "Sexpt" "lisp_pow" 2 :sig "(num1 num2)")
   ("log" "Slog" "lisp_log" 1 :sig "(number)")
   ("mod" "Smod" "lisp_mod" 2)
   ("pow" "Spow" "lisp_pow_driver" 1 :restarg 1)
   ("sin" "Ssin" "lisp_sin" 1 :sig "(number)")
   ("tan" "Stan" "lisp_tan" 1 :sig "(number)")))
(define SciLisp-cons-funs
  (list
    ("assoc" "Sassoc" "lisp_assoc" 2 :sig ("key list" "Skey list"))
    ("assq" "Sassq" "lisp_assq" 2 :sig ("key list" "Skey list"))
    ("cons" "Scons" "Cons" 2 :sig "(car cdr)")
    ("copy-tree" "Scopy-tree" "copy_tree" 1 :sig "(cell)")
    ("drop" "Sdrop" "cons_drop" 2 :sig "(list n)")
    ("last" "Slast" "lisp_last" 1 :sig "(list)")
    ("rand-list" "Srand-list" "rand_list" 1 :optargs 1)
    ("rassoc" "Srassoc" "lisp_rassoc" 2 :sig ("key list" "Skey list"))
    ("rassq" "Srassq" "lisp_rassq" 2 :sig ("key list" "Skey list"))
    ("push!" "Spush" "push_cons" 2 :sig "(new-val place)")
    ("reduce" "Sreduce" "cons_reduce" 2 :optargs 1 :sig "(seq function)")
    ("reverse!" "Sreverse!" "cons_nreverse" 1 :sig "(seq)")
    ("reverse" "Sreverse" "cons_reverse" 1 :sig "(seq)")
    ("set-car!" "Sset_car" "set_car" 2 :sig "(cell new-val)")
    ("set-cdr!" "Sset_cdr" "set_cdr" 2 :sig "(cell new-val)")
    ("split" "Ssplit" "cons_split" 1 :optargs 1)
    ("take" "Stake" "cons_take" 2 :sig "(list n)")))
(define SciLisp-array-funs
  (list
   ("aref" "Saref" "aref" 2 :sig "(array index)")
   ("array-map!" "Sarray-map!" "array_nmap" 2 :sig "(array map-fn)")
   ("array-map" "Sarray-map" "array_map" 2 :sig "(array map-fn)")
   ("array-qsort" "Sarray-qsort" "array_qsort" 2 :optargs 1
    :sig "(array predicate &optional in-place)")
   ("array-reduce" "Sarray-reduce" "array_reduce" 2 :optargs 1
    :sig "(array function &optional init)")
   ("array-reverse!" "Sarray-reverse!" "array_nreverse" 1 :sig "(array)")
   ("array-reverse" "Sarray-reverse" "array_reverse" 1 :sig "(array)")))
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
  '(("addhash" "Saddhash" "hashtable_add_entry" 3 :optargs 1
     :sig "(hash-table key value &optional option)")
    ("apply" "Sapply" "lisp_apply" 2 :optargs 1 :sig "(fun arglist &optional env)")
    ("array->list" "Sarray->list" "array_to_list" 1 :sig "(array)")
    ("ash" "Sash" "ash" 2 :sig "(value count)")
    ("assert" "Sassert" "lisp_assert" 1 :sig "(expr)")
    ("bigfloat" "Sbigfloat" "lisp_bigfloat" 1 :optargs 2
     :sig "(number-or-string &optional prec rnd)")
    ("bigint" "Sbigint" "lisp_bigint" 1 :sig "(number)")
;    ("c-ptr-val" "Sc-ptr-val" "lisp_dereference_c_ptr" 1 :sig "(pointer)")
    ("concat" "Sconcat" "lisp_concat" 0 :restarg 1 :sig "(&rest seqs)")
    ("ccall" "Sccall" "ffi_ccall" 5 :optargs 1
     :sig "(function-name libname return-type argtypes args &optional thread)")

    ("copy" "Scopy" "Fcopy" "lisp_copy" 1 :sig "(obj)");maybe call this duplicate
    ("char->string" "Schar->string" "lisp_char_to_string" 1 :sig "(character)")
    ("documentation" "Sdocumentation" "lisp_get_docstring" 1 :sig "(obj)")
    ("eval" "Seval" "lisp_eval" 1 :optargs 1 :sig "(expr &optional env)")
    ("exit" "Sexit" "lisp_exit" 0 :optargs 1 :sig "(&optional exit-code)")
    ("fclose" "Sfclose" "lisp_close" 1 :sig "(stream)")
    ("fopen" "Sfopen" "lisp_open" 1 :optargs 1 :sig "(file &optional mode)")
    ("format" "Sformat" "lisp_format" 1 :restarg 1 :sig "(format &rest objects)")
    ("fprint" "Sfprint" "lisp_fprint" 2 :sig "(obj stream)")
    ("fprintln" "Sfprintln" "lisp_fprintln" 2 :sig "(obj stream)")
    ("fputs" "Sfputs" "lisp_fputs" 2 :sig "(obj stream)")
    ;("ge" "Sge" "lisp_cmp_ge" 2 :sig "(num1 num2)")
    ("gensym" "Sgensym" "lisp_gensym" 0)
    ("get-type" "Sget_type" "getKeywordType" 1 :sig "(obj)")
    ("gethash" "Sgethash" "hashtable_get_entry" 2 :sig "(hash-table key)")
    ;("gt" "Sgt" "lisp_cmp_gt" 2 :sig "(num1 num2)")
    ("hash-table-entries" "Shash-table-entries" "hashtable_num_entries" 1 :sig "(hash-table)")
    ("hash-table-growth-factor" "Shash-table-growth-factor" "hashtable_growth_threshold" 1
     :sig "(hash-table)")
    ("hash-table-growth-size" "Shash-table-growth-size" "hashtable_growth_factor" 1
     :sig "(hash-table)"p)
    ("hash-table-size" "Shash-table-size" "hashtable_size" 1)
    ("iota" "Siota" "lisp_iota" 1 :optargs 4
     :sig "(start &optional stop step seq-type round)")
    ("identity" "Sidentity" "lisp_identity" 1 :sig "(form)")
    ("length" "Slength" "lisp_length" 1 :sig "(sequence)")
    ("list" "Slist" "lisp_list" 0 :restarg 1 :sig "(&rest objects)")
    ("list->array" "Slist->array" "array_from_list" 1 :sig "(list)")
    ("list-qsort" "Slist-qsort" "cons_qsort" 2 :sig "(list predicate)")
    ("load" "Sload" "lisp_load" 1 :sig "(file)")
;these need to be made into arith-driver functions
    ("logand" "Slogand" "lisp_logand" 2 :sig "(int1 int2)")
    ("logandn" "Slogandn" "lisp_logandn" 2 :sig "(int1 int2)")
    ("logior" "Slogior" "lisp_logior" 2 :sig "(int1 int2)")
    ("logxor" "Slogxor" "lisp_xor" 2 :sig "(int1 int2)")
    ("lognot" "Slognot" "lisp_lognot" 1 :sig "(integer)")
    ("make-c-ptr" "Smake-c-ptr" "make_c_ptr" 1 :optargs 1)
    ("make-hash-table" "Smake-hash-table" "makeHashtable" 0 :keyargs 7)
    ("make-string-input-stream" "Smake-string-input-stream" "make_string_input_stream" 1)
    ("make-tree" "Smake-tree" "make_tree" 1 :optargs 1 :restarg 1)
    ("mapcar" "Smapcar" "mapcar" 2 :sig "(map-fn list)")
    ("max" "Smax" "lisp_max_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("merge-sort" "Smerge-sort" "sequence_merge_sort" 2 :sig "(seq predicate)")
    ("min" "Smin" "lisp_min_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("not" "Snot" "lisp_not" 1)
    ("nth" "Snth" "lisp_nth" 2 :sig "(list n)")
    ("pop!" "Spop!" "pop_cons" 1 :sig "(place)")
    ("pprint" "Spprint" "lisp_pprint" 1 :sig "(object)")
    ("print" "Sprint" "lisp_print" 1 :sig "(object)")
    ("print-to-string" "Sprint-to-string" "lisp_print_to_string" 1 :sig "(object)")
    ("println" "Sprintln" "lisp_println" 1 :sig "(object)")

    ("pwd" "Spwd" "lisp_getcwd" 0 :sig "()")
    ("qsort" "Sqsort" "sequence_qsort" 2 :sig "(seq predicate)")
    ;raises a simple error
    ("raise" "Sraise" "lisp_error" 1 :optargs 1 :sig "(tag &optional value)")
    ("rand-array" "Srand_array" "rand_array" 1 :optargs 1)
    ("seed-rand" "Sseed_rand" "lisp_init_rand" 0 :optargs 1 :sig "(seed-val)")
    ("seed-rand-r" "Sseed_rand_r" "lisp_init_rand_r" 0 :optargs 1 :sig "(seed-val)")
    ("rand-float" "Srand_float" "lisp_randfloat" 0 :optargs 2 :sig "(&optional state scale)")
    ("rand-int" "Srand_int" "lisp_randint" 0 :optargs 2 :sig "(&optional state unsigned)")
    ("re-compile" "Sre_compile" "lisp_re_compile" 1 :optargs 1)
    ("re-match" "Sre_match" "lisp_re_match" 2 :optargs 3)
    ("re-subexpr" "Sre_subexpr" "lisp_get_re_backref" 2)
    ("read-string" "Sread_string" "lisp_read_string" 1 :sig "(string)")
    ("read" "Sread" "lisp_read" 1 :sig "(stream)")
;need to make generic

    ("round" "Sround" "lisp_round" 1 :optargs 1)
    ("set-aref!" "Sset_aref" "set_aref" 3 :sig "(array index new-val)")
    ("arglist" "Sarglist" "lisp_get_signature" 1 :sig "(function)")
    ("sort" "Ssort" "lisp_sort" 2 :sig "(seq sort-fn)")
    ("sqrt" "Ssqrt" "lisp_sqrt" 1 :sig "(number)")
    ("string->char" "Sstring_to_char" "lisp_string_to_char" 1 :sig "(string)")
    ("sxhash" "Ssxhash" "lisp_hash_sexp" 1 :sig "(object)")
    ("system" "Ssystem" "lisp_system" 1 :restarg 1)
    ("time" "Stime" "lisp_time" 0 :optargs 1 :restarg 1)
    ("type-of" "Stype_of" "type_of" 1 :sig "(object)")
    ("typeName" "StypeName" "lisp_typeName" 1 :sig "(object)")))

(defun intern-prims ()
  (let* ((prims (append SciLisp-prims SciLisp-types SciLisp-keywords)))
    (with-output-to-string
      (princ "void init_global_obarray(){\n")
      (dolist (prim prims)
        (princ (format "  cintern_unsafe(global_obarray,%s_val);\n" prim)))
      (princ "}\n"))))
(define SciLisp-keywords
  (mapcar (lambda (x) (concat "Q" x))());special things..?/reserved words
  (mapcar (lambda (x) (concat "E" x))()));builtin error types
(define SciLisp-keywords '())
(defun make-symbol-declarations()
  (with-temp-file "builtin_symbols.h"
    (insert "#ifndef _BUILTIN_SYMBOLS_H_\n#define _BUILTIN_SYMBOLS_H_\n")
    (dolist (err SciLisp-errors)
      (insert (format "extern symbol *E%s;\n" err)))
    (dolist (form SciLisp-special-forms)
      (insert (format "extern symbol *Q%s;\n" form)))
    (dolist (type SciLisp-Types)
      (insert (format "extern symbol *T%s;\n" type)))
    (insert "#endif\n")))
(defun get-hash (str)
  (shell-command-to-string (format "%s '%s'"
                                   (expand-file-name "../get_hash")
                                   str)))
(defun assq-val (key list)
  (return (cdr-safe (assq key list))))
(defmacro make-SciLisp-something (thing format-str &rest args)
  `(defun ,(intern (concat "make-SciLisp-" (symbol-name thing) "s")) (buf)
     (with-current-buffer buf
       (dolist (,thing ,(intern (concat "SciLisp-" (symbol-name thing) "s")))
         (insert (format  ,format-str ,@args))))))
(make-SciLisp-something type 
                        "MAKE_TYPE(%s,%s,%d,%s,NIL,sexp_%s);\n"
                        (concat "T" type) type (length type)
                        (get-hash type) type)
(make-SciLisp-something error 
                        "MAKE_SELF_QUOTING_SYMBOL(%s,%s,%d,%s,NIL)"
                        ;;if needed (replace-regexp-in-string "-" "_" err)
                        (concat "E" err) err (length err) (get-hash err))

;#define MAKE_SYMBOL(cname,lname,sym_len,sym_hashv,sym_val,proplist,const_sym) \
(make-SciLisp-something subr "MAKE_SYMBOL(%s,\"%s\",%d,%s,%s,NIL,%d);\n"
                        (assq-val :fname subr) (assq-val :lname subr)
                        (length (assq-val :lname subr))
                        (get-hash (assq-val :lname subr)) (assq-val :cname subr)
                        (assq-val :const subr))
(make-SciLisp-something global "MAKE_SYMBOL(%s,\"%s\",%d,%s,%s,NIL,%d);\n"
                        (assq-val :cname global) (assq-val :lname global )
                        (length (assq-val :lname global))
                        (get-hash (assq-val :lname global)) (assq-val :value global)
                        (assq :const global))
