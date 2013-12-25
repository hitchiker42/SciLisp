#!/usr/bin/emacs --script
;TODO: change what is currently :doc to :sig so :doc
;can hold actual documenation while :sig is the function signature
;;utitily functions
(require 'cl)
(defvar current-dir-name
  (if load-in-progress
      (file-name-directory load-file-name)
    (file-name-directory (pwd))))
;Just to quiet the byte compilier
(defmacro define (var defn)
  "define and/or set a variable"
  (progn
    `(defvar ,var)
    `(setq ,var ,defn)))
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
(defun iota (num &optional end)
  (let ((ls nil)
        (end (if (null end) 0 end))
        (i num))
    (while (>= i end)
      (push i ls)
      (setq i (1- i)))
    ls))
(defun make-itemized-docstring (numargs item-name)
  (concat "("
          (mapconcat (lambda (x) x)
                     (mapcar (lambda (x) (format "%s%d" item-name x))
                             (iota numargs 1))
                     " ")
          ")"))
(defun bignum-ops (op-list typename nargs &optional item-name)
  (mapcar
   (lambda (op)
     (mk-prim (format "%s-%s" typename op) 
              (format "lisp_%s_%s" typename op) 
              nargs :sig
              (make-itemized-docstring 
               nargs (if (null item-name) typename item-name))))
   op-list))
(defun mpfr-binops (op-list)
  (bignum-ops op-list "bigfloat" 2))
(defun mpfr-unops (op-list)
  (bignum-ops op-list "bigfloat" 1))
(defun mpz-binops (op-list)
  (bignum-ops op-list "bigint" 2))
(defun mk-predicate (name)
  (mk-prim name (format "lisp_%s" name) 1))
;;lists of primitives
(define *cadrs*
  (let ((ls ()))
    (dolist (i '("a" "d"))
      (dolist (j '("a" "d" ""))
        (dolist (k '("a" "d" ""))
          (dolist (l '("a" "d" ""))
            (add-to-list
             'ls (list (concat "c" i j k l "r") (concat "c" i j k l "r") 1))))))
    ls))
(define mpz-binops-list '("add" "sub" "mul" "mod" "cdiv_q" "fdiv_q" "tdiv_q"
                          "cdiv_r""fdiv_r" "tdiv_r" "and" "ior" "xor"
                          "gt" "eq" "lt" "ge" "le" "ne"))
(define mpfr-binops-list '("add" "sub" "mul" "div" "pow"
                           "gt" "eq" "lt" "ge" "le" "ne"))
(define mpfr-unops-list '("log" "exp" "cos" "sin" "tan"))
(define arith-driver-funs-list '("add" "sub" "mul" "div" "pow" "min" "max"))
;(mapcar (lambda (x) (list x (concat "lisp_" x "_driver") 1 :restarg 1)) arith-driver-funs-list)
; the doc field here will just be the list of arguments
; it'll expand to (<funname>':' <arglist>)
; ':' will work for docstrings since it's a reserved character
;i.e. there can't be a function cons: , it'd have to be quoted at least
(define basic-prims-list
  '(("!=" "lisp_numne" 2 :sig "(num1 num2)")
    ("*" "lisp_mul_driver" 1 :restarg 1 :sig "(num1 num2)")
    ("+" "lisp_add_driver" 1 :restarg 1 :sig "(num1 num2)")
    ("++" "lisp_inc" 1 :sig "(number)")
    ("-" "lisp_sub_driver" 1 :restarg 1 :sig "(num1 num2)")
    ("--" "lisp_dec" 1 :sig "(number)")
    ("/" "lisp_div_driver" 1 :restarg 1 :sig "(num1 num2)")
    ("<" "lisp_numlt" 2 :sig "(num1 num2)")
    ("<=" "lisp_numle" 2 :sig "(num1 num2)")
    ("=" "lisp_numeq" 2 :sig "(num1 num2)")
    (">" "lisp_numgt" 2 :sig "(num1 num2)")
    (">=" "lisp_numge" 2 :sig "(num1 num2)")
    ("abs" "lisp_abs" 1 :sig "(number)")
    ("add-num" "lisp_add_num" 2 :sig "(num1 num2)")
    ("addhash" "hashtable_add_entry" 3 :optargs 1)
    ("apply" "lisp_apply" 2 :optargs 1 :sig "(fun arglist &optional env)")
    ("aref" "aref" 2 :sig "(array index)")
    ("array->list" "array_to_list" 1 :sig "(array)")
    ("array-map!" "array_nmap" 2 :sig "(array map-fn)")
    ("array-map" "array_map" 2 :sig "(array map-fn)")
    ("array-qsort" "array_qsort" 2 :optargs 1 :sig "(array predicate &optional in-place)")
    ("array-reduce" "array_reduce" 2 :optargs 1 :sig "(array function &optional init)")
    ("array-reverse!" "array_nreverse" 1 :sig "(array)")
    ("array-reverse" "array_reverse" 1 :sig "(array)")
    ("ash" "ash" 2 :sig "(value count)")
    ("assert" "lisp_assert" 1 :sig "(expr)")
    ("assert-eq" "lisp_assert_eq" 2 :sig "(obj1 obj2)")
    ("assert-equal" "lisp_assert_equal" 2 :sig "(obj1 obj2)")
    ("assert-not-eq" "lisp_assert_not_eq" 2 :sig "(obj1 obj2)")
    ("assert-not-equal" "lisp_assert_not_equal" 2 :sig "(obj1 obj2)")
    ("bigfloat" "lisp_bigfloat" 1 :optargs 2)
    ("bigint" "lisp_bigint" 1 :sig "(number)")
    ("c-ptr-val" "lisp_dereference_c_ptr" 1)
    ("cat" "lisp_cat" 0 :restarg 1 :sig "(&rest seqs)")
    ("ccall" "ccall" 5 :optargs 1)
    ("cons" "Cons" 2 :sig "(car cdr)")
    ("cos" "lisp_cos" 1 :sig "(number)")
    ("div-num" "lisp_div_num" 2 :sig "(num1 num2)")
    ("drand" "lisp_randfloat" 0 :optargs 1 :sig "(&optional scale)")
    ("driv-!=" "lisp_ne_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("driv-<" "lisp_lt_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("driv-<=" "lisp_le_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("driv-=" "lisp_eq_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("driv->" "lisp_gt_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("driv->=" "lisp_ge_driver" 1 :restarg 1 :sig "(number &rest numbers)")
    ("drop" "cons_drop" 2 :sig "(list n)")
    ("eq" "lisp_eq" 2 :sig "(obj1 obj2)")
    ("eql" "lisp_eql" 2 :sig "(obj1 obj2)")
    ("equal" "lisp_equal" 2 :sig "(obj1 obj2)")
    ("eval" "lisp_eval" 1 :optargs 1 :sig "(expr &optional env)")
    ("even?" "lisp_evenp" 1 :sig "(number)")
    ("exp" "lisp_exp" 1 :sig "(number)")
    ("expt" "lisp_pow" 2 :sig "(num1 num2)")
    ("fclose" "lisp_close" 1 :sig "(stream)")
    ("fopen" "lisp_open" 1 :optargs 1 :sig "(file &optional mode)")
    ("fprint" "lisp_fprint" 2 :sig "(obj stream)")
    ("fprintln" "lisp_fprintln" 2 :sig "(obj stream)")
    ("fputs" "lisp_fputs" 2 :sig "(obj stream)")
    ("ge" "lisp_cmp_ge" 2 :sig "(num1 num2)")
    ("gensym" "lisp_gensym" 0)
    ("get-type" "getKeywordType" 1)
    ("gethash" "hashtable_get_entry" 2)
    ("gt" "lisp_cmp_gt" 2);("eq" "lisp_cmp_eq" 2)
    ("hash-table-entries" "hashtable_num_entries" 1)
    ("hash-table-growth-factor" "hashtable_growth_threshold" 1)
    ("hash-table-growth-size" "hashtable_growth_factor" 1)
    ("hash-table-size" "hashtable_size" 1)
    ("iota" "lisp_iota" 1 :optargs 4 :sig "(start &optional stop step seq-type round)")
    ("last" "lisp_last" 1 :sig "(list)")
    ("le" "lisp_cmp_le" 2 :sig "(num1 num2)")
    ("length" "lisp_length" 1 :sig "(sequence)")
    ("list" "lisp_list" 0 :restarg 1 :sig "(&rest objects)")
    ("list->array" "array_from_list" 1 :sig "(list)")
    ("list-qsort" "cons_qsort" 2 :sig "(list predicate)")
    ("load" "lisp_load" 1 :sig "(file)")
    ("log" "lisp_log" 1 :sig "(number)")
    ("logand" "lisp_logand" 2)
    ("logand" "lisp_logand" 2)
    ("logor" "lisp_logor" 2)
    ("logor" "lisp_logor" 2)
    ("logxor" "lisp_xor" 2)
    ("logxor" "lisp_xor" 2)
    ("lrand" "lisp_randint" 0 :optargs 1)
    ("lt" "lisp_cmp_lt" 2)
    ("make-c-ptr" "make_c_ptr" 1 :optargs 1)
    ("make-hash-table" "makeHashtable" 0 :keyargs 7)
    ("make-hash-table-default" "make_hashtable_default" 0)
    ("make-string-input-stream" "make_string_input_stream" 1)
    ("make-tree" "make_tree" 1 :optargs 1 :restarg 1)
    ("mapcar" "mapcar" 2 :sig "(list map-fn)")
;    ("max" "lisp_max" 2)
    ("max" "lisp_max_driver" 1 :restarg 1)
    ("merge-sort" "sequence_merge_sort" 2)
;    ("min" "lisp_min" 2)
    ("min" "lisp_min_driver" 1 :restarg 1)
    ("mod" "lisp_mod" 2)
    ("mul-num" "lisp_mul_num" 2)
    ("ne" "lisp_cmp_ne" 2)
    ("not" "lisp_not" 1)
    ("not-eq" "lisp_not_eq" 2)
    ("not-equal" "lisp_not_equal" 2)
    ("nth" "lisp_nth" 2)
    ("odd?" "lisp_oddp" 1)
    ("pop!" "pop_cons" 1)
    ("pow" "lisp_pow_driver" 1 :restarg 1)
    ("pprint" "lisp_pprint" 1)
    ("print" "lisp_print" 1)
    ("print-to-string" "lisp_print_to_string" 1)
    ("println" "lisp_println" 1)
    ("push!" "push_cons" 2)
    ("pwd" "lisp_getcwd" 0 :sig "()")
    ("qsort" "sequence_qsort" 2)
    ("raise-error" "lisp_error" 1)
    ("rand-array" "rand_array" 1 :optargs 1)
    ("rand-list" "rand_list" 1 :optargs 1)
    ("re-compile" "lisp_re_compile" 1 :optargs 1)
    ("re-match" "lisp_re_match" 2 :optargs 3)
    ("re-subexpr" "lisp_get_re_backref" 2)
    ("read" "lisp_read" 1)
    ("read-string" "lisp_read_string" 1)
    ("reduce" "cons_reduce" 2)
    ("reverse!" "cons_nreverse" 1)
    ("reverse" "cons_reverse" 1)
    ("round" "lisp_round" 1 :optargs 1)
    ("set-car!" "set_car" 2 :sig "(cell new-val)")
    ("set-cdr!" "set_cdr" 2 :sig "(cell new-val)")
    ("sin" "lisp_sin" 1 :sig "(number)")
    ("sort" "lisp_sort" 2 :sig "(seq sort-fn)")
    ("split" "cons_split" 1 :optargs 1)
    ("sqrt" "lisp_sqrt" 1 :sig "(number)")
    ("sub-num" "lisp_sub_num" 2)
    ("sxhash" "lisp_hash_sexp" 1 :sig "(object)")
    ("system" "lisp_system" 1 :restarg 1)
    ("take" "cons_take" 2)
    ("tan" "lisp_tan" 1 :sig "(number)")
    ("time" "lisp_time" 0 :optargs 1 :restarg 1)
    ("type-of" "typeOf" 1 :sig "(object)")
    ("typeName" "lisp_typeName" 1 :sig "(object)")
    ("zero?" "lisp_zerop" 1)))
(define predicates '("arrayp" "consp" "numberp" "nilp" "symbolp" "bigintp" "bigfloatp" "stringp"
                     "bignump" "errorp" "functionp" "streamp"))
(define basic-SciLisp-prims
  (mapcar (lambda (x) (apply #'mk-prim x)) basic-prims-list))
(define SciLisp-prims
  (append
   basic-SciLisp-prims
  (mpfr-binops mpfr-binops-list)
  (mapcar #'mk-predicate predicates)
  (mapcar  (lambda (x) (apply #'mk-prim x)) *cadrs*)))
(define SciLisp-prim-macros
  (list (mk-prim "incf" "lisp_inc_ref" 1)
        (mk-prim "decf" "lisp_dec_ref" 1)
        (mk-prim "and" "lisp_and" 0 :restarg 1)
        (mk-prim "or" "lisp_or" 0 :restarg 1)))
(setq SciLisp-prims (delete-duplicates SciLisp-prims :test #'equal))
(define SciLisp-globals
  (list (vector "lisp_mach_eps" "Meps" 1) (vector "lisp_pi" "pi" 1)
        (vector "lisp_euler" "e" 1) (vector "lisp_max_long" "max-int64" 1)
        (vector "lisp_NIL"  "nil" 1) (vector "lisp_LISP_TRUE" "#t" 1)
        (vector "lisp_LISP_FALSE" "#f" 1)
        (vector "lisp_stdin" "stdin" 0) (vector "lisp_stdout" "stdout" 0)
        (vector "lisp_stderr" "stderr" 0)
        (vector "lisp_double_0" "double-0" 1) (vector "lisp_double_1" "double-1" 1)
        (vector "lisp_long_0" "long-0" 1) (vector "lisp_long_1" "long-1" 1)
        (vector "lisp_ans" "ans" 0)
        ;(vector "lisp_max_real64" "max-real64")(vector "lisp_min_real64" "min-real64")
        (vector "lisp_bigfloat_0" "bigfloat-0" 1)
        (vector "lisp_bigfloat_1" "bigfloat-1" 1)
        (vector "lisp_bigfloat_e" "bigfloat-e" 1)
        (vector "lisp_bigfloat_pi" "bigfloat-pi" 1)
        (vector "lisp_bigint_0" "bigint-0" 1)
        (vector "lisp_bigint_1" "bigint-1" 1)))
(define SciLisp-Types
  '("int8" "int16" "int32" "int64" "uint8" "uint16" "uint32" "uint64" "error"
    "real32" "real64" "bigint" "bigfloat" "char" "string" "array" "stream"
    "list" "fun" "symbol" "macro" "type" "keyword" "hashtable" "spec" "regex"
    "nil" "dpair" "lenv" "env" "obarray" "funargs" "true" "false" "uninterned"
    "cons"))
(defun mk-type-tests (type)
  (let
      ((type-macro
        (format "#define %sP(obj) (obj.tag ==_%s)\n"
                upcase(type) type))
       (type-function-c
        (format "int is_%s(sexp obj){\n  return %sP(obj);\n}\n"
                type upcase(type)))
       (type-function-lisp
        (format
         "sexp lisp_%sp(sexp obj){
  return(%sP(obj)?LISP_TRUE:LISP_FALSE);\n}\n" type upcase(type))))
    (list type-macro type-function-c type-function-lisp)))
;;need to actually use this, because I don't do anything with it right now
(define SciLisp-aliases
  (mapcar
   (lambda (x)
     (list
      (concat "lisp_" x)
      (concat "\"" (replace-regexp-in-string "\\(.*\\)p" "\\1?" x) "\"")
      1))
   predicates))
;  '(("lisp_consp" "\"cons?\"" 1)))
(defvar initPrimsObarray-header)
(setq initPrimsObarray-header
"mpz_t *lisp_mpz_1,*lisp_mpz_0;
mpfr_t *lisp_mpfr_1,*lisp_mpfr_0;
static void initPrimsObarray(obarray *ob,env* ob_env);
void initPrims(){
if(initPrimsFlag){
initPrimsFlag=0;
} else {
return;
}
globalObarray=xmalloc(sizeof(obarray));
obarray_entry** global_buckets=xmalloc(128*sizeof(obarray_entry*));
*globalObarray=(obarray)
{.buckets=global_buckets,.size=128,.used=0,.entries=0,.capacity=0.0,
                .capacity_inc=(1.0/(128*10)),.gthresh=0.75,.gfactor=2,
                .is_weak_hash=0,.hash_fn=fnv_hash};
keywordObarray=xmalloc(sizeof(obarray));
obarray_entry** keyword_buckets=xmalloc(128*sizeof(obarray_entry*));
*keywordObarray=(obarray)
{.buckets=keyword_buckets,.size=128,.used=0,.entries=0,.capacity=0.0,
                .capacity_inc=(1.0/(128*10)),.gthresh=0.75,.gfactor=2,
                .is_weak_hash=0,.hash_fn=fnv_hash};
globalObarrayEnv=xmalloc(sizeof(obarray_env));
keywordObarrayEnv=xmalloc(sizeof(obarray_env));
topLevelEnv=xmalloc(sizeof(env));
globalObarrayEnv->enclosing=keywordObarrayEnv->enclosing=0;
globalObarrayEnv->head=globalObarray;
keywordObarrayEnv->head=keywordObarray;
initPrimsObarray(globalObarray,(env*)globalObarrayEnv);
*topLevelEnv=(env){.enclosing=globalObarrayEnv->enclosing,
.head={.ob=globalObarrayEnv->head},.tag=_obEnv};
mpfr_set_default_prec(256);
mp_set_memory_functions(GC_MALLOC_1,GC_REALLOC_3,GC_FREE_2);
set_global_vars();
srand48(time(NULL));
INIT_SYNONYM(lisp_consp,\"cons?\",1);
}
static void initPrimsObarray(obarray *ob,env* ob_env){

")
(defvar initPrimsObarray-suffix)
(setq initPrimsObarray-suffix
"}
")
(defvar primc-suffix "#undef DEFUN
")
(defun make-type-cons (type)
  (if (consp type)
      type
    (cons type (concat "_" type))))
(make-type-cons "int8")  
(defun defType-format (type)
  (let ((type-cons (make-type-cons type)))
  (format "DEFTYPE(%s,%s);\n" (car type-cons) (cdr type-cons))))
(defun makeType-format (type)
  (let ((type-cons (make-type-cons type)))
  (format "MAKE_TYPE(%s,%s);\n" (car type-cons) (cdr type-cons))))
(defun makeGlobals-format (global)
  (format
   "MAKE_GLOBAL(\"%s\",%s,%d);\n" (aref global 1) (aref global 0)
   (aref global 2)))
(defun make-lookupType ()
  (let ((type-case (generate-new-buffer "type-case")))
    (with-current-buffer type-case
      (insert "#define mkTypeCase(type,tag) case tag: return type\n"
              "sexp typeOfTag(_tag tag){\n  switch(tag){\n")
      (dolist (type SciLisp-Types)
        (insert (format "    mkTypeCase(%s,%s);\n"
                        (concat "Q" type)(concat "_" type))))
      (insert "  }\n}\n"
              "sexp typeOf(sexp obj){\n  return typeOfTag(obj.tag);\n}\n")
    (prog1 (buffer-string)
      (kill-buffer type-case)))))
(defun initGlobals-format (global)
  (format "INIT_GLOBAL(%s);\n" global))
(defmacro prim-val (keysym)
  `(cdr (assq ,keysym prim)))
(defun make-signature (prim)
  (format "%s: %s" (prim-val :lname) (prim-name :sig)))
(defun primc-format (prim)
  (format
   "DEFUN(\"%s\",%s,%d,%d,%d,%d,%d);\n"
   (prim-val :lname) (prim-val :cname) (prim-val :minargs) (prim-val :optargs)
   (prim-val :keyargs) (prim-val :restarg) (prim-val :maxargs)))
(defun defmacro-format (prim)
  (format
   "DEFMACRO(\"%s\",%s,%d,%d,%d,%d,%d,%d);\n"
   (prim-val :lname) (prim-val :cname) (prim-val :minargs) (prim-val :optargs)
   (prim-val :keyargs) (prim-val :restarg) (prim-val :maxargs)(1+ (prim-val :maxargs))))
(defun primh-format-generic (name maxargs)
  (format "DEFUN(%s,%s);\n" name maxargs))
(defun primh-format (prim)
  (primh-format-generic
   (cdr (assq :cname prim))
   (cdr (assq :maxargs prim))))
(defun primh-format-macro (macro)
  (primh-format-generic
   (cdr (assq :cname macro))
   (1+ (cdr (assq :maxargs macro)))))
(defun initPrimsObarray-format (prim)
  (format "INIT_SYMBOL(%s);\n"
          (cdr (assq :cname prim))))
(defun initPrimMacrosObarray-format (prim)
  (format "INIT_MACRO_SYMBOL(%s);\n"
          (cdr (assq :cname prim))))
(defun makePrimSymbols-format(prim)
  (format "MAKE_SYMBOL(\"%s\",%s,%s);\n"
          (cdr (assq :lname prim))(cdr (assq :cname prim))
          (shell-command-to-string
           (format "%s '%s'"
            (expand-file-name "../fnv_hash" current-dir-name)
            (cdr (assq :cname prim))))))
(defun makePrimMacroSymbols-format (prim)
  (format "MAKE_MACRO_SYMBOL(\"%s\",%s,%s);\n"
          (cdr (assq :lname prim))(cdr (assq :cname prim))
          (shell-command-to-string
           (format "%s '%s'"
            (expand-file-name "../fnv_hash" current-dir-name)
            (cdr (assq :cname prim))))))
(defun generate-SciLisp-prims()
  (let ((primh (generate-new-buffer "primh"))
        (initPrimsObarray (generate-new-buffer "initPrimsObarray"))
        (primc (generate-new-buffer "primc"))
        (primSyms (generate-new-buffer "primSyms")))
    (princ initPrimsObarray-header initPrimsObarray)
    (dolist (prim SciLisp-prims)
      (princ (primc-format prim) primc)
      (princ (primh-format prim) primh)
      (princ (makePrimSymbols-format prim) primSyms)
      (princ (initPrimsObarray-format prim) initPrimsObarray))
    (dolist (global SciLisp-globals)
      (princ (makeGlobals-format global) primSyms)
      (princ (initGlobals-format (aref global 0)) initPrimsObarray))
    (dolist (type SciLisp-Types)
      (princ (defType-format type) primh)
      (princ (makeType-format type) primSyms)
      (princ (initGlobals-format type) initPrimsObarray))
    (dolist (macro SciLisp-prim-macros)
      (princ (defmacro-format macro) primc)
      (princ (primh-format-macro macro) primh)
      (princ (makePrimMacroSymbols-format macro) primSyms)
      (princ (initPrimMacrosObarray-format macro) initPrimsObarray))
    (princ initPrimsObarray-suffix initPrimsObarray)
    (princ primc-suffix primc)
    (with-current-buffer primh
      (goto-char (point-min))
      (insert-file-contents
       (expand-file-name "primh_header.h" current-dir-name))
      (goto-char (point-max))
      (insert "#undef DEFUN\n#endif")
      (write-file (expand-file-name "../prim.h" current-dir-name))
      (kill-buffer))
    (with-current-buffer primc
      (goto-char (point-min))
      (insert-file-contents
       (expand-file-name "primc_header.c" current-dir-name))
      (goto-char (point-max))
      (insert (make-lookupType))
      (insert-buffer-substring primSyms)
      (kill-buffer primSyms)
      (insert-buffer-substring initPrimsObarray)
      (kill-buffer initPrimsObarray)
      (write-file (expand-file-name "../prim.c" current-dir-name))
      (kill-buffer))
    (progn (kill-buffer primc)(kill-buffer primh)(kill-buffer initPrimsObarray))))
(if load-in-progress
    (let
        ((auto-mode-alist nil)
         (vc-handled-backends nil))
      (generate-SciLisp-prims)))
