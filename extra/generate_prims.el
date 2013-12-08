#!/usr/bin/emacs --script
;;utitily functions
(defvar current-dir-name (file-name-directory load-file-name))
(defmacro define (var defn)
  (progn
    `(defvar ,var)
    `(setq ,var ,defn)))
(defun collect (fun args)
  (let ((ls ()))
    (dolist (i args)
      (push (funcall fun i) ls))
    (nreverse ls)))
(defun indent-buffer ()
  "Indent entire buffer using indent-region"
  (interactive)
  (indent-region (point-min) (point-max)))
(defun mkPrimBasic (lname cname numargs)
  `((:lname . ,lname) (:cname . ,cname) (:minargs . ,numargs)
    (:maxargs . ,numargs) (:optargs . 0) (:keyargs . 0) (:restarg . 0)))
(defun bignum-ops (op-list typename nargs)
  (collect (lambda (op)
`((:lname . ,(format "%s-%s" typename op))
  (:cname . ,(format "lisp_%s_%s" typename op)) (:minargs . ,nargs) (:maxargs . ,nargs)
  (:optargs . 0) (:keyargs . 0) (:restarg . 0))) op-list))
(defun mpfr-binops (op-list)
  (bignum-ops op-list "bigfloat" 2))
(defun mpfr-unops (op-list)
  (bignum-ops op-list "bigfloat" 1))
(defun mpz-binops (op-list)
  (bignum-ops op-list "bigint" 2))
(defun mk-predicate (name)
  `((:lname . ,name) (:cname . ,(format "lisp_%s" name)) (:minargs . 1)
    (:maxargs . 1) (:optargs . 0) (:keyargs . 0) (:restarg . 0)))
(defun arith-driver-funs (name)
  `((:lname . ,name) (:cname . ,(format "lisp_%s_driver" name)) (:minargs . 1)
    (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 1)))
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
(define basic-prims-list
  '(("+" "lisp_add" 2) ("-" "lisp_sub" 2) ("*" "lisp_mul" 2) ("/" "lisp_div" 2)
    ("<" "lisp_lt" 2) (">" "lisp_gt" 2) (">=" "lisp_gte" 2) ("<=" "lisp_lte" 2)
    ("!=" "lisp_ne" 2) ("=" "lisp_numeq" 2) ("++" "lisp_inc" 1) ("--" "lisp_dec" 1)
    ("cons" "Cons" 2) ("set-car!" "set_car" 2) ("set-cdr!" "set_cdr" 2)
    ("last" "lisp_last" 1) ("push!" "push_cons" 2) ("pop!" "pop_cons" 1)
    ("mapcar" "mapcar" 2) ("reduce" "cons_reduce" 2)("list-qsort" "cons_qsort" 2)
    ("list-mergesort" "merge_sort" 2)("qsort" "sequence_qsort" 2)
    ("length" "lisp_length" 1) ("aref" "aref" 2) ("array->list" "array_to_list" 1)
    ("typeName" "lisp_typeName" 1) ("print" "lisp_print" 1)
    ("fclose" "lisp_close" 1) ("fputs" "lisp_fputs" 2) ("fprint" "lisp_fprint" 2)
    ("fprintln" "lisp_fprintln" 2) ("pwd" "lisp_getcwd" 0) ("eq" "lisp_eq" 2)
    ("logxor" "lisp_xor" 2) ("logand" "lisp_logand" 2) ("logor" "lisp_logor" 2)
    ("ash" "ash" 2) ("expt" "lisp_pow" 2) ("sqrt" "lisp_sqrt" 1)
    ("cos" "lisp_cos" 1) ("sin" "lisp_sin" 1) ("tan" "lisp_tan" 1)
    ("exp" "lisp_exp" 1) ("log" "lisp_log" 1) ("min" "lisp_min" 2)
    ("max" "lisp_max" 2) ("mod" "lisp_mod" 2) ("abs" "lisp_abs" 1)
    ("eq" "lisp_eq" 2) ("eql" "lisp_eql" 2) ("equal" "lisp_equal" 2)
    ("even?" "lisp_evenp" 1)("odd?" "lisp_oddp" 1)("zero?" "lisp_zerop" 1)
    ("nth" "lisp_nth" 2)("assert-equal" "lisp_assert_equal" 2)
    ("list->array" "array_from_list" 1)("raise-error" "lisp_error" 1)
    ("not" "lisp_not" 1)("assert" "lisp_assert" 1)
    ("assert-eq" "lisp_assert_eq" 2)("gensym" "lisp_gensym" 0)
    ("assert-not-eq" "lisp_assert_not_eq" 2)
    ("assert-not-equal" "lisp_assert_not_equal" 2)
    ("reverse!" "cons_nreverse" 1)("drop" "cons_drop" 2)
    ("take" "cons_take" 2)("reverse" "cons_reverse" 1)
    ("load" "lisp_load" 1)("array-reverse" "array_reverse" 1)
    ("array-reverse!" "array_nreverse" 1)("get-type" "getKeywordType" 1)
    ("sort" "lisp_sort" 2)("gt" "lisp_cmp_gt" 2)("eq" "lisp_cmp_eq" 2)
    ("lt" "lisp_cmp_lt" 2)("ge" "lisp_cmp_ge" 2)("le" "lisp_cmp_le" 2)
    ("ne" "lisp_cmp_ne" 2)("read" "lisp_read" 1)
    ("read-string" "lisp_read_string" 1)
    ("pprint" "lisp_pprint" 1)
    ("print-to-string" "lisp_print_to_string" 1)
    ("make-string-input-stream" "make_string_input_stream" 1)))
(define predicates '("arrayp" "consp" "numberp" "nilp" "symbolp" "bigintp" "bigfloatp" "stringp" "bignump" "errorp" "functionp" "streamp"))
(define basic-SciLisp-prims
  (collect (lambda (x) (apply #'mkPrimBasic x)) basic-prims-list))
(define SciLisp-prims (append
                       basic-SciLisp-prims
  '(
    ((:lname . "sum") (:cname . "lisp_sum") (:minargs . 1) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 1))
    ((:lname . "iota") (:cname ."lisp_iota") (:minargs . 1) (:maxargs . 5)
     (:optargs . 4) (:keyargs . 0) (:restarg . 0))
    ;array functions
    ((:lname . "array-iota") (:cname . "array_iota") (:minargs . 1)
     (:maxargs . 4) (:optargs . 3) (:keyargs . 0) (:restarg . 0))
    ((:lname . "array-qsort") (:cname . "array_qsort")(:minargs . 2)
     (:maxargs . 3)(:optargs . 1)(:keyargs . 0)(:restarg . 0))
    ((:lname . "array-map") (:cname . "array_map")(:minargs . 2)
     (:maxargs . 2)(:optargs . 0)(:keyargs . 0)(:restarg . 0))
    ((:lname . "array-map!") (:cname . "array_nmap")(:minargs . 2)
     (:maxargs . 2)(:optargs . 0)(:keyargs . 0)(:restarg . 0))
    ((:lname . "array-reduce") (:cname . "array_reduce")(:minargs . 2)
     (:maxargs . 3)(:optargs . 1)(:keyargs . 0)(:restarg . 0))
    ((:lname . "make-tree")(:cname . "make_tree")(:minargs . 1)(:maxargs . 3)
     (:optargs . 1)(:keyargs . 0)(:restarg . 1))
    ((:lname . "rand-array") (:cname . "rand_array") (:minargs . 1)
     (:maxargs . 2) (:optargs . 1) (:keyargs . 0) (:restarg . 0))
    ((:lname . "rand-list") (:cname . "rand_list") (:minargs . 1)
     (:maxargs . 2) (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ;meta information/ read eval print / system ctl
    ((:lname . "typeName") (:cname ."lisp_typeName") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "type-of") (:cname . "typeOf") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "print") (:cname ."lisp_print") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "println") (:cname ."lisp_println") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "eval") (:cname ."lisp_eval") (:minargs . 1) (:maxargs . 2)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "fopen") (:cname . "lisp_open") (:minargs . 1) (:maxargs . 2)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "fclose") (:cname . "lisp_close") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "fputs") (:cname . "lisp_fputs") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "fprint") (:cname ."lisp_fprint") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "fprintln") (:cname ."lisp_fprintln") (:minargs . 2)
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "cat") (:cname . "lisp_cat") (:minargs . 1) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 1))
     ((:lname . "pwd") (:cname . "lisp_getcwd") (:minargs . 0) (:maxargs . 0)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "system") (:cname . "lisp_system") (:minargs . 1)
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 1))
     ((:lname . "ccall") (:cname . "ccall") (:minargs . 5) (:maxargs . 6)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ;bit twiddling
     ((:lname . "logxor") (:cname ."lisp_xor") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "logand") (:cname ."lisp_logand") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "logor") (:cname ."lisp_logor") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "ash") (:cname ."ash") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ;math functions
     ((:lname . "round") (:cname ."lisp_round") (:minargs . 1) (:maxargs . 2)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "drand") (:cname ."lisp_randfloat") (:minargs . 0) (:maxargs . 1)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "lrand") (:cname ."lisp_randint") (:minargs . 0) (:maxargs . 1)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint") (:cname . "lisp_bigint") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat") (:cname . "lisp_bigfloat") (:minargs . 1)
      (:maxargs . 3) (:optargs . 2) (:keyargs . 0) (:restarg . 0))
     ((:lname . "apply") (:cname . "lisp_apply") (:minargs . 2) (:maxargs . 3)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "re-compile") (:cname . "lisp_re_compile") (:minargs . 1)
      (:maxargs . 2)(:optargs . 1)(:keyargs . 0)(:restarg . 0))
     ((:lname . "re-match") (:cname . "lisp_re_match") (:minargs . 2)
      (:maxargs . 5) (:optargs . 3) (:keyargs . 0)(:restarg . 0))
     ((:lname . "re-subexpr")(:cname . "lisp_get_re_backref")(:minargs . 2)
      (:maxargs . 2)(:optargs . 0)(:keyargs . 0)(:restarg . 0))
     ((:lname . "make-cpointer")(:cname . "make_c_ptr")(:minargs . 2)
      (:maxargs . 2)(:optargs . 0)(:keyargs . 0)(:restarg . 0))
     ((:lname . "list")(:cname . "lisp_list")(:minargs . 0)(:maxargs . 1)
      (:optargs . 0)(:keyargs . 0)(:restarg . 1))
     ((:lname . "split")(:cname . "cons_split")(:minargs . 1)(:maxargs . 2)
      (:optargs . 1)(:keyargs . 0)(:restarg . 0)))
  (mpfr-binops mpfr-binops-list)
  (collect #'mk-predicate predicates)
  (collect #'arith-driver-funs arith-driver-funs-list)
  (collect  (lambda (x) (apply #'mkPrimBasic x)) *cadrs*)))
(require 'cl)
(delete-duplicates SciLisp-prims :test #'equal)
(define SciLisp-globals
  (list (vector "lisp_mach_eps" "Meps" 1) (vector "lisp_pi" "pi" 1)
        (vector "lisp_euler" "e" 1) (vector "lisp_max_long" "max_long" 1)
        (vector "lisp_NIL"  "nil" 1) (vector "lisp_LISP_TRUE" "t" 1)
        (vector "lisp_LISP_FALSE" "#f" 1)
        (vector "lisp_stdin" "stdin" 0) (vector "lisp_stdout" "stdout" 0)
        (vector "lisp_stderr" "stderr" 0)
        (vector "lisp_double_0" "double-0" 1) (vector "lisp_double_1" "double-1" 1)
        (vector "lisp_long_0" "long-0" 1) (vector "lisp_long_1" "long-1" 1)
        (vector "lisp_ans" "ans" 0)
        (vector "lisp_bigfloat_0" "bigfloat-0" 1)
        (vector "lisp_bigfloat_1" "bigfloat-1" 1) 
        (vector "lisp_bigint_0" "bigint-0" 1)
        (vector "lisp_bigint_1" "bigint-1" 1)))
(define SciLisp-Types
  '("int8" "int16" "int32" "int64" "uint8" "uint16" "uint32" "uint64" "error"
    "real32" "real64" "bigint" "bigfloat" "char" "string" "array" "stream"
    "list" "fun" "symbol" "macro" "type" "keyword" "hashtable" "spec" "regex"
    "nil" "dpair" "lenv" "env" "obarray" "funargs" "true" "false" "uninterned"
    "cons"))
(define SciLisp-aliases
  '(("lisp_consp" "\"cons?\"" 1)))
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
(defun defType-format (type)
  (format "DEFTYPE(%s,%s);\n" type (concat "_" type)))
(defun makeType-format (type)
  (format "MAKE_TYPE(%s,%s);\n" type (concat "_" type)))
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
  (format "INIT_GLOBAL(%s);\n" global ))
(defmacro prim-val (keysym)
  `(cdr (assq ,keysym prim)))
(defun primc-format (prim)
  (format
   "DEFUN(\"%s\",%s,%d,%d,%d,%d,%d);\n"
   (prim-val :lname) (prim-val :cname) (prim-val :minargs) (prim-val :optargs)
   (prim-val :keyargs) (prim-val :restarg) (prim-val :maxargs)))
(defun primh-format (prim)
  (format "DEFUN(%s,%s);\n"
          (cdr (assq :cname prim))
          (cdr(assq :maxargs prim))))
(defun initPrimsObarray-format (prim)
  (format "INIT_SYMBOL(%s);\n"
          (cdr (assq :cname prim))))
(defun makePrimSymbols-format(prim)
  (format "MAKE_SYMBOL(\"%s\",%s,%s);\n"
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
;; Local Variables:
;; auto-async-byte-compile-display-function: (lambda (&rest pargs) nil)
;; End:
(let 
    ((auto-mode-alist nil)
     (vc-handled-backends nil))
  (generate-SciLisp-prims))

