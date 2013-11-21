;;utitily functions
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
    ("mapcar" "mapcar" 2) ("reduce" "reduce" 2)("qsort" "qsort_cons" 2)
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
    ("nth" "lisp_nth" 2)))
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
    ((:lname . "aref") (:cname ."aref") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "array->list") (:cname ."array_to_list") (:minargs . 1)
     (:maxargs . 1) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname "array-iota") (:cname . "array_iota") (:minargs . 1)
     (:maxargs . 4) (:optargs . 3) (:keyargs . 0) (:restarg . 0))
     ;meta information/ read eval print / system ctl
    ((:lname . "typeName") (:cname ."lisp_typeName") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "typeOf") (:cname . "typeOf") (:minargs . 1) (:maxargs . 1)
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
     ((:lname . "eq") (:cname . "lisp_eq") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
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
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "drand") (:cname ."lisp_randfloat") (:minargs . 0) (:maxargs . 1)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "lrand") (:cname ."lisp_randint") (:minargs . 0) (:maxargs . 0)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint") (:cname . "lisp_bigint") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat") (:cname . "lisp_bigfloat") (:minargs . 1)
      (:maxargs . 3) (:optargs . 2) (:keyargs . 0) (:restarg . 0))
     ((:lname . "apply") (:cname . "lisp_apply") (:minargs . 2) (:maxargs . 3)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0)))
  (mpfr-binops mpfr-binops-list)
  (collect #'mk-predicate predicates)
  (collect #'arith-driver-funs arith-driver-funs-list)
  (collect  (lambda (x) (apply #'mkPrimBasic x)) *cadrs*)))
(require 'cl)
(delete-duplicates SciLisp-prims :test #'equal)
(define SciLisp-constants
  '(("lisp_mach_eps" . "Meps") ("lisp_pi" . "pi") ("lisp_euler" ."e") 
    ("lisp_max_long" . "max_long") ("lisp_NIL" . "nil") ("lisp_LISP_TRUE" . "t")
    ("lisp_LISP_FALSE" . "#f")
    ;("stdin" . "lisp_stdin") ("stdout" . "lisp_stdout") ("stderr" . "lisp_stderr")
    ("lisp_double_0" . "double-0") ("lisp_double_1" . "double-1")
    ("lisp_long_0" ."long-0") ("lisp_long_1" . "long-1")))
;    "lisp_bigfloat_0" "lisp_bigfloat_1" "lisp_bigint_0" "lisp_bigint_1"))
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
(defun makeConsts-format (const)
  (format
   "MAKE_CONSTANT(\"%s\",%s);\n" (cdr const) (car const)))
(defun initConsts-format (const)
  (format "INIT_CONST(%s);\n" (car const)))
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
          (shell-command-to-string (format "../fnv_hash '%s'" (cdr (assq :cname prim))))))
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
    (dolist (const SciLisp-constants)
      (princ (makeConsts-format const) primSyms)
      (princ (initConsts-format const) initPrimsObarray))
    (princ initPrimsObarray-suffix initPrimsObarray)
    (princ primc-suffix primc)
    (with-current-buffer primh
      (goto-char (point-min))
      (insert-file-contents "primh_header.h")
      (goto-char (point-max))
      (insert "#undef DEFUN\n#endif")
      (write-file (expand-file-name "../prim.h"))
      (kill-buffer))
    (with-current-buffer primc
      (goto-char (point-min))
      (insert-file-contents "primc_header.c")
      (goto-char (point-max))
      (insert-buffer-substring primSyms)
      (kill-buffer primSyms)
      (insert-buffer-substring initPrimsObarray)
      (kill-buffer initPrimsObarray)
      (write-file (expand-file-name "../prim.c"))
      (kill-buffer))
    (progn (kill-buffer primc)(kill-buffer primh)(kill-buffer initPrimsObarray))))
;; Local Variables:
;; auto-async-byte-compile-display-function: (lambda (&rest args) nil)
;; End:
 
(generate-SciLisp-prims)
