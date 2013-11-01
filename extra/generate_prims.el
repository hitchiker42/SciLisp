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
(define *cadrs*
(let ((ls ()))
  (dolist (i '("a" "d"))
    (dolist (j '("a" "d" ""))
      (dolist (k '("a" "d" ""))
        (dolist (l '("a" "d" ""))
          (add-to-list
           'ls (list (concat "c" i j k l "r") (concat "c" i j k l "r") 1))))))
  ls))
(defun mkPrimBasic (lname cname numargs)
  `((:lname . ,lname) (:cname . ,cname) (:minargs . ,numargs)
    (:maxargs . ,numargs) (:optargs . 0) (:keyargs . 0) (:restarg . 0)))
(defun mpz-binops (op-list)
  (collect (lambda (op)
`((:lname . ,(format "bigint-%s" op))
  (:cname . ,(format "lisp_bigint_%s" op)) (:minargs . 2) (:maxargs . 2)
  (:optargs . 0) (:keyargs . 0) (:restarg . 0))) op-list))
(define mpz-binops-list (append '("add" "sub" "mul" "mod" "cdiv_q" "fdiv_q" "tdiv_q")
                                '("cdiv_r""fdiv_r" "tdiv_r" "and" "ior" "xor")
                                '("gt" "eq" "lt" "ge" "le" "ne")))
(defun mpfr-binops (op-list)
  (collect (lambda (op)
`((:lname . ,(format "bigfloat-%s" op))
  (:cname . ,(format "lisp_bigfloat_%s" op)) (:minargs . 2) (:maxargs . 2)
  (:optargs . 0) (:keyargs . 0) (:restarg . 0))) op-list))
(define mpfr-binops-list '("add" "sub" "mul" "div" "pow"
                           "gt" "eq" "lt" "ge" "le" "ne"))
(defun mpfr-unops (op-list)
  (collect (lambda (op)
`((:lname . ,(format "bigfloat-%s" op))
  (:cname . ,(format "lisp_bigfloat_%s" op)) (:minargs . 1) (:maxargs . 1)
  (:optargs . 0) (:keyargs . 0) (:restarg . 0))) op-list))
(define mpfr-unops-list '("log" "exp" "cos" "sin" "tan"))
(define basic-prims-list
  '(("+" "lisp_add" 2) ("-" "lisp_sub" 2) ("*" "lisp_mul" 2) ("/" "lisp_div" 2)
    ("<" "lisp_lt" 2) (">" "lisp_gt" 2) (">=" "lisp_gte" 2) ("<=" "lisp_lte" 2)
    ("!=" "lisp_ne" 2) ("=" "lisp_numeq" 2) ("++" "lisp_inc" 1) ("--" "lisp_dec" 1)
    ("cons" "Cons" 2) ("set-car!" "set_car" 2) ("set-cdr!" "set_cdr" 2)
    ("last" "last" 1) ("push!" "push_cons" 2) ("pop!" "pop_cons" 1)
    ("mapcar" "mapcar" 2) ("reduce" "reduce" 2)("qsort!" "qsort_cons" 2)
    ("length" "lisp_length" 1) ("aref" "aref" 2) ("array->list" "array_to_list" 1)
    ("typName" "lisp_typeName" 1) ("print" "lisp_print" 1)
    ("fclose" "lisp_close" 1) ("fputs" "lisp_fputs" 2) ("fprint" "lisp_fprint" 2)
    ("fprintln" "lisp_fprintln" 2) ("pwd" "lisp_getcwd" 0) ("eq" "lisp_eq" 2)
    ("logxor" "lisp_xor" 2) ("logand" "lisp_logand" 2) ("logor" "lisp_logor" 2)
    ("ash" "ash" 2) ("expt" "lisp_pow" 2) ("sqrt" "lisp_sqrt" 1)
    ("cos" "lisp_cos" 1) ("sin" "lisp_sin" 1) ("tan" "lisp_tan" 1)
    ("exp" "lisp_exp" 1) ("log" "lisp_log" 1) ("min" "lisp_min" 2)
    ("max" "lisp_max" 2) ("mod" "lisp_mod" 2) ("abs" "lisp_abs" 1)
    ("arrayp" "lisp_arrayp" 1)))
(defun mk-predicate (name)
  `((:lname . ,name) (:cname . ,(format "lisp_%s" name)) (:minargs . 1)
    (:maxargs . 1) (:optargs . 0) (:keyargs . 0) (:restarg . 0)))
(define predicates '("arrayp" "consp" "numberp" "nilp" "symbolp" "bigintp" "bigfloatp" "stringp" "bignump" "errorp" "functionp" "streamp"))
(define basic-SciLisp-prims
  (collect (lambda (x) (apply #'mkPrimBasic x)) basic-prims-list))
;  (dolist (f basic-prims-list)
;    (let ((ls ()))
;      (push (apply #'mkPrimBasic f) ls)
;      ls)))
(define SciLisp-prims (nconc
  '(
;:minargs->:reqargs,add :optargs and :restarg
;
    ;arithmatic functions
    ((:lname . "+") (:cname ."lisp_add")   (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "-") (:cname ."lisp_sub")   (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "*") (:cname ."lisp_mul")   (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "/") (:cname ."lisp_div")   (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "<") (:cname ."lisp_lt")    (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . ">") (:cname ."lisp_gt")    (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . ">=") (:cname ."lisp_gte")   (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "<=") (:cname ."lisp_lte")   (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "!=") (:cname ."lisp_ne")    (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "=") (:cname ."lisp_numeq") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "++") (:cname . "lisp_inc")  (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "--") (:cname . "lisp_dec")  (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "sum") (:cname . "lisp_sum") (:minargs . 1) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 1))
     ;functions on conses
    ((:lname . "cons") (:cname ."Cons") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "set-car!") (:cname ."set_car") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "set-cdr!") (:cname ."set_cdr") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "last") (:cname . "last") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "push!") (:cname . "push_cons") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "pop!") (:cname . "pop_cons") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "mapcar") (:cname ."mapcar") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "reduce") (:cname ."reduce") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "qsort!") (:cname . "qsort_cons") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "length") (:cname ."lisp_length") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "iota") (:cname ."lisp_iota") (:minargs . 1) (:maxargs . 5)
     (:optargs . 4) (:keyargs . 0) (:restarg . 0))
                                        ;array functions
    ((:lname . "aref") (:cname ."aref") (:minargs . 2) (:maxargs . 2)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "array->list") (:cname ."array_to_list") (:minargs . 1)
     (:maxargs . 1) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ;meta information/ read eval print / system ctl
    ((:lname . "typeName") (:cname ."lisp_typeName") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "typeOf") (:cname . "typeOf") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "print") (:cname ."lisp_print") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "println") (:cname ."lisp_println") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "eval") (:cname ."lisp_eval") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
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
     ((:lname . "expt") (:cname ."lisp_pow") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "sqrt") (:cname ."lisp_sqrt") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "cos") (:cname ."lisp_cos") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "sin") (:cname ."lisp_sin") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "tan") (:cname ."lisp_tan") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "exp") (:cname ."lisp_exp") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "log") (:cname ."lisp_log") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "abs") (:cname ."lisp_abs") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "mod") (:cname ."lisp_mod") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "min") (:cname . "lisp_min") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "max") (:cname . "lisp_max") (:minargs . 2) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "round") (:cname ."lisp_round") (:minargs . 1) (:maxargs . 2)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "drand") (:cname ."lisp_randfloat") (:minargs . 0) (:maxargs . 1)
      (:optargs . 1) (:keyargs . 0) (:restarg . 0))
     ((:lname . "lrand") (:cname ."lisp_randint") (:minargs . 0) (:maxargs . 0)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint") (:cname . "lisp_bigint") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat") (:cname . "lisp_bigfloat") (:minargs . 1)
      (:maxargs . 3) (:optargs . 2) (:keyargs . 0) (:restarg . 0)))
  (mpz-binops mpz-binops-list)
  (mpfr-binops mpfr-binops-list)
  (collect #'mk-predicate predicates)
  (collect  (lambda (x) (apply #'mkPrimBasic x)) *cadrs*)))
;idea, have files with constant contend then generate
;code for primitives, create a new file, copy the text
;from the constant file into the new file then add
;the code for the primitives(could also have a suffix file)
(defvar initPrimsObarray-header)
(setq initPrimsObarray-header
"mpz_t *lisp_mpz_1,*lisp_mpz_0;
mpfr_t *lisp_mpfr_1,*lisp_mpfr_0;
static void initPrimsObarray(obarray *ob,env* ob_env){

")
(defvar initPrimsObarray-suffix)
(setq initPrimsObarray-suffix
"}
")
(defvar initPrims-header)
(setq initPrims-header
"#define initPrims()                                                     \\
if(initPrimsFlag){                                                    \\
initPrimsFlag=0;                                                      \\
globalSymbolTable=(global_env){.enclosing=NULL,.head=NULL};           \\
topLevelEnv=(env){.tag = 1,.enclosing=NULL,.head={.global = globalSymbolTable.head}}; \\
keywordSymbols=(global_env){.enclosing=NULL,.head=NULL};\\
mpfr_set_default_prec(256);\\
mp_set_memory_functions(GC_MALLOC_1,GC_REALLOC_3,GC_FREE_2);\\
")
(defvar initPrims-suffix)
(setq initPrims-suffix
"INTERN_ALIAS(\"cons?\",lisp_consp,17);                                  \\
INTERN_ALIAS(\"array?\",lisp_arrayp,23);                                \\
DEFCONST(\"Meps\",lisp_mach_eps);                                       \\
DEFCONST(\"pi\",lisp_pi);                                               \\
DEFCONST(\"e\",lisp_euler);                                             \\
DEFCONST(\"nil\",NIL);                                                  \\
DEFCONST(\"t\",LISP_TRUE);                                              \\
DEFCONST(\"#f\",LISP_FALSE);                                            \\
DEFCONST(\"MAX_LONG\",lisp_max_long);                                   \\
DEFCONST(\"$$\",LispEmptyList);                                         \\
DEFCONST(\"stderr\",lisp_stderr);                                       \\
DEFCONST(\"stdout\",lisp_stdout);                                       \\
DEFCONST(\"stdin\",lisp_stdin);                                         \\
srand48(time(NULL));}
#undef DEFUN
#endif
")
(defvar primc-suffix "#undef DEFUN
")
(defmacro prim-val (keysym)
  `(cdr (assq ,keysym prim)))
;(defvar llvm-header
;"static name_args_pair lisp_prims[]={\n")
(defun primc-format (prim)
  (format
   "DEFUN(\"%s\",%s,%d,%d,%d,%d,%d);\n"
   (prim-val :lname) (prim-val :cname) (prim-val :minargs) (prim-val :optargs)
   (prim-val :keyargs) (prim-val :restarg) (prim-val :maxargs)))
(defun primc-normal-format (prim)
  (format
   "DEFUN(\"%s\",%s,%d,%d);\n"
   (prim-val :lname) (prim-val :cname) (prim-val :minargs) (prim-val :maxargs)))
(defun primc-many-format(prim)
  (format "DEFUN_MANY(\"%s\",%s,%d,%d)\n"
          (cdr (assq :lname prim)) (cdr (assq :cname prim))
          (cdr (assq :minargs prim)) (1+ (cdr (assq :minargs prim)))))
;(defun primc-format (prim)
;  (if (stringp (cdr (assq :maxargs prim)))
;      (primc-many-format prim)
;    (primc-normal-format prim)))
(defun primh-format (prim)
  (format "DEFUN(%s,%s);\n"
          (cdr (assq :cname prim))
          (cdr(assq :maxargs prim))))
;(defun llvmh-format (prim)
;  (format "{\"%scall\",%d}, "
;          (cdr (assq :cname prim))(cdr (assq :maxargs prim))))
(defun initPrims-format (prim)
  (format "DEFUN_INTERN(\"%s\",%s);\\\n"
          (cdr (assq :lname prim))(cdr (assq :cname prim))))
(defun initPrimsObarray-format (prim)
  (format "INIT_SYMBOL(%s);\n"
          (cdr (assq :cname prim))))
(defun makePrimSymbols-format(prim)
  (format "MAKE_SYMBOL(\"%s\",%s,%s);\n"
          (cdr (assq :lname prim))(cdr (assq :cname prim))
          (shell-command-to-string (format "../fnv_hash '%s'" (cdr (assq :cname prim))))))
(defun generate-SciLisp-prims()
  (let ((primh (generate-new-buffer "primh"))
        (initPrims (generate-new-buffer "initPrims"))
        (initPrimsObarray (generate-new-buffer "initPrimsObarray"))
        (primc (generate-new-buffer "primc"))
        (primSyms (generate-new-buffer "primSyms")))
    (princ initPrims-header initPrims)
    (princ initPrimsObarray-header initPrimsObarray)
    (dolist (prim SciLisp-prims)
      (princ (primc-format prim) primc)
      (princ (primh-format prim) primh)
      (princ (initPrims-format prim) initPrims)
      (princ (makePrimSymbols-format prim) primSyms)
      (princ (initPrimsObarray-format prim) initPrimsObarray))
    (princ initPrims-suffix initPrims)
    (princ initPrimsObarray-suffix initPrimsObarray)
    (princ primc-suffix primc)
    (with-current-buffer primh
      (goto-char (point-min))
      (insert-file-contents "primh_header.h")
      (write-file (expand-file-name "../prim.h"))
      (kill-buffer))
    (with-current-buffer initPrims
      (indent-buffer)
      (append-to-file (point-min) (point-max)
                      (expand-file-name "../prim.h"))
      (kill-buffer))
    ;; (with-current-buffer initPrimsObarray
    ;;   (indent-buffer)
    ;;   (append-to-file (point-min) (point-max)
    ;;                   (expand-file-name "../prim.h"))
    ;;   (kill-buffer))
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
    (progn (kill-buffer primc)(kill-buffer primh)(kill-buffer initPrims))))
                                        ;    (with-current-buffer llvmh
;      (goto-char (point-max))
;      (delete-backward-char 2)
;      (write-char ?} llvmh)
;      (fill-region (point-min)(point-max))
      ;;cut last comma and add closing brace
;      (goto-char (point-min))
;      (insert-file-contents "llvmh_header.c")
;      (write-file "llvm_temp.h")
;      (kill-buffer))))
