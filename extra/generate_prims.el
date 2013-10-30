(defun indent-buffer ()
  "Indent entire buffer using indent-region"
  (interactive)
  (indent-region (point-min) (point-max)))
(defvar *cadrs*
(let ((ls ()))
  (dolist (i '("a" "d"))
    (dolist (j '("a" "d" ""))
      (dolist (k '("a" "d" ""))
        (dolist (l '("a" "d" ""))
          (add-to-list 'ls (concat "c" i j k l "r"))))))
  ls))
(defun mkPrimBasic (lname cname numargs)
  (insert (format (concat
"((:lname . \"%s\") (:cname . \"%s\") (:minargs . %d) (:maxargs . %d)"
"(:optargs . 0) (:keyargs . 0) (:restarg . 0))") lname cname numargs numargs)))
(defun mpz-binops(op-list)
  (dolist (op op-list)
    (insert (format (concat
"((:lname . \"bigint-%s\") (:cname . \"lisp_gmp_%s\") (:minargs . 2) \n"
"(:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))\n") op op))))
(defvar mpz-binops-list '("add" "sub" "mul" "mod" "cdiv_q" "cdiv_r" "fdiv_q"
                          "fdiv_r" "tdiv_r" "tdiv_q" "and" "ior" "xor"))
(defun mpfr-binops(op-list)
  (dolist (op op-list)
    (insert (format (concat
"((:lname . \"bigfloat-%s\")(:cname . \"lisp_mpfr_%s\") (:minargs . 2)\n" 
"(:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))\n") op op))))
(defvar mpfr-binops-list '("add" "sub" "mul" "div" "pow"))
(defun mpfr-unops(op-list)
  (dolist (op op-list)
    (insert (format (concat 
"((:lname . \"bigfloat-%s\")(:cname . \"lisp_mpfr_%s\") (:minargs . 1) "
"(:maxargs . 1) (:optargs . 0) (:keyargs . 0) (:restarg . 0))") op op))))
(defvar mpfr-unops-list '("log" "exp" "cos" "sin" "tan"))
(defvar SciLisp-prims)
(defvar basic-prims-list (append 
  '(("+" "lisp_add" 2) ("-" "lisp_sub" 2) ("*" "lisp_mul" 2) ("/" "lisp_div" 2)
    ("<" "lisp_lt" 2) (">" "lisp_gt" 2) (">=" "lisp_gte" 2) ("<=" "lisp_lte" 2)
    ("!=" "lisp_ne" 2) ("=" "lisp_numeq" 2) ("++" "lisp_inc" 1) ("--" "lisp_dec" 1)
    ("cons" "Cons" 2) ("set-car!" "set_car" 2) ("set-cdr!" "set_cdr" 2) 
    ("last" "last" 1) ("push!" "push_cons" 2) ("pop!" "pop_cons" 1)
    ("mapcar" "mapcar" 2) ("reduce" "reduce" 2))
  (mapcar (lambda (x) (list x x 1)) *cadrs*)
  ))
(defvar basic-SciLisp-prims
  (dolist (f basic-prims-list)
    (let ((ls ()))
      (add-to-list 'ls (apply #'mkPrimBasic f))
      ls)))
(setq SciLisp-prims 
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
    ((:lname . "car") (:cname ."car") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdr") (:cname ."cdr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caar") (:cname ."caar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cadr") (:cname ."cadr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cddr") (:cname ."cddr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdar") (:cname ."cdar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caaar") (:cname ."caaar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caadr") (:cname ."caadr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caddr") (:cname ."caddr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdddr") (:cname ."cdddr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cddar") (:cname ."cddar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdaar") (:cname ."cdaar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cadar") (:cname ."cadar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdadr") (:cname ."cdadr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caaaar") (:cname . "caaaar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caaadr") (:cname . "caaadr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caadar") (:cname . "caadar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caaddr") (:cname . "caaddr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cadaar") (:cname . "cadaar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cadadr") (:cname . "cadadr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "caddar") (:cname . "caddar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cadddr") (:cname . "cadddr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdaaar") (:cname . "cdaaar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdaadr") (:cname . "cdaadr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdadar") (:cname . "cdadar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdaddr") (:cname . "cdaddr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cddaar") (:cname . "cddaar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cddadr") (:cname . "cddadr") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cdddar") (:cname . "cdddar") (:minargs . 1) (:maxargs . 1)
     (:optargs . 0) (:keyargs . 0) (:restarg . 0))
    ((:lname . "cddddr") (:cname . "cddddr") (:minargs . 1) (:maxargs . 1)
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
     ((:lname . "consp") (:cname . "lisp_consp") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "numberp") (:cname . "lisp_numberp") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "arrayp") (:cname . "lisp_arrayp") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "nilp") (:cname . "lisp_nilp") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "stringp") (:cname . "lisp_stringp") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "symbolp") (:cname . "lisp_symbolp") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint") (:cname . "lisp_bigint") (:minargs . 1) (:maxargs . 1)
      (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat") (:cname . "lisp_bigfloat") (:minargs . 1)
      (:maxargs . 3) (:optargs . 2) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-add") (:cname . "lisp_gmp_add") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-sub") (:cname . "lisp_gmp_sub") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-mul") (:cname . "lisp_gmp_mul") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-mod") (:cname . "lisp_gmp_mod") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-cdiv_q") (:cname . "lisp_gmp_cdiv_q") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-cdiv_r") (:cname . "lisp_gmp_cdiv_r") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-fdiv_q") (:cname . "lisp_gmp_fdiv_q") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-fdiv_r") (:cname . "lisp_gmp_fdiv_r") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-tdiv_r") (:cname . "lisp_gmp_tdiv_r") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-tdiv_q") (:cname . "lisp_gmp_tdiv_q") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-and") (:cname . "lisp_gmp_and") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-ior") (:cname . "lisp_gmp_ior") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigint-xor") (:cname . "lisp_gmp_xor") (:minargs . 2) 
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat-add")(:cname . "lisp_mpfr_add") (:minargs . 2)
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat-sub")(:cname . "lisp_mpfr_sub") (:minargs . 2)
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat-mul")(:cname . "lisp_mpfr_mul") (:minargs . 2)
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat-div")(:cname . "lisp_mpfr_div") (:minargs . 2)
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))
     ((:lname . "bigfloat-pow")(:cname . "lisp_mpfr_pow") (:minargs . 2)
      (:maxargs . 2) (:optargs . 0) (:keyargs . 0) (:restarg . 0))))
;idea, have files with constant contend then generate
;code for primitives, create a new file, copy the text
;from the constant file into the new file then add
;the code for the primitives(could also have a suffix file)
(defvar initPrimsObarray-header)
(setq initPrimsObarray-header
"static void initPrimsObarray(obarray *ob,env* ob_env){
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
#endif")
(defvar primc-suffix "#undef DEFUN")
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
  (format "DEFUN_INTERN_OBARRAY(\"%s\",%s);\n"
          (cdr (assq :lname prim))(cdr (assq :cname prim))))
(defun makePrimSymbols-format(prim)
  (format "MAKE_SYMBOL(\"%s\",%s,%s);\n"
          (cdr (assq :lname prim))(cdr (assq :cname prim))
          (shell-command-to-string (format "../fnv_hash '%s'" 
                                           (cdr (assq :cname prim))))))
(makePrimSymbols-format (car SciLisp-prims))
(defun generate-SciLisp-prims()
  (let ((primh (generate-new-buffer "primh"))
        (initPrims (generate-new-buffer "initPrims"))
        (initPrimsObarray (generate-new-buffer "initPrimsObarray"))
        (primc (generate-new-buffer "primc")))
    (princ initPrims-header initPrims)
    (princ initPrimsObarray-header initPrimsObarray)
    (dolist (prim SciLisp-prims)
      (princ (primc-format prim) primc)
      (princ (primh-format prim) primh)
      (princ (initPrims-format prim) initPrims)
      (princ (initPrimsObarray-format prim) initPrimsObarray))
    (princ initPrims-suffix initPrims)
    (princ initPrimsObarray-suffix initPrimsObarray)
    (princ primc-suffix primc)
                                        ;next should be some thing like this
                                        ;let primh.txt be the prim.h header(same for the other 3)
    (with-current-buffer primh
      (goto-char (point-min))
      (insert-file-contents "primh_header.h")
      (write-file (expand-file-name "../prim.h"))
      (kill-buffer))
    (with-current-buffer initPrimsObarray
      (indent-buffer)
      (append-to-file (point-min) (point-max) 
                      (expand-file-name "../prim.h"))
      (kill-buffer))
    (with-current-buffer initPrims
      (indent-buffer)
      (append-to-file (point-min) (point-max) 
                      (expand-file-name "../prim.h"))
      (kill-buffer)) 
    (with-current-buffer primc
      (goto-char (point-min))
      (insert-file-contents "primc_header.c")
      (goto-char (point-max))
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
