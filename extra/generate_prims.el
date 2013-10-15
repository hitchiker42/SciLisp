(defvar SciLisp-prims
  '(((:lname ."car") (:cname ."cdr") (:minargs . 2) (:maxargs . 2))
    ((:lname . "+")(:cname ."lisp_add")(:minargs . 2)(:maxargs . 2))
    ((:lname . "-")(:cname ."lisp_sub")(:minargs . 2)(:maxargs . 2))
    ((:lname . "*")(:cname ."lisp_mul")(:minargs . 2)(:maxargs . 2))
    ((:lname . "/")(:cname ."lisp_div")(:minargs . 2)(:maxargs . 2))
    ((:lname . "logxor")(:cname ."lisp_xor")(:minargs . 2)(:maxargs . 2))
    ((:lname . "logand")(:cname ."lisp_logand")(:minargs . 2)(:maxargs . 2))
    ((:lname . "logor")(:cname ."lisp_logor")(:minargs . 2)(:maxargs . 2))
    ((:lname . "car")(:cname ."car")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cdr")(:cname ."cdr")(:minargs . 1)(:maxargs . 1))
    ((:lname . "caar")(:cname ."caar")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cadr")(:cname ."cadr")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cddr")(:cname ."cddr")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cdar")(:cname ."cdar")(:minargs . 1)(:maxargs . 1))
    ((:lname . "caaar")(:cname ."caaar")(:minargs . 1)(:maxargs . 1))
    ((:lname . "caadr")(:cname ."caadr")(:minargs . 1)(:maxargs . 1))
    ((:lname . "caddr")(:cname ."caddr")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cdddr")(:cname ."cdddr")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cddar")(:cname ."cddar")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cdaar")(:cname ."cdaar")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cadar")(:cname ."cadar")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cdadr")(:cname ."cdadr")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cons")(:cname ."Cons")(:minargs . 2)(:maxargs . 2))
    ((:lname . "set-car!")(:cname ."set_car")(:minargs . 2)(:maxargs . 2))
    ((:lname . "set-cdr!")(:cname ."set_cdr")(:minargs . 2)(:maxargs . 2))
    ((:lname . "mapcar")(:cname ."mapcar")(:minargs . 2)(:maxargs . 2))
    ((:lname . "typeName")(:cname ."lisp_typeName")(:minargs . 1)(:maxargs . 1))
    ((:lname . "print")(:cname ."lisp_print")(:minargs . 1)(:maxargs . 1))
    ((:lname . "reduce")(:cname ."reduce")(:minargs . 2)(:maxargs . 2))
    ((:lname . "<")(:cname ."lisp_lt")(:minargs . 2)(:maxargs . 2))
    ((:lname . ">")(:cname ."lisp_gt")(:minargs . 2)(:maxargs . 2))
    ((:lname . ">=")(:cname ."lisp_gte")(:minargs . 2)(:maxargs . 2))
    ((:lname . "<=")(:cname ."lisp_lte")(:minargs . 2)(:maxargs . 2))
    ((:lname . "!=")(:cname ."lisp_ne")(:minargs . 2)(:maxargs . 2))
    ((:lname . "=")(:cname ."lisp_equals")(:minargs . 2)(:maxargs . 2))
    ((:lname . "expt")(:cname ."lisp_pow")(:minargs . 2)(:maxargs . 2))
    ((:lname . "sqrt")(:cname ."lisp_sqrt")(:minargs . 1)(:maxargs . 1))
    ((:lname . "cos")(:cname ."lisp_cos")(:minargs . 1)(:maxargs . 1))
    ((:lname . "sin")(:cname ."lisp_sin")(:minargs . 1)(:maxargs . 1))
    ((:lname . "tan")(:cname ."lisp_tan")(:minargs . 1)(:maxargs . 1))
    ((:lname . "exp")(:cname ."lisp_exp")(:minargs . 1)(:maxargs . 1))
    ((:lname . "log")(:cname ."lisp_log")(:minargs . 1)(:maxargs . 1))
    ((:lname . "abs")(:cname ."lisp_abs")(:minargs . 1)(:maxargs . 1))
    ((:lname . "ash")(:cname ."ash")(:minargs . 2)(:maxargs . 2))
    ((:lname . "mod")(:cname ."lisp_mod")(:minargs . 2)(:maxargs . 2))
    ((:lname . "drand")(:cname ."lisp_randfloat")(:minargs . 0)(:maxargs . 1))
    ((:lname . "lrand")(:cname ."lisp_randint")(:minargs . 0)(:maxargs . 0))
    ((:lname . "iota")(:cname ."lisp_iota")(:minargs . 1)(:maxargs . 4))
    ((:lname . "aref")(:cname ."aref")(:minargs . 2)(:maxargs . 2))
    ((:lname . "array->list")(:cname ."array_to_list")(:minargs . 1)(:maxargs . 1))
    ((:lname . "eval")(:cname ."lisp_eval")(:minargs . 1)(:maxargs . 1))
    ((:lname . "length")(:cname ."lisp_length")(:minargs . 1)(:maxargs . 1))
    ((:lname . "round")(:cname ."lisp_round")(:minargs . 1)(:maxargs . 2))))
;idea, have files with constant contend then generate
;code for primitives, create a new file, copy the text
;from the constant file into the new file then add
;the code for the primitives(could also have a suffix file)
(defvar initPrims-header
"#define initPrims()                                                     \\
globalSymbolTable=(global_env){.enclosing=NULL,.head=NULL};           \\
topLevelEnv=(env){.tag = 1,.enclosing=NULL,.head={.global = globalSymbolTable.head}}; \\")
(defvar initPrims-suffix 
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
srand48(time(NULL));
#undef DEFUN
#endif")
(defvar llvm-header
"static name_args_pair lisp_prims[]={\n")
(defun primc-format (prim)
  (format "DEFUN(\"%s\",%s,%d,%d);\n"
          (cdr (assq :lname prim)) (cdr (assq :cname prim))
          (cdr (assq :minargs prim)) (cdr (assq :maxargs prim))))
(defun primh-format (prim)
  (format "DEFUN(%s,%d);\n"
          (cdr (assq :cname prim))(cdr (assq :maxargs prim))))
(defun llvmh-format (prim)
  (format "{\"%scall\",%d}, "
          (cdr (assq :cname prim))(cdr (assq :maxargs prim))))
(defun initPrims-format (prim)
  (format "DEFUN_INTERN(\"%s\",%s);\\\n"
          (cdr (assq :lname prim))(cdr (assq :cname prim))))
(defun generate-SciLisp-prims()
  (let ((primh (generate-new-buffer "primh"))
        (initPrims (generate-new-buffer "initPrims"))
        (primc (generate-new-buffer "primc"))
        (llvmh (generate-new-buffer "llvmh")))
    (princ initPrims-header initPrims)
    (princ llvm-header llvmh)
    (dolist (prim SciLisp-prims)
      (princ (primc-format prim) primc)
      (princ (primh-format prim) primh)
      (princ (initPrims-format prim) initPrims)
      (princ (llvmh-format prim) llvmh))
    (princ initPrims-suffix initPrims)
    ;next should be some thing like this
    ;let primh.txt be the prim.h header(same for the other 3)
    (with-current-buffer primh
      (goto-char (point-min))
      (insert-file-contents "primh_header.h")
      (write-file "prim_temp.h")
      (kill-buffer))
    (with-current-buffer initPrims
      (append-to-file (point-min) (point-max) "prim_temp.h")
      (kill-buffer))
    (with-current-buffer primc
      (goto-char (point-min))
      (insert-file-contents "primc_header.c")
      (goto-char (point-max))
      (write-file "prim_temp.c")
      (kill-buffer))
    (with-current-buffer llvmh
      (goto-char (point-max))
      (delete-backward-char 2)
      (write-char ?} llvmh)
      (fill-region (point-min)(point-max))
      ;;cut last comma and add closing brace
      (goto-char (point-min))
      (insert-file-contents "llvmh_header.c")
      (write-file "llvm_temp.h")
      (kill-buffer))))
