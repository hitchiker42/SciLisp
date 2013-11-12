;This is at the moment just a copy of lisp.el from emacs with some of
;the emacs specific stuff trimed out, but I do intend to make this more
;specific to scilisp(also it'd be nice to allow interactive evaluation and
;all that)
(define-derived-mode SciLisp-mode lisp-mode "SciLisp"
  (setq lisp-mode-hook (lambda nil nil)))
(defalias 'scilisp-mode 'SciLisp-mode)
;;; SciLisp-mode.el --- Lisp mode, and its idiosyncratic commands  -*- coding: utf-8 -*-

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The base major mode for editing Lisp code (used also for Emacs Lisp).
;; This mode is documented in the Emacs manual.

;;; Code:
(defvar lisp-mode-abbrev-table nil)
(define-abbrev-table 'lisp-mode-abbrev-table ()
  "Abbrev table for Lisp mode.")

(defvar SciLisp-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\[ "_   " table)
    (modify-syntax-entry ?\] "_   " table)
    (modify-syntax-entry ?# "' 14" table)
    (modify-syntax-entry ?| "\" 23bn" table)
    table)
  "Syntax table used in `lisp-mode'.")

(defvar lisp-imenu-generic-expression
  (list
   (list nil
	 (purecopy (concat "^\\s-*("
			   (eval-when-compile
			     (regexp-opt
			      '("defun" "defun*" "defsubst" "defmacro"
				"defadvice" "define-skeleton"
				"define-minor-mode" "define-global-minor-mode"
				"define-globalized-minor-mode"
				"define-derived-mode" "define-generic-mode"
				"define-compiler-macro" "define-modify-macro"
				"defsetf" "define-setf-expander"
				"define-method-combination"
				"defgeneric" "defmethod"
				"cl-defun" "cl-defsubst" "cl-defmacro"
				"cl-define-compiler-macro") t))
			   "\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"))
	 2)
   (list (purecopy "Variables")
	 (purecopy (concat "^\\s-*("
			   (eval-when-compile
			     (regexp-opt
			      '("defconst" "defconstant" "defcustom"
				"defparameter" "define-symbol-macro") t))
			   "\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"))
	 2)
   ;; For `defvar', we ignore (defvar FOO) constructs.
   (list (purecopy "Variables")
	 (purecopy (concat "^\\s-*(defvar\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"
			   "[[:space:]\n]+[^)]"))
	 1)
   (list (purecopy "Types")
	 (purecopy (concat "^\\s-*("
			   (eval-when-compile
			     (regexp-opt
			      '("defgroup" "deftheme" "deftype" "defstruct"
				"defclass" "define-condition" "define-widget"
				"defface" "defpackage" "cl-deftype"
				"cl-defstruct") t))
			   "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
	 2))

  "Imenu generic expression for Lisp mode.  See `imenu-generic-expression'.")

;; This was originally in autoload.el and is still used there.
(put 'autoload 'doc-string-elt 3)
(put 'defmethod 'doc-string-elt 3)
(put 'defvar   'doc-string-elt 3)
(put 'defconst 'doc-string-elt 3)
(put 'defalias 'doc-string-elt 3)
(put 'defvaralias 'doc-string-elt 3)
(put 'define-category 'doc-string-elt 2)

(defvar lisp-doc-string-elt-property 'doc-string-elt
  "The symbol property that holds the docstring position info.")

(defun lisp-font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let* ((listbeg (nth 1 state))
                 (firstsym (and listbeg
                                (save-excursion
                                  (goto-char listbeg)
                                  (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                       (match-string 1)))))
                 (docelt (and firstsym
                              (function-get (intern-soft firstsym)
                                            lisp-doc-string-elt-property))))
            (if (and docelt
                     ;; It's a string in a form that can have a docstring.
                     ;; Check whether it's in docstring position.
                     (save-excursion
                       (when (functionp docelt)
                         (goto-char (match-end 1))
                         (setq docelt (funcall docelt)))
                       (goto-char listbeg)
                       (forward-char 1)
                       (condition-case nil
                           (while (and (> docelt 0) (< (point) startpos)
                                       (progn (forward-sexp 1) t))
                             (setq docelt (1- docelt)))
                         (error nil))
                       (and (zerop docelt) (<= (point) startpos)
                            (progn (forward-comment (point-max)) t)
                            (= (point) (nth 8 state)))))
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

(defun lisp-mode-variables (&optional lisp-syntax keywords-case-insensitive)
  "Common initialization routine for lisp modes.
The LISP-SYNTAX argument is used by code in inf-lisp.el and is
\(uselessly) passed from pp.el, chistory.el, gnus-kill.el and
score-mode.el.  KEYWORDS-CASE-INSENSITIVE non-nil means that for
font-lock keywords will not be case sensitive."
  (when lisp-syntax
    (set-syntax-table lisp-mode-syntax-table))
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets the fill wrong for a one-line paragraph made of
  ;; a single docstring.  Let's fix it here.
  (setq-local adaptive-fill-function
	      (lambda () (if (looking-at "\\s-+\"[^\n\"]+\"\\s-*$") "")))
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  ;;  I believe that newcomment's auto-fill code properly deals with it  -stef
  ;;(set (make-local-variable 'adaptive-fill-mode) nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|(")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq-local comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  ;; Font lock mode uses this only when it KNOWS a comment is starting.
  (setq-local font-lock-comment-start-skip ";+ *")
  (setq-local comment-add 1)		;default to `;;' in comment-region
  (setq-local comment-column 40)
  ;; Don't get confused by `;' in doc strings when paragraph-filling.
  (setq-local comment-use-global-state t)
  (setq-local imenu-generic-expression lisp-imenu-generic-expression)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local syntax-begin-function 'beginning-of-defun)
  (setq font-lock-defaults
	`((lisp-font-lock-keywords
	   lisp-font-lock-keywords-1
           lisp-font-lock-keywords-2)
	  nil ,keywords-case-insensitive nil nil
	  (font-lock-mark-block-function . mark-defun)
	  (font-lock-syntactic-face-function
	   . lisp-font-lock-syntactic-face-function)))
  (setq-local prettify-symbols-alist lisp--prettify-symbols-alist))

(defun lisp-outline-level ()
  "Lisp mode `outline-level' function."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (if (looking-at "(\\|;;;###autoload")
	1000
      len)))

(defun lisp-current-defun-name ()
  "Return the name of the defun at point, or nil."
  (save-excursion
    (let ((location (point)))
      ;; If we are now precisely at the beginning of a defun, make sure
      ;; beginning-of-defun finds that one rather than the previous one.
      (or (eobp) (forward-char 1))
      (beginning-of-defun)
      ;; Make sure we are really inside the defun found, not after it.
      (when (and (looking-at "\\s(")
		 (progn (end-of-defun)
			(< location (point)))
		 (progn (forward-sexp -1)
			(>= location (point))))
	(if (looking-at "\\s(")
	    (forward-char 1))
	;; Skip the defining construct name, typically "defun" or
	;; "defvar".
	(forward-sexp 1)
	;; The second element is usually a symbol being defined.  If it
	;; is not, use the first symbol in it.
	(skip-chars-forward " \t\n'(")
	(buffer-substring-no-properties (point)
					(progn (forward-sexp 1)
					       (point)))))))

(defvar lisp-mode-shared-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; This gets in the way when viewing a Lisp file in view-mode.  As
    ;; long as [backspace] is mapped into DEL via the
    ;; function-key-map, this should remain disabled!!
    ;;;(define-key map [backspace] 'backward-delete-char-untabify)
    map)
  "Keymap for commands shared by all sorts of Lisp modes.")

(defcustom lisp-mode-hook nil
  "Hook run when entering Lisp mode."
  :options '(imenu-add-menubar-index)
  :type 'hook
  :group 'lisp)

(defcustom lisp-interaction-mode-hook nil
  "Hook run when entering Lisp Interaction mode."
  :options '(turn-on-eldoc-mode)
  :type 'hook
  :group 'lisp)

(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
;;; Generic Lisp mode.

(defvar lisp-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Lisp")))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\C-x" 'lisp-eval-defun)
    (define-key map "\C-c\C-z" 'run-lisp)
    (bindings--define-key map [menu-bar lisp] (cons "Lisp" menu-map))
    (bindings--define-key menu-map [run-lisp]
      '(menu-item "Run inferior Lisp" run-lisp
		  :help "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'"))
    (bindings--define-key menu-map [ev-def]
      '(menu-item "Eval defun" lisp-eval-defun
		  :help "Send the current defun to the Lisp process made by M-x run-lisp"))
    (bindings--define-key menu-map [ind-sexp]
      '(menu-item "Indent sexp" indent-sexp
		  :help "Indent each line of the list starting just after point"))
    map)
  "Keymap for ordinary Lisp mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(define-derived-mode lisp-mode prog-mode "Lisp"
  "Major mode for editing Lisp code for Lisps other than GNU Emacs Lisp.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.

\\{lisp-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `lisp-mode-hook'
if that value is non-nil."
  (lisp-mode-variables nil t)
  (setq-local find-tag-default-function 'lisp-find-tag-default)
  (setq-local comment-start-skip
	      "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq imenu-case-fold-search t))

(defun lisp-find-tag-default ()
  (let ((default (find-tag-default)))
    (when (stringp default)
      (if (string-match ":+" default)
          (substring default (match-end 0))
	default))))

;; Used in old LispM code.
(defalias 'common-lisp-mode 'lisp-mode)

;; This will do unless inf-lisp.el is loaded.
(defun lisp-eval-defun (&optional and-go)
  "Send the current defun to the Lisp process made by \\[run-lisp]."
  (interactive)
  (error "Process lisp does not exist"))

(defvar lisp-interaction-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Lisp-Interaction")))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\C-x" 'eval-defun)
    (define-key map "\e\C-q" 'indent-pp-sexp)
    (define-key map "\e\t" 'completion-at-point)
    (define-key map "\n" 'eval-print-last-sexp)
    (bindings--define-key map [menu-bar lisp-interaction]
      (cons "Lisp-Interaction" menu-map))
    (bindings--define-key menu-map [eval-defun]
      '(menu-item "Evaluate Defun" eval-defun
		  :help "Evaluate the top-level form containing point, or after point"))
    (bindings--define-key menu-map [eval-print-last-sexp]
      '(menu-item "Evaluate and Print" eval-print-last-sexp
		  :help "Evaluate sexp before point; print value into current buffer"))
    (bindings--define-key menu-map [edebug-defun-lisp-interaction]
      '(menu-item "Instrument Function for Debugging" edebug-defun
		  :help "Evaluate the top level form point is in, stepping through with Edebug"
		  :keys "C-u C-M-x"))
    (bindings--define-key menu-map [indent-pp-sexp]
      '(menu-item "Indent or Pretty-Print" indent-pp-sexp
		  :help "Indent each line of the list starting just after point, or prettyprint it"))
    (bindings--define-key menu-map [complete-symbol]
      '(menu-item "Complete Lisp Symbol" completion-at-point
		  :help "Perform completion on Lisp symbol preceding point"))
    map)
  "Keymap for Lisp Interaction mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(define-derived-mode lisp-interaction-mode emacs-lisp-mode "Lisp Interaction"
  "Major mode for typing and evaluating Lisp forms.
Like Lisp mode except that \\[eval-print-last-sexp] evals the Lisp expression
before point, and prints its value into the buffer, advancing point.
Note that printing is controlled by `eval-expression-print-length'
and `eval-expression-print-level'.

Commands:
Delete converts tabs to spaces as it moves back.
Paragraphs are separated only by blank lines.
Semicolons start comments.

\\{lisp-interaction-mode-map}
Entry to this mode calls the value of `lisp-interaction-mode-hook'
if that value is non-nil."
  :abbrev-table nil)

(defun eval-print-last-sexp ()
  "Evaluate sexp before point; print value into current buffer.

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger.

Note that printing the result is controlled by the variables
`eval-expression-print-length' and `eval-expression-print-level',
which see."
  (interactive)
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp t)
    (terpri)))


(defun last-sexp-setup-props (beg end value alt1 alt2)
  "Set up text properties for the output of `eval-last-sexp-1'.
BEG and END are the start and end of the output in current-buffer.
VALUE is the Lisp value printed, ALT1 and ALT2 are strings for the
alternative printed representations that can be displayed."
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'last-sexp-toggle-display)
    (define-key map [down-mouse-2] 'mouse-set-point)
    (define-key map [mouse-2] 'last-sexp-toggle-display)
    (add-text-properties
     beg end
     `(printed-value (,value ,alt1 ,alt2)
		     mouse-face highlight
		     keymap ,map
		     help-echo "RET, mouse-2: toggle abbreviated display"
		     rear-nonsticky (mouse-face keymap help-echo
						printed-value)))))


(defun last-sexp-toggle-display (&optional arg)
  "Toggle between abbreviated and unabbreviated printed representations."
  (interactive "P")
  (save-restriction
    (widen)
    (let ((value (get-text-property (point) 'printed-value)))
      (when value
	(let ((beg (or (previous-single-property-change (min (point-max) (1+ (point)))
							'printed-value)
		       (point)))
	      (end (or (next-single-char-property-change (point) 'printed-value) (point)))
	      (standard-output (current-buffer))
	      (point (point)))
	  (delete-region beg end)
	  (insert (nth 1 value))
	  (or (= beg point)
	      (setq point (1- (point))))
	  (last-sexp-setup-props beg (point)
				 (nth 0 value)
				 (nth 2 value)
				 (nth 1 value))
	  (goto-char (min (point-max) point)))))))

(defun prin1-char (char)
  "Return a string representing CHAR as a character rather than as an integer.
If CHAR is not a character, return nil."
  (and (integerp char)
       (eventp char)
       (let ((c (event-basic-type char))
	     (mods (event-modifiers char))
	     string)
	 ;; Prevent ?A from turning into ?\S-a.
	 (if (and (memq 'shift mods)
		  (zerop (logand char ?\S-\^@))
		  (not (let ((case-fold-search nil))
			 (char-equal c (upcase c)))))
	     (setq c (upcase c) mods nil))
	 ;; What string are we considering using?
	 (condition-case nil
	     (setq string
		   (concat
		    "?"
		    (mapconcat
		     (lambda (modif)
		       (cond ((eq modif 'super) "\\s-")
			     (t (string ?\\ (upcase (aref (symbol-name modif) 0)) ?-))))
		     mods "")
		    (cond
		     ((memq c '(?\; ?\( ?\) ?\{ ?\} ?\[ ?\] ?\" ?\' ?\\)) (string ?\\ c))
		     ((eq c 127) "\\C-?")
		     (t
		      (string c)))))
	   (error nil))
	 ;; Verify the string reads a CHAR, not to some other character.
	 ;; If it doesn't, return nil instead.
	 (and string
	      (= (car (read-from-string string)) char)
	      string))))


(defun preceding-sexp ()
  "Return sexp before the point."
  (let ((opoint (point))
	ignore-quotes
	expr)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
	;; If this sexp appears to be enclosed in `...'
	;; then ignore the surrounding quotes.
	(setq ignore-quotes
	      (or (eq (following-char) ?\')
		  (eq (preceding-char) ?\')))
	(forward-sexp -1)
	;; If we were after `?\e' (or similar case),
	;; use the whole thing, not just the `e'.
	(when (eq (preceding-char) ?\\)
	  (forward-char -1)
	  (when (eq (preceding-char) ??)
	    (forward-char -1)))

	;; Skip over hash table read syntax.
	(and (> (point) (1+ (point-min)))
	     (looking-back "#s" (- (point) 2))
	     (forward-char -2))

	;; Skip over `#N='s.
	(when (eq (preceding-char) ?=)
	  (let (labeled-p)
	    (save-excursion
	      (skip-chars-backward "0-9#=")
	      (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
	    (when labeled-p
	      (forward-sexp -1))))

	(save-restriction
	  ;; vladimir@cs.ualberta.ca 30-Jul-1997: skip ` in
	  ;; `variable' so that the value is returned, not the
	  ;; name
	  (if (and ignore-quotes
		   (eq (following-char) ?`))
	      (forward-char))
	  (narrow-to-region (point-min) opoint)
	  (setq expr (read (current-buffer)))
	  ;; If it's an (interactive ...) form, it's more
	  ;; useful to show how an interactive call would
	  ;; use it.
	  (and (consp expr)
	       (eq (car expr) 'interactive)
	       (setq expr
		     (list 'call-interactively
			   (list 'quote
				 (list 'lambda
				       '(&rest args)
				       expr
				       'args)))))
	  expr)))))


(defun eval-last-sexp-1 (eval-last-sexp-arg-internal)
  "Evaluate sexp before point; print value in the echo area.
With argument, print output into current buffer."
  (let ((standard-output (if eval-last-sexp-arg-internal (current-buffer) t)))
    ;; Setup the lexical environment if lexical-binding is enabled.
    (eval-last-sexp-print-value
     (eval (eval-sexp-add-defvars (preceding-sexp)) lexical-binding))))


(defun eval-last-sexp-print-value (value)
  (let ((unabbreviated (let ((print-length nil) (print-level nil))
			 (prin1-to-string value)))
	(print-length eval-expression-print-length)
	(print-level eval-expression-print-level)
	(beg (point))
	end)
    (prog1
	(prin1 value)
      (let ((str (eval-expression-print-format value)))
	(if str (princ str)))
      (setq end (point))
      (when (and (bufferp standard-output)
		 (or (not (null print-length))
		     (not (null print-level)))
		 (not (string= unabbreviated
			       (buffer-substring-no-properties beg end))))
	(last-sexp-setup-props beg end value
			       unabbreviated
			       (buffer-substring-no-properties beg end))
	))))


(defvar eval-last-sexp-fake-value (make-symbol "t"))

(defun eval-sexp-add-defvars (exp &optional pos)
  "Prepend EXP with all the `defvar's that precede it in the buffer.
POS specifies the starting position where EXP was found and defaults to point."
  (setq exp (macroexpand-all exp))      ;Eager macro-expansion.
  (if (not lexical-binding)
      exp
    (save-excursion
      (unless pos (setq pos (point)))
      (let ((vars ()))
        (goto-char (point-min))
        (while (re-search-forward
                "(def\\(?:var\\|const\\|custom\\)[ \t\n]+\\([^; '()\n\t]+\\)"
                pos t)
          (let ((var (intern (match-string 1))))
            (and (not (special-variable-p var))
                 (save-excursion
                   (zerop (car (syntax-ppss (match-beginning 0)))))
              (push var vars))))
        `(progn ,@(mapcar (lambda (v) `(defvar ,v)) vars) ,exp)))))

(defun eval-last-sexp (eval-last-sexp-arg-internal)
  "Evaluate sexp before point; print value in the echo area.
Interactively, with prefix argument, print output into current buffer.
Truncates long output according to the value of the variables
`eval-expression-print-length' and `eval-expression-print-level'.

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger."
  (interactive "P")
  (if (null eval-expression-debug-on-error)
      (eval-last-sexp-1 eval-last-sexp-arg-internal)
    (let ((value
	   (let ((debug-on-error eval-last-sexp-fake-value))
	     (cons (eval-last-sexp-1 eval-last-sexp-arg-internal)
		   debug-on-error))))
      (unless (eq (cdr value) eval-last-sexp-fake-value)
	(setq debug-on-error (cdr value)))
      (car value))))

(defun eval-defun-1 (form)
  "Treat some expressions specially.
Reset the `defvar' and `defcustom' variables to the initial value.
\(For `defcustom', use the :set function if there is one.)
Reinitialize the face according to the `defface' specification."
  ;; The code in edebug-defun should be consistent with this, but not
  ;; the same, since this gets a macroexpanded form.
  (cond ((not (listp form))
	 form)
	((and (eq (car form) 'defvar)
	      (cdr-safe (cdr-safe form))
	      (boundp (cadr form)))
	 ;; Force variable to be re-set.
	 `(progn (defvar ,(nth 1 form) nil ,@(nthcdr 3 form))
		 (setq-default ,(nth 1 form) ,(nth 2 form))))
	;; `defcustom' is now macroexpanded to
	;; `custom-declare-variable' with a quoted value arg.
	((and (eq (car form) 'custom-declare-variable)
	      (default-boundp (eval (nth 1 form) lexical-binding)))
	 ;; Force variable to be bound, using :set function if specified.
	 (let ((setfunc (memq :set form)))
	   (when setfunc
	     (setq setfunc (car-safe (cdr-safe setfunc)))
	     (or (functionp setfunc) (setq setfunc nil)))
	   (funcall (or setfunc 'set-default)
		    (eval (nth 1 form) lexical-binding)
		    ;; The second arg is an expression that evaluates to
		    ;; an expression.  The second evaluation is the one
		    ;; normally performed not by normal execution but by
		    ;; custom-initialize-set (for example), which does not
		    ;; use lexical-binding.
		    (eval (eval (nth 2 form) lexical-binding))))
	 form)
	;; `defface' is macroexpanded to `custom-declare-face'.
	((eq (car form) 'custom-declare-face)
	 ;; Reset the face.
	 (let ((face-symbol (eval (nth 1 form) lexical-binding)))
	   (setq face-new-frame-defaults
		 (assq-delete-all face-symbol face-new-frame-defaults))
	   (put face-symbol 'face-defface-spec nil)
	   (put face-symbol 'face-override-spec nil))
	 form)
	((eq (car form) 'progn)
	 (cons 'progn (mapcar 'eval-defun-1 (cdr form))))
	(t form)))

(defun eval-defun-2 ()
  "Evaluate defun that point is in or before.
The value is displayed in the echo area.
If the current defun is actually a call to `defvar',
then reset the variable using the initial value expression
even if the variable already has some other value.
\(Normally `defvar' does not change the variable's value
if it already has a value.\)

Return the result of evaluation."
  ;; FIXME: the print-length/level bindings should only be applied while
  ;; printing, not while evaluating.
  (let ((debug-on-error eval-expression-debug-on-error)
	(print-length eval-expression-print-length)
	(print-level eval-expression-print-level))
    (save-excursion
      ;; Arrange for eval-region to "read" the (possibly) altered form.
      ;; eval-region handles recording which file defines a function or
      ;; variable.  Re-written using `apply' to avoid capturing
      ;; variables like `end'.
      (apply
       #'eval-region
       (let ((standard-output t)
	     beg end form)
	 ;; Read the form from the buffer, and record where it ends.
	 (save-excursion
	   (end-of-defun)
	   (beginning-of-defun)
	   (setq beg (point))
	   (setq form (read (current-buffer)))
	   (setq end (point)))
	 ;; Alter the form if necessary.
	 (setq form (eval-sexp-add-defvars (eval-defun-1 (macroexpand form))))
	 (list beg end standard-output
	       `(lambda (ignore)
		 ;; Skipping to the end of the specified region
		 ;; will make eval-region return.
		 (goto-char ,end)
		 ',form))))))
  ;; The result of evaluation has been put onto VALUES.  So return it.
  (car values))

(defun eval-defun (edebug-it)
  "Evaluate the top-level form containing point, or after point.

If the current defun is actually a call to `defvar' or `defcustom',
evaluating it this way resets the variable using its initial value
expression (using the defcustom's :set function if there is one), even
if the variable already has some other value.  \(Normally `defvar' and
`defcustom' do not alter the value if there already is one.)  In an
analogous way, evaluating a `defface' overrides any customizations of
the face, so that it becomes defined exactly as the `defface' expression
says.

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger.

With a prefix argument, instrument the code for Edebug.

If acting on a `defun' for FUNCTION, and the function was
instrumented, `Edebug: FUNCTION' is printed in the echo area.  If not
instrumented, just FUNCTION is printed.

If not acting on a `defun', the result of evaluation is displayed in
the echo area.  This display is controlled by the variables
`eval-expression-print-length' and `eval-expression-print-level',
which see."
  (interactive "P")
  (cond (edebug-it
	 (require 'edebug)
	 (eval-defun (not edebug-all-defs)))
	(t
	 (if (null eval-expression-debug-on-error)
	     (eval-defun-2)
	   (let ((old-value (make-symbol "t")) new-value value)
	     (let ((debug-on-error old-value))
	       (setq value (eval-defun-2))
	       (setq new-value debug-on-error))
	     (unless (eq old-value new-value)
	       (setq debug-on-error new-value))
	     value)))))

;; May still be used by some external Lisp-mode variant.
(define-obsolete-function-alias 'lisp-comment-indent
    'comment-indent-default "22.1")
(define-obsolete-function-alias 'lisp-mode-auto-fill 'do-auto-fill "23.1")

(defcustom lisp-indent-offset nil
  "If non-nil, indent second line of expressions that many more columns."
  :group 'lisp
  :type '(choice (const nil) integer))
(put 'lisp-indent-offset 'safe-local-variable
     (lambda (x) (or (null x) (integerp x))))

(defcustom lisp-indent-function 'lisp-indent-function
  "A function to be called by `calculate-lisp-indent'.
It indents the arguments of a Lisp function call.  This function
should accept two arguments: the indent-point, and the
`parse-partial-sexp' state at that position.  One option for this
function is `common-lisp-indent-function'."
  :type 'function
  :group 'lisp)

(defun lisp-indent-line (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt end
	(pos (- (point-max) (point)))
	(beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
	;; Don't alter indentation of a ;;; comment line
	;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
	(goto-char (- (point-max) pos))
      (if (and (looking-at "\\s<") (not (looking-at "\\s<\\s<")))
	  ;; Single-semicolon comment lines should be indented
	  ;; as comment lines, not as code.
	  (progn (indent-for-comment) (forward-char -1))
	(if (listp indent) (setq indent (car indent)))
	(setq shift-amt (- indent (current-column)))
	(if (zerop shift-amt)
	    nil
	  (delete-region beg (point))
	  (indent-to indent)))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defvar calculate-lisp-indent-last-sexp)

(defun calculate-lisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (if parse-start
          (goto-char parse-start)
          (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
		  state
                  (> (setq paren-depth (elt state 0)) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
		 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
					    indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
	    ;; indent-point immediately follows open paren.
	    ;; Don't call hook.
            (setq desired-indent (current-column))
	  ;; Find the start of first element of containing sexp.
	  (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
	  (cond ((looking-at "\\s(")
		 ;; First element of containing sexp is a list.
		 ;; Indent under that list.
		 )
		((> (save-excursion (forward-line 1) (point))
		    calculate-lisp-indent-last-sexp)
		 ;; This is the first line to start within the containing sexp.
		 ;; It's almost certainly a function call.
		 (if (= (point) calculate-lisp-indent-last-sexp)
		     ;; Containing sexp has nothing before this line
		     ;; except the first element.  Indent under that element.
		     nil
		   ;; Skip the first element, find start of second (the first
		   ;; argument of the function call) and indent under.
		   (progn (forward-sexp 1)
			  (parse-partial-sexp (point)
					      calculate-lisp-indent-last-sexp
					      0 t)))
		 (backward-prefix-chars))
		(t
		 ;; Indent beneath first sexp on same line as
		 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
		 ;; almost certainly a function call.
		 (goto-char calculate-lisp-indent-last-sexp)
		 (beginning-of-line)
		 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
				     0 t)
		 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
	       nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
		;; or it does not apply to this argument,
		;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
		       ;; Handle prefix characters and whitespace
		       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (and (not (looking-back "^[ \t]*\\|([ \t]+"))
                                   (or (not containing-sexp)
                                       (< (1+ containing-sexp) (point))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(defun lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
		(progn (goto-char calculate-lisp-indent-last-sexp)
		       (beginning-of-line)
		       (parse-partial-sexp (point)
					   calculate-lisp-indent-last-sexp 0 t)))
	    ;; Indent under the list or under the first sexp on the same
	    ;; line as calculate-lisp-indent-last-sexp.  Note that first
	    ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
			 (get (intern-soft function) 'lisp-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method indent-point state)))))))

(defcustom lisp-body-indent 2
  "Number of columns to indent the second line of a `(def...)' form."
  :group 'lisp
  :type 'integer)
(put 'lisp-body-indent 'safe-local-variable 'integerp)

(defun lisp-indent-specform (count state indent-point normal-indent)
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; lisp-body-indent, else normal indent.  With lisp-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 lisp-body-indent))
                  containing-form-start)
            (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun lisp-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ lisp-body-indent (current-column)))))


;; (put 'progn 'lisp-indent-function 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'autoload 'lisp-indent-function 'defun)
(put 'progn 'lisp-indent-function 0)
(put 'prog1 'lisp-indent-function 1)
(put 'prog2 'lisp-indent-function 2)
(put 'save-excursion 'lisp-indent-function 0)
(put 'save-restriction 'lisp-indent-function 0)
(put 'save-current-buffer 'lisp-indent-function 0)
(put 'let 'lisp-indent-function 1)
(put 'let* 'lisp-indent-function 1)
(put 'while 'lisp-indent-function 1)
(put 'if 'lisp-indent-function 2)
(put 'catch 'lisp-indent-function 1)
(put 'condition-case 'lisp-indent-function 2)
(put 'unwind-protect 'lisp-indent-function 1)
(put 'with-output-to-temp-buffer 'lisp-indent-function 1)

(defun indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (let ((indent-stack (list nil))
	(next-depth 0)
	;; If ENDPOS is non-nil, use nil as STARTING-POINT
	;; so that calculate-lisp-indent will find the beginning of
	;; the defun we are in.
	;; If ENDPOS is nil, it is safe not to scan before point
	;; since every line we indent is more deeply nested than point is.
	(starting-point (if endpos nil (point)))
	(last-point (point))
	last-depth bol outer-loop-done inner-loop-done state this-indent)
    (or endpos
	;; Get error now if we don't have a complete sexp after point.
	(save-excursion (forward-sexp 1)))
    (save-excursion
      (setq outer-loop-done nil)
      (while (if endpos (< (point) endpos)
	       (not outer-loop-done))
	(setq last-depth next-depth
	      inner-loop-done nil)
	;; Parse this line so we can learn the state
	;; to indent the next line.
	;; This inner loop goes through only once
	;; unless a line ends inside a string.
	(while (and (not inner-loop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  ;; If the line contains a comment other than the sort
	  ;; that is indented like code,
	  ;; indent it now with indent-for-comment.
	  ;; Comments indented like code are right already.
	  ;; In any case clear the in-comment flag in the state
	  ;; because parse-partial-sexp never sees the newlines.
	  (if (car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setcar (nthcdr 4 state) nil)))
	  ;; If this line ends inside a string,
	  ;; go straight to next line, remaining within the inner loop,
	  ;; and turn off the \-flag.
	  (if (car (nthcdr 3 state))
	      (progn
		(forward-line 1)
		(setcar (nthcdr 5 state) nil))
	    (setq inner-loop-done t)))
	(and endpos
	     (<= next-depth 0)
	     (progn
	       (setq indent-stack (nconc indent-stack
					 (make-list (- next-depth) nil))
		     last-depth (- last-depth next-depth)
		     next-depth 0)))
	(forward-line 1)
	;; Decide whether to exit.
	(if endpos
	    ;; If we have already reached the specified end,
	    ;; give up and do not reindent this line.
	    (if (<= endpos (point))
		(setq outer-loop-done t))
	  ;; If no specified end, we are done if we have finished one sexp.
	  (if (<= next-depth 0)
	      (setq outer-loop-done t)))
	(unless outer-loop-done
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  ;; Now indent the next line according
	  ;; to what we learned from parsing the previous one.
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  ;; But not if the line is blank, or just a comment
	  ;; (except for double-semi comments; indent them as usual).
	  (if (or (eobp) (looking-at "\\s<\\|\n"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (calculate-lisp-indent
			  (if (car indent-stack) (- (car indent-stack))
			    starting-point))))
		(if (null val)
		    (setq this-indent val)
		  (if (integerp val)
		      (setcar indent-stack
			      (setq this-indent val))
		    (setcar indent-stack (- (car (cdr val))))
		    (setq this-indent (car val))))))
	    (if (and this-indent (/= (current-column) this-indent))
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))
	(or outer-loop-done
	    (setq outer-loop-done (= (point) last-point))
	    (setq last-point (point)))))))

(defun indent-pp-sexp (&optional arg)
  "Indent each line of the list starting just after point, or prettyprint it.
A prefix argument specifies pretty-printing."
  (interactive "P")
  (if arg
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (progn (forward-sexp 1) (point)))
          (pp-buffer)
          (goto-char (point-max))
          (if (eq (char-before) ?\n)
              (delete-char -1)))))
  (indent-sexp))

;;;; Lisp paragraph filling commands.

(defcustom emacs-lisp-docstring-fill-column 65
  "Value of `fill-column' to use when filling a docstring.
Any non-integer value means do not use a different value of
`fill-column' when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current `fill-column'" t))
  :group 'lisp)
(put 'emacs-lisp-docstring-fill-column 'safe-local-variable
     (lambda (x) (or (eq x t) (integerp x))))

(defun lisp-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Emacs Lisp comments and docstrings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial semicolons."
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; Since fill-comment-paragraph returned nil, that means we're not in
      ;; a comment: Point is on a program line; we are interested
      ;; particularly in docstring lines.
      ;;
      ;; We bind `paragraph-start' and `paragraph-separate' temporarily.  They
      ;; are buffer-local, but we avoid changing them so that they can be set
      ;; to make `forward-paragraph' and friends do something the user wants.
      ;;
      ;; `paragraph-start': The `(' in the character alternative and the
      ;; left-singlequote plus `(' sequence after the \\| alternative prevent
      ;; sexps and backquoted sexps that follow a docstring from being filled
      ;; with the docstring.  This setting has the consequence of inhibiting
      ;; filling many program lines that are not docstrings, which is sensible,
      ;; because the user probably asked to fill program lines by accident, or
      ;; expecting indentation (perhaps we should try to do indenting in that
      ;; case).  The `;' and `:' stop the paragraph being filled at following
      ;; comment lines and at keywords (e.g., in `defcustom').  Left parens are
      ;; escaped to keep font-locking, filling, & paren matching in the source
      ;; file happy.
      ;;
      ;; `paragraph-separate': A clever regexp distinguishes the first line of
      ;; a docstring and identifies it as a paragraph separator, so that it
      ;; won't be filled.  (Since the first line of documentation stands alone
      ;; in some contexts, filling should not alter the contents the author has
      ;; chosen.)  Only the first line of a docstring begins with whitespace
      ;; and a quotation mark and ends with a period or (rarely) a comma.
      ;;
      ;; The `fill-column' is temporarily bound to
      ;; `emacs-lisp-docstring-fill-column' if that value is an integer.
      (let ((paragraph-start (concat paragraph-start
				     "\\|\\s-*\\([(;:\"]\\|`(\\|#'(\\)"))
	    (paragraph-separate
	     (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (if (and (integerp emacs-lisp-docstring-fill-column)
                                  (derived-mode-p 'emacs-lisp-mode))
                             emacs-lisp-docstring-fill-column
                           fill-column)))
	(fill-paragraph justify))
      ;; Never return nil.
      t))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them.

Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))

(provide 'lisp-mode)
;;mode stolen(not literally) emacs code
;;; lisp-mode.el ends here
;;; inf-lisp.el --- an inferior-lisp mode

;; Copyright (C) 1988, 1993-1994, 2001-2013 Free Software Foundation,
;; Inc.

;; Author: Olin Shivers <shivers@cs.cmu.edu>
;; Keywords: processes, lisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Hacked from tea.el by Olin Shivers (shivers@cs.cmu.edu). 8/88

;; This file defines a lisp-in-a-buffer package (inferior-lisp mode)
;; built on top of comint mode.  This version is more featureful,
;; robust, and uniform than the Emacs 18 version.  The key bindings are
;; also more compatible with the bindings of Hemlock and Zwei (the
;; Lisp Machine emacs).

;; Since this mode is built on top of the general command-interpreter-in-
;; a-buffer mode (comint mode), it shares a common base functionality,
;; and a common set of bindings, with all modes derived from comint mode.
;; This makes these modes easier to use.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customizing it, see the file comint.el.
;; For further information on inferior-lisp mode, see the comments below.

;; Needs fixin:
;; The load-file/compile-file default mechanism could be smarter -- it
;; doesn't know about the relationship between filename extensions and
;; whether the file is source or executable. If you compile foo.lisp
;; with compile-file, then the next load-file should use foo.bin for
;; the default, not foo.lisp. This is tricky to do right, particularly
;; because the extension for executable files varies so much (.o, .bin,
;; .lbin, .mo, .vo, .ao, ...).
;;
;; It would be nice if inferior-lisp (and inferior scheme, T, ...) modes
;; had a verbose minor mode wherein sending or compiling defuns, etc.
;; would be reflected in the transcript with suitable comments, e.g.
;; ";;; redefining fact". Several ways to do this. Which is right?
;;
;; When sending text from a source file to a subprocess, the process-mark can
;; move off the window, so you can lose sight of the process interactions.
;; Maybe I should ensure the process mark is in the window when I send
;; text to the process? Switch selectable?

;;; Code:

(require 'comint)
(require 'lisp-mode)
(defcustom inferior-lisp-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior Lisp's input history.
Input matching this regexp is not saved on the input history in Inferior Lisp
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inferior-lisp)

(defvar inferior-lisp-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\C-x\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-l" 'lisp-load-file)
    (define-key map "\C-c\C-k" 'lisp-compile-file)
    (define-key map "\C-c\C-a" 'lisp-show-arglist)
    (define-key map "\C-c\C-d" 'lisp-describe-sym)
    (define-key map "\C-c\C-f" 'lisp-show-function-documentation)
    (define-key map "\C-c\C-v" 'lisp-show-variable-documentation)
    map))

;;; These commands augment Lisp mode, so you can process Lisp code in
;;; the source files.
(define-key lisp-mode-map "\M-\C-x"  'lisp-eval-defun)     ; Gnu convention
(define-key lisp-mode-map "\C-x\C-e" 'lisp-eval-last-sexp) ; Gnu convention
(define-key lisp-mode-map "\C-c\C-e" 'lisp-eval-defun)
(define-key lisp-mode-map "\C-c\C-r" 'lisp-eval-region)
(define-key lisp-mode-map "\C-c\C-c" 'lisp-compile-defun)
(define-key lisp-mode-map "\C-c\C-z" 'switch-to-lisp)
(define-key lisp-mode-map "\C-c\C-l" 'lisp-load-file)
(define-key lisp-mode-map "\C-c\C-k" 'lisp-compile-file)  ; "kompile" file
(define-key lisp-mode-map "\C-c\C-a" 'lisp-show-arglist)
(define-key lisp-mode-map "\C-c\C-d" 'lisp-describe-sym)
(define-key lisp-mode-map "\C-c\C-f" 'lisp-show-function-documentation)
(define-key lisp-mode-map "\C-c\C-v" 'lisp-show-variable-documentation)


;;; This function exists for backwards compatibility.
;;; Previous versions of this package bound commands to C-c <letter>
;;; bindings, which is not allowed by the gnumacs standard.

;;;  "This function binds many inferior-lisp commands to C-c <letter> bindings,
;;;where they are more accessible. C-c <letter> bindings are reserved for the
;;;user, so these bindings are non-standard. If you want them, you should
;;;have this function called by the inferior-lisp-load-hook:
;;;  (add-hook 'inferior-lisp-load-hook 'inferior-lisp-install-letter-bindings)
;;;You can modify this function to install just the bindings you want."
(defun inferior-lisp-install-letter-bindings ()
  (define-key lisp-mode-map "\C-ce" 'lisp-eval-defun-and-go)
  (define-key lisp-mode-map "\C-cr" 'lisp-eval-region-and-go)
  (define-key lisp-mode-map "\C-cc" 'lisp-compile-defun-and-go)
  (define-key lisp-mode-map "\C-cz" 'switch-to-lisp)
  (define-key lisp-mode-map "\C-cl" 'lisp-load-file)
  (define-key lisp-mode-map "\C-ck" 'lisp-compile-file)
  (define-key lisp-mode-map "\C-ca" 'lisp-show-arglist)
  (define-key lisp-mode-map "\C-cd" 'lisp-describe-sym)
  (define-key lisp-mode-map "\C-cf" 'lisp-show-function-documentation)
  (define-key lisp-mode-map "\C-cv" 'lisp-show-variable-documentation)

  (define-key inferior-lisp-mode-map "\C-cl" 'lisp-load-file)
  (define-key inferior-lisp-mode-map "\C-ck" 'lisp-compile-file)
  (define-key inferior-lisp-mode-map "\C-ca" 'lisp-show-arglist)
  (define-key inferior-lisp-mode-map "\C-cd" 'lisp-describe-sym)
  (define-key inferior-lisp-mode-map "\C-cf" 'lisp-show-function-documentation)
  (define-key inferior-lisp-mode-map "\C-cv"
    'lisp-show-variable-documentation))

(defcustom inferior-lisp-program "lisp"
  "Program name for invoking an inferior Lisp in Inferior Lisp mode."
  :type 'string
  :group 'inferior-lisp)

(defcustom inferior-lisp-load-command "(load \"%s\")\n"
  "Format-string for building a Lisp expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Lisp expression that will command the inferior Lisp
to load that file.  The default works acceptably on most Lisps.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\n\"
produces cosmetically superior output for this application,
but it works only in Common Lisp."
  :type 'string
  :group 'inferior-lisp)

(defcustom inferior-lisp-prompt "^[^> \n]*>+:? *"
  "Regexp to recognize prompts in the Inferior Lisp mode.
Defaults to \"^[^> \\n]*>+:? *\", which works pretty good for Lucid, kcl,
and franz.  This variable is used to initialize `comint-prompt-regexp' in the
Inferior Lisp buffer.

This variable is only used if the variable
`comint-use-prompt-regexp' is non-nil.

More precise choices:
Lucid Common Lisp: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
kcl: \"^>+ *\""
  :type 'regexp
  :group 'inferior-lisp)

(defvar inferior-lisp-buffer nil "*The current inferior-lisp process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Lisp processes, you start the first up
with \\[inferior-lisp].  It will be in a buffer named `*inferior-lisp*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inferior-lisp].  It will be in a new buffer,
named `*inferior-lisp*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Lisp processes --
like `lisp-eval-defun' or `lisp-show-arglist' -- have to choose a process
to send to, when you have more than one Lisp process around.  This
is determined by the global variable `inferior-lisp-buffer'.  Suppose you
have three inferior Lisps running:
    Buffer              Process
    foo                 inferior-lisp
    bar                 inferior-lisp<2>
    *inferior-lisp*     inferior-lisp<3>
If you do a \\[lisp-eval-defun] command on some Lisp source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inferior-lisp*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inferior-lisp-buffer'.
This process selection is performed by function `inferior-lisp-proc'.

Whenever \\[inferior-lisp] fires up a new process, it resets
`inferior-lisp-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you might need to change `inferior-lisp-buffer' to
whichever process buffer you want to use.")

(defvar inferior-lisp-mode-hook '()
  "Hook for customizing Inferior Lisp mode.")

(put 'inferior-lisp-mode 'mode-class 'special)

(define-derived-mode inferior-lisp-mode comint-mode "Inferior Lisp"
  "Major mode for interacting with an inferior Lisp process.
Runs a Lisp interpreter as a subprocess of Emacs, with Lisp I/O through an
Emacs buffer.  Variable `inferior-lisp-program' controls which Lisp interpreter
is run.  Variables `inferior-lisp-prompt', `inferior-lisp-filter-regexp' and
`inferior-lisp-load-command' can customize this mode for different Lisp
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-lisp-buffer'.

\\{inferior-lisp-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-lisp-mode-hook' (in that order).

You can send text to the inferior Lisp process from other buffers containing
Lisp source.
    `switch-to-lisp' switches the current buffer to the Lisp process buffer.
    `lisp-eval-defun' sends the current defun to the Lisp process.
    `lisp-compile-defun' compiles the current defun.
    `lisp-eval-region' sends the current region to the Lisp process.
    `lisp-compile-region' compiles the current region.

    Prefixing the lisp-eval/compile-defun/region commands with
    a \\[universal-argument] causes a switch to the Lisp process buffer after sending
    the text.

Commands:\\<inferior-lisp-mode-map>
\\[comint-send-input] after the end of the process' output sends the text from the
    end of process to point.
\\[comint-send-input] before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
\\[comint-copy-old-input] copies the sexp ending at point to the end of the process' output,
    allowing you to edit it before sending it.
If `comint-use-prompt-regexp' is nil (the default), \\[comint-insert-input] on old input
   copies the entire old input to the end of the process' output, allowing
   you to edit it before sending it.  When not used on old input, or if
   `comint-use-prompt-regexp' is non-nil, \\[comint-insert-input] behaves according to
   its global binding.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
\\[lisp-indent-line] indents for Lisp; with argument, shifts rest
    of expression rigidly with the current line.
\\[indent-sexp] does \\[lisp-indent-line] on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (setq comint-prompt-regexp inferior-lisp-prompt)
  (setq mode-line-process '(":%s"))
  (lisp-mode-variables t)
  (setq comint-get-old-input (function lisp-get-old-input))
  (setq comint-input-filter (function lisp-input-filter)))

(defun lisp-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun lisp-input-filter (str)
  "t if STR does not match `inferior-lisp-filter-regexp'."
  (not (string-match inferior-lisp-filter-regexp str)))

;;;###autoload
(defun inferior-lisp (cmd)
  "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run lisp: " inferior-lisp-program)
		       inferior-lisp-program)))
  (if (not (comint-check-proc "*inferior-lisp*"))
      (let ((cmdlist (split-string cmd)))
	(set-buffer (apply (function make-comint)
			   "inferior-lisp" (car cmdlist) nil (cdr cmdlist)))
	(inferior-lisp-mode)))
  (setq inferior-lisp-buffer "*inferior-lisp*")
  (pop-to-buffer-same-window "*inferior-lisp*"))

;;;###autoload
(defalias 'run-lisp 'inferior-lisp)

(defun lisp-eval-region (start end &optional and-go)
  "Send the current region to the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inferior-lisp-proc) start end)
  (comint-send-string (inferior-lisp-proc) "\n")
  (if and-go (switch-to-lisp t)))

(defun lisp-compile-string (string)
  "Send the string to the inferior Lisp process to be compiled and executed."
  (comint-send-string
   (inferior-lisp-proc)
   (format "(funcall (compile nil (lambda () %s)))\n" string)))

(defun lisp-eval-string (string)
  "Send the string to the inferior Lisp process to be executed."
  (comint-send-string (inferior-lisp-proc) (concat string "\n")))

(defun lisp-do-defun (do-string do-region)
  "Send the current defun to the inferior Lisp process.
The actually processing is done by `do-string' and `do-region'
 which determine whether the code is compiled before evaluation.
DEFVAR forms reset the variables to the init values."
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (if (looking-at "(defvar")
          (funcall do-string
                   ;; replace `defvar' with `defparameter'
                   (concat "(defparameter "
                           (buffer-substring-no-properties (+ (point) 7) end)
                           "\n"))
        (funcall do-region (point) end)))))

(defun lisp-eval-defun (&optional and-go)
  "Send the current defun to the inferior Lisp process.
DEFVAR forms reset the variables to the init values.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-do-defun 'lisp-eval-string 'lisp-eval-region)
  (if and-go (switch-to-lisp t)))

(defun lisp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun lisp-compile-region (start end &optional and-go)
  "Compile the current region in the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "r\nP")
  (lisp-compile-string (buffer-substring-no-properties start end))
  (if and-go (switch-to-lisp t)))

(defun lisp-compile-defun (&optional and-go)
  "Compile the current defun in the inferior Lisp process.
DEFVAR forms reset the variables to the init values.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-do-defun 'lisp-compile-string 'lisp-compile-region)
  (if and-go (switch-to-lisp t)))

(defun switch-to-lisp (eob-p)
  "Switch to the inferior Lisp process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-lisp-buffer)
      (let ((pop-up-frames
	     ;; Be willing to use another frame
	     ;; that already has the window in it.
	     (or pop-up-frames
		 (get-buffer-window inferior-lisp-buffer t))))
	(pop-to-buffer inferior-lisp-buffer))
      (run-lisp inferior-lisp-program))
  (when eob-p
	 (push-mark)
    (goto-char (point-max))))


;;; Now that lisp-compile/eval-defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun lisp-eval-region-and-go (start end)
  "Send the current region to the inferior Lisp, and switch to its buffer."
  (interactive "r")
  (lisp-eval-region start end t))

(defun lisp-eval-defun-and-go ()
  "Send the current defun to the inferior Lisp, and switch to its buffer."
  (interactive)
  (lisp-eval-defun t))

(defun lisp-compile-region-and-go (start end)
  "Compile the current region in the inferior Lisp, and switch to its buffer."
  (interactive "r")
  (lisp-compile-region start end t))

(defun lisp-compile-defun-and-go ()
  "Compile the current defun in the inferior Lisp, and switch to its buffer."
  (interactive)
  (lisp-compile-defun t))

;;; A version of the form in H. Shevis' soar-mode.el package. Less robust.
;;; (defun lisp-compile-sexp (start end)
;;;   "Compile the s-expression bounded by START and END in the inferior lisp.
;;; If the sexp isn't a DEFUN form, it is evaluated instead."
;;;   (cond ((looking-at "(defun\\s +")
;;; 	 (goto-char (match-end 0))
;;; 	 (let ((name-start (point)))
;;; 	   (forward-sexp 1)
;;; 	   (process-send-string "inferior-lisp"
;;; 				(format "(compile '%s #'(lambda "
;;; 					(buffer-substring name-start
;;; 							  (point)))))
;;; 	 (let ((body-start (point)))
;;; 	   (goto-char start) (forward-sexp 1) ; Can't use end-of-defun.
;;; 	   (process-send-region "inferior-lisp"
;;; 				(buffer-substring body-start (point))))
;;; 	 (process-send-string "inferior-lisp" ")\n"))
;;; 	(t (lisp-eval-region start end)))))
;;;
;;; (defun lisp-compile-region (start end)
;;;   "Each s-expression in the current region is compiled (if a DEFUN)
;;; or evaluated (if not) in the inferior lisp."
;;;   (interactive "r")
;;;   (save-excursion
;;;     (goto-char start) (end-of-defun) (beginning-of-defun) ; error check
;;;     (if (< (point) start) (error "region begins in middle of defun"))
;;;     (goto-char start)
;;;     (let ((s start))
;;;       (end-of-defun)
;;;       (while (<= (point) end) ; Zip through
;;; 	(lisp-compile-sexp s (point)) ; compiling up defun-sized chunks.
;;; 	(setq s (point))
;;; 	(end-of-defun))
;;;       (if (< s end) (lisp-compile-sexp s end)))))
;;;
;;; End of HS-style code


(defvar lisp-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `lisp-load-file' or `lisp-compile-file' command.")

(defcustom lisp-source-modes '(lisp-mode)
  "Used to determine if a buffer contains Lisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Lisp source file by `lisp-load-file' and `lisp-compile-file'.
Used by these commands to determine defaults."
  :type '(repeat symbol)
  :group 'inferior-lisp)

(defun lisp-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil)) ; nil because LOAD
					; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc)
		      (format inferior-lisp-load-command file-name))
  (switch-to-lisp t))


(defun lisp-compile-file (file-name)
  "Compile a Lisp file in the inferior Lisp process."
  (interactive (comint-get-source "Compile Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil)) ; nil = don't need
					; suffix .lisp
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc) (concat "(compile-file \""
						   file-name
						   "\"\)\n"))
  (switch-to-lisp t))



;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command strings
;;; ===============

(defvar lisp-function-doc-command
  "(let ((fn '%s))
     (format t \"Documentation for ~a:~&~a\"
	     fn (documentation fn 'function))
     (values))\n"
  "Command to query inferior Lisp for a function's documentation.")

(defvar lisp-var-doc-command
  "(let ((v '%s))
     (format t \"Documentation for ~a:~&~a\"
	     v (documentation v 'variable))
     (values))\n"
  "Command to query inferior Lisp for a variable's documentation.")

(defvar lisp-arglist-command
  "(let ((fn '%s))
     (format t \"Arglist for ~a: ~a\" fn (arglist fn))
     (values))\n"
  "Command to query inferior Lisp for a function's arglist.")

(defvar lisp-describe-sym-command
  "(describe '%s)\n"
  "Command to query inferior Lisp for a variable's documentation.")


;;; Ancillary functions
;;; ===================

;;; Reads a string from the user.
(defun lisp-symprompt (prompt default)
  (list (let* ((prompt (if default
			   (format "%s (default %s): " prompt default)
			 (concat prompt ": ")))
	       (ans (read-string prompt)))
	  (if (zerop (length ans)) default ans))))


;;; Adapted from function-called-at-point in help.el.
(defun lisp-fn-called-at-pt ()
  "Returns the name of the function called in the current call.
The value is nil if it can't find one."
  (condition-case nil
      (save-excursion
	(save-restriction
	  (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	  (backward-up-list 1)
	  (forward-char 1)
	  (let ((obj (read (current-buffer))))
	    (and (symbolp obj) obj))))
    (error nil)))


;;; Adapted from variable-at-point in help.el.
(defun lisp-var-at-pt ()
  (condition-case ()
      (save-excursion
	(forward-sexp -1)
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) obj)))
    (error nil)))


;;; Documentation functions: fn and var doc, arglist, and symbol describe.
;;; ======================================================================

(defun lisp-show-function-documentation (fn)
  "Send a command to the inferior Lisp to give documentation for function FN.
See variable `lisp-function-doc-command'."
  (interactive (lisp-symprompt "Function doc" (lisp-fn-called-at-pt)))
  (comint-proc-query (inferior-lisp-proc)
		     (format lisp-function-doc-command fn)))

(defun lisp-show-variable-documentation (var)
  "Send a command to the inferior Lisp to give documentation for function FN.
See variable `lisp-var-doc-command'."
  (interactive (lisp-symprompt "Variable doc" (lisp-var-at-pt)))
  (comint-proc-query (inferior-lisp-proc) (format lisp-var-doc-command var)))

(defun lisp-show-arglist (fn)
  "Send a query to the inferior Lisp for the arglist for function FN.
See variable `lisp-arglist-command'."
  (interactive (lisp-symprompt "Arglist" (lisp-fn-called-at-pt)))
  (comint-proc-query (inferior-lisp-proc) (format lisp-arglist-command fn)))

(defun lisp-describe-sym (sym)
  "Send a command to the inferior Lisp to describe symbol SYM.
See variable `lisp-describe-sym-command'."
  (interactive (lisp-symprompt "Describe" (lisp-var-at-pt)))
  (comint-proc-query (inferior-lisp-proc)
		     (format lisp-describe-sym-command sym)))


;;  "Returns the current inferior Lisp process.
;; See variable `inferior-lisp-buffer'."
(defun inferior-lisp-proc ()
  (let ((proc (get-buffer-process (if (derived-mode-p 'inferior-lisp-mode)
				      (current-buffer)
				    inferior-lisp-buffer))))
    (or proc
	(error "No Lisp subprocess; see variable `inferior-lisp-buffer'"))))


;;; Do the user's customization...
;;;===============================
(defvar inferior-lisp-load-hook nil
  "This hook is run when the library `inf-lisp' is loaded.")

(run-hooks 'inferior-lisp-load-hook)

;;; CHANGE LOG
;;; ===========================================================================
;;; 7/21/92 Jim Blandy
;;; - Changed all uses of the cmulisp name or prefix to inferior-lisp;
;;;   this is now the official inferior lisp package.  Use the global
;;;   ChangeLog from now on.
;;; 5/24/90 Olin
;;; - Split cmulisp and cmushell modes into separate files.
;;;   Not only is this a good idea, it's apparently the way it'll be rel 19.
;;; - Upgraded process sends to use comint-send-string instead of
;;;   process-send-string.
;;; - Explicit references to process "cmulisp" have been replaced with
;;;   (cmulisp-proc). This allows better handling of multiple process bufs.
;;; - Added process query and var/function/symbol documentation
;;;   commands. Based on code written by Douglas Roberts.
;;; - Added lisp-eval-last-sexp, bound to C-x C-e.
;;;
;;; 9/20/90 Olin
;;; Added a save-restriction to lisp-fn-called-at-pt. This bug and fix
;;; reported by Lennart Staflin.
;;;
;;; 3/12/90 Olin
;;; - lisp-load-file and lisp-compile-file no longer switch-to-lisp.
;;;   Tale suggested this.
;;; - Reversed this decision 7/15/91. You need the visual feedback.
;;;
;;; 7/25/91 Olin
;;; Changed all keybindings of the form C-c <letter>. These are
;;; supposed to be reserved for the user to bind. This affected
;;; mainly the compile/eval-defun/region[-and-go] commands.
;;; This was painful, but necessary to adhere to the gnumacs standard.
;;; For some backwards compatibility, see the
;;;     cmulisp-install-letter-bindings
;;; function.
;;;
;;; 8/2/91 Olin
;;; - The lisp-compile/eval-defun/region commands now take a prefix arg,
;;;   which means switch-to-lisp after sending the text to the Lisp process.
;;;   This obsoletes all the -and-go commands. The -and-go commands are
;;;   kept around for historical reasons, and because the user can bind
;;;   them to key sequences shorter than C-u C-c C-<letter>.
;;; - If M-x cmulisp is invoked with a prefix arg, it allows you to
;;;   edit the command line.

(provide 'inf-lisp)

;;; inf-lisp.el ends here
