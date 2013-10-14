(define-derived-mode SciLisp-mode lisp-mode "SciLisp"
  (setq lisp-mode-hook (lambda nil nil)))
(defalias 'scilisp-mode 'SciLisp-mode)
