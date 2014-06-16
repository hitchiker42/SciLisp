#!/usr/bin/emacs --script
;; -*- lexical-binding: t; -*-
(load (expand-file-name "~/.emacs.d/my-elisp/misc.elc"))
(require 'cl)
(require 'tucker-misc)
(defun loop-files (fun &rest files)
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (funcall fun (current-buffer))
      (save-buffer))))
(defun loop-lines (fun)
  (save-excursion
    (goto-char (point-min))
    (until (eq (point) (point-max))
           (funcall fun)
           (forward-line))))
(defun libvirt-indent-statements (&optional start end)
  (goto-char start)
  (while (re-search-forward "\\(if\\|for\\|while\\|switch\\) ?(\\(.*\\)) ?{" end t)
    (replace-match "\\1 (\\2) {" t)))
(defsubst indent-commas-line ()
  (while (re-search-forward ",\\([^\n ]\\)" (line-end-position) t)
    (replace-match ", \\1" t)))
(defsubst indent-equals-line ()
  (while (re-search-forward
          "\\([a-zA-Z0-9_)]\\) ?\\([-+*/^&|><=!]?[<>]?=\\) ?\\([{([a-zA-Z0-9_-\"']\\)"
          (line-end-position) t)
    (replace-match "\\1 \\2 \\3")))
(defsubst indent-commas ()
  (while (re-search-forward ",\\([^\n ]\\)" nil t)
    (replace-match ", \\1" t)))
(defsubst indent-equals ()
  (while (re-search-forward
          "\\([a-zA-Z0-9_)]\\) ?\\([-+*/^&|><=!]?[<>]?=\\) ?\\([{([a-zA-Z0-9_-\"']\\)"
          nil t)
    (replace-match "\\1 \\2 \\3")))
(defun indent-by-line ()
  (unless (looking-at-p "^ *[\"/*]")
    (indent-commas-line)
    (beginning-of-line)
    (indent-equals-line)))
(eval-when-load
 (let
     ((args command-line-args-left))
   (if (null args)
       (princ (concat "Usage: indent_script.el filenames\n"
                      "\tIndent C code in filenames\n")))
   (message "%s" args)
   (apply #'loop-files
          (lambda (&optional x)
            (loop-lines #'indent-by-line)) args)))
