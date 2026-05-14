;;; init-puni.el --- puni package                    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun puni-jump-out-pair-and-newline ()
  "Puni jump out pair and newline."
  (interactive)
  (puni-mark-sexp-at-point)
  (forward-char 1)
  (newline-and-indent))

(defun puni-wrap-single-quote (&optional n)
  "Wrap the following S-expression with single quote brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "'" "'"))

(defun puni-wrap-double-quote (&optional n)
  "Wrap the following S-expression with double quote brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "\"" "\""))

(with-eval-after-load 'meow
  (require 'meow-lisp-state)
  (meow-normal-define-key
   '("(" . puni-wrap-round)
   '(")" . puni-splice)
   '("{" . puni-wrap-curly)
   '("\"" . puni-wrap-double-quote)))

(keymap-binds puni-mode-map
  ("M-(" . puni-wrap-round)
  ("s-(" . puni-wrap-round)
  ("s-[" . puni-wrap-square)
  ("M-[" . puni-wrap-square)
  ("M-{" . puni-wrap-curly)
  ("s-{" . puni-wrap-curly)
  ("s-<" . puni-wrap-angle)
  ("M-\"" . puni-wrap-double-quote)
  ("s-\"" . puni-wrap-double-quote)
  ("M-'" . puni-wrap-single-quote)
  ("s-'" . puni-wrap-single-quote)
  ("M-)" . puni-splice)
  ("s-)" . puni-splice)
  ("C-j" . puni-jump-out-pair-and-newline)
  ("C-<backword>" . puni-backward-kill-word)
  ("C-s-f" . puni-forward-sexp)
  ("C-s-b" . puni-backward-sexp)
  ("C-s-a" . puni-beginning-of-sexp)
  ("C-s-e" . puni-end-of-sexp)
  ("X" . meow-lisp-mode))

(add-hooks '(prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook yaml-ts-mode-hook)
           #'puni-mode)

(provide 'init-puni)
;;; init-puni.el ends here
