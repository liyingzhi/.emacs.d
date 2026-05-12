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

(require 'transient)
(transient-define-prefix puni-sexp-menu ()
  "Puni sexp menu."
  :transient-non-suffix 'transient--do-stay
  ["Actions"
   ["slurp & barf"
    ("s f" "Slurp forward" puni-slurp-forward :transient t)
    ("s b" "Slurp backward" puni-slurp-backward :transient t)
    ("b f" "Barf forward" puni-barf-forward :transient t)
    ("b b" "Barf backward" puni-barf-backward :transient t)]
   ["Other"
    ("r" "Raise" puni-raise :transient t)
    ("c" "Convolute" puni-convolute :transient t)
    ("p" "Split" puni-split :transient t)
    ("t" "Transpose" puni-transpose :transient t)
    ("S" "Squeeze" puni-squeeze :transient t)]
   ["Move"
    ("F" "Forward Sexp" puni-forward-sexp :transient t)
    ("B" "Backward Sexp" puni-backward-sexp :transient t)
    ("h" "Forward Sexp" puni-syntactic-backward-punct :transient t)
    ("l" "Backward Sexp" puni-syntactic-forward-punct :transient t)
    ("a" "Beginning Sexp" puni-beginning-of-sexp :transient t)
    ("e" "End Sexp" puni-end-of-sexp :transient t)]]

  [("q" "Quit" transient-quit-all)])

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
  ("X" . puni-sexp-menu))

(add-hooks '(prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook yaml-ts-mode-hook)
           #'puni-mode)

(provide 'init-puni)
;;; init-puni.el ends here
