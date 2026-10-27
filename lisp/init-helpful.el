;;; init-helpful.el --- helpful                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lazy-load-global-keys
 '(("C-h f" . helpful-callable)
   ("C-h C-f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command))
 "helpful")

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(add-hooks '(help-mode helpful-mode)
           #'visual-line-mode)

(with-eval-after-load 'helpful
  (defun helpful-goto-info ()
    "View the *info* node of the current help item."
    (interactive)
    (info-lookup-symbol helpful--sym 'emacs-lisp-mode nil))

  (defun helpful-view-source ()
    "View the source of the current help item."
    (interactive)
    (pcase-let* ((primitive-p (helpful--primitive-p helpful--sym helpful--callable-p))
                 (lookfor-src-p (or (not primitive-p)
                                    find-function-C-source-directory))
                 (`(,buf ,pos _) (if lookfor-src-p
                                     (helpful--definition helpful--sym helpful--callable-p)
                                   '(nil nil nil))))
      (cond
       (buf
        (let ((path (buffer-file-name buf)))
          (if (not path)
              (switch-to-buffer-other-window buf)
            (find-file-other-window path)
            (revert-buffer-quick))
          (when pos
            (goto-char pos)
            (recenter))))
       (primitive-p
        (message "defined in C source code"))
       (t
        (message "without a source file")))))

  (defun helpful-customize ()
    "Customize variable or face whose doc string is shown in the current buffer."
    (interactive)
    (let ((sym helpful--sym))
      (unless (or (boundp sym) (facep sym))
        (user-error "No variable or face to customize"))
      (cond
       ((boundp sym) (customize-variable sym))
       ((facep sym) (customize-face sym)))))

  (keymap-binds helpful-mode-map
    ("h" . meow-left)
    ("j" . meow-next)
    ("k" . meow-prev)
    ("l" . meow-right)
    ("x" . meow-line)
    (";" . meow-reverse)
    ("y" . meow-save)
    ("w" . meow-mark-word)
    ("W" . meow-mark-symbol)
    ("L" . meow-right-expand)
    ("H" . meow-left-expand)
    ("J" . meow-next-expand)
    ("K" . meow-prev-expand)
    ("m" . describe-mode)
    ("i" . helpful-goto-info)
    ("s" . helpful-view-source)
    ("c" . helpful-customize)
    ("M-<left>" . previous-buffer-dedicated-window)
    ("M-<right>" . next-buffer-dedicated-window)))

(keymap-binds help-mode-map
  ("h" . meow-left)
  ("j" . meow-next)
  ("k" . meow-prev)
  ("l" . meow-right)
  ("x" . meow-line)
  (";" . meow-reverse)
  ("y" . meow-save)
  ("w" . meow-mark-word)
  ("W" . meow-mark-symbol)
  ("L" . meow-right-expand)
  ("H" . meow-left-expand)
  ("J" . meow-next-expand)
  ("K" . meow-prev-expand)
  ("m" . describe-mode)
  ("n" . forward-button)
  ("p" . backward-button)
  ("M-<left>" . previous-buffer-dedicated-window)
  ("M-<right>" . next-buffer-dedicated-window))

;;; for embark
(with-eval-after-load 'embark
  (keymap-binds embark-symbol-map
    ("h" . helpful-symbol)))

(provide 'init-helpful)
;;; init-helpful.el ends here
