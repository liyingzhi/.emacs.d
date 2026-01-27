;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

(require 'lib-edit)

;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)

(advice-add #'meow-grab
            :before
            #'(lambda ()
                (call-interactively #'electric-pair-mode)
                (call-interactively #'fingertip-mode)))

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

;;; fingertip
(require 'init-fingertip)

;;; expreg
(with-eval-after-load 'expreg
  (setq expreg-restore-point-on-quit t))

(require 'home-row-expreg-diverted)
(add-list-to-list 'home-row-expreg-diverted-commands '(meow-save indent-for-tab-command))
(home-row-expreg-diverted-mode 1)

;;; vundo
(require 'vundo)
(setq undo-limit (* 1 1024 1024))
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;; like replace-regexp with live visual feedback in the buffer
(require 'visual-regexp)
;; extension to visual-regexp which enables the use of modern regexp engines
(require 'visual-regexp-steroids)

;;; Visual Replace
(require 'visual-replace)
(global-set-key (kbd "C-c C-r") #'visual-replace)
(keymap-sets visual-replace-mode-map
  '(("C-r" . visual-replace-toggle-regexp)
    ("C-q" . visual-replace-toggle-query)
    ("C-w" . visual-replace-toggle-word)
    ("C-f" . visual-replace-toggle-case-fold)
    ("C-u" . visual-replace-undo)
    ("C-s" . visual-replace-toggle-scope)))
(setq visual-replace-min-length 1)

(transient-define-prefix visual-replace-dispatch ()
  "Visual replace menu."
  ["Toggles"
   ("r" "Regexp" visual-replace-toggle-regexp)
   ("s" "Scope" visual-replace-toggle-scope)
   ("q" "Query" visual-replace-toggle-query)
   ("w" "Word" visual-replace-toggle-word)
   ("c" "Case Fold" visual-replace-toggle-case-fold)
   ("l" "Lax ws" visual-replace-toggle-lax-ws)])

(keymap-set visual-replace-mode-map
            "C-o" #'visual-replace-dispatch)

(visual-replace-global-mode 1)

;;; isearch
(setq isearch-lazy-count t
      isearch-case-fold-search t
      lazy-count-prefix-format "%s/%s "
      search-whitespace-regexp ".*?"
      isearch-repeat-on-direction-change t
      isearch-wrap-pause nil)

;; use selection to search
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(defun my-isearch-consult-line-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (consult-line query)))

(defun my-occur-from-isearch ()
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (occur query)))

(with-eval-after-load 'isearch
  (defun my/isearch-forward-symbol-at-point ()
    "Search for symbol at point and ensure escape exits isearch."
    (interactive)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (isearch-forward-symbol-at-point))

  (keymap-sets isearch-mode-map
    '(("<escape>" . isearch-exit)
      ("C-." . my/isearch-forward-symbol-at-point)
      ("C-n" . my-isearch-consult-line-from-isearch)
      ("C-b" . my-occur-from-isearch)
      ("C-c C-r" . visual-replace-from-isearch)
      ("C-v" . visual-replace-from-isearch)))

  ;; add isearch-consult-line to casual-isearch-tmenu
  (with-eval-after-load 'casual-isearch
    (transient-define-suffix isearch-consult-line ()
      (interactive)
      (call-interactively #'my-isearch-consult-line-from-isearch))
    (transient-append-suffix 'casual-isearch-tmenu "u"
      '("c" "Use consult line" isearch-consult-line))))

;; repeat for isearch
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

;;; nxml
(with-eval-after-load 'nxml-mode
  (keymap-sets nxml-mode-map
    '(("C-c C-f" . nxml-down-element)
      ("C-c C-n" . nxml-forward-element)
      ("C-c C-p" . nxml-backward-element)
      ("C-c C-b" . nxml-backward-up-element))))

;;; csv-mode
;; disable line wrap
(add-hook 'csv-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; auto detect separator
(add-hook 'csv-mode-hook #'csv-guess-set-separator)
;; turn on field alignment
(add-hook 'csv-mode-hook #'csv-align-mode)

(add-hook 'csv-mode-hook #'display-line-numbers-mode)

;;; aggressive-indent
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'aggressive-indent-mode )

;;; indent yank
(require 'indent-yank)
(add-hooks '(python-mode python-ts-mode)
           #'indent-yank-mode)

;;; Outline indent
(with-eval-after-load 'outline-indent
  (setq outline-indent-ellipsis " ▼"))

(add-hooks '(yaml-ts-mode nxml-mode)
           #'outline-indent-minor-mode)

;;; Auto rename tag
(add-hooks '(html-mode web-mode)
           #'auto-rename-tag-mode)

;;; string inflection
(require 'auto-string-inflection)

;;; select and copy utils
(require 'select-copy-utils)

;;; grugru
(grugru-default-setup)

(grugru-define-multiple
  ((c-ts-mode c++-ts-mode)
   (non-alphabet "&&" "||")
   (non-alphabet "&" "|")
   (non-alphabet "+" "-")
   (non-alphabet "*" "/" "%")
   (non-alphabet ">>" "<<")
   (non-alphabet "+=" "-=")
   (non-alphabet "*=" "/=" "%=")
   (non-alphabet "&=" "|=" "^=")
   (non-alphabet ">>=" "<<=")
   (non-alphabet "++" "--")
   (non-alphabet "==" "!=")
   (non-alphabet "<" "<=" ">" ">=")
   (symbol "break" "continue")
   (symbol "signed" "unsigned"))
  (c++-ts-mode
   (symbol "true" "false")
   (symbol "vector" "array" "deque")
   (symbol "class" "struct")
   (symbol "float" "double")
   (symbol "private" "public" "protected")
   (symbol "pair" "tuple")
   (symbol "static_cast" "dynamic_cast" "reinterpret_cast" "const_cast"))
  ((python-mode python-ts-mode)
   (symbol "True" "False"))
  ((emacs-lisp-mode lisp-mode)
   (symbol "when-let" "if-let")))

(global-set-keys
 '((("C-c <") . remember-init)
   (("C-c >") . remember-jump)))

(keymap-binds buffer-navigation-repeat-map
  ("n" . next-buffer)
  ("p" . previous-buffer))

;;; editkit
(autoload #'editkit-transform-menu "editkit" nil t)
(autoload #'editkit-rectangle-menu "editkit" nil t)

(with-eval-after-load 'meow
  (meow-normal-define-key
   '("C-;" . editkit-transform-menu)))

(global-bind-keys
 ("C-c M" . editkit-rectangle-menu))


;;; Local Variables

;; Local Variables:
;; eval: (when user/hidden-outline (outline-hide-sublevels 2))
;; End:

(provide 'init-edit)
;;; init-edit.el ends here.
