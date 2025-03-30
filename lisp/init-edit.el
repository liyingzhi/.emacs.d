
;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

;;; edit buffer content
(defun my/copy-current-line ()
  "Copy the current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (line-beginning-position)
                      (line-end-position)))))

(defun my/select-end-of-buffer-to-point ()
  "Select contents from the end of the buffer to the current point."
  (interactive)
  (push-mark (point-max) t t)
  (goto-char (point)))

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)

(advice-add #'meow-grab
            :before
            #'(lambda ()
                (call-interactively #'electric-pair-mode)))

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

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

(keymap-sets isearch-mode-map
             '(("C-c C-r" . visual-replace-from-isearch)))

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
      lazy-count-prefix-format "%s/%s ")

(defvar my/isearch--direction nil)

(defun my/isearch-repeat (&optional arg)
  (interactive "P")
  (isearch-repeat my/isearch--direction arg))

(with-eval-after-load 'isearch

  (define-advice isearch-exit (:after nil)
    (setq-local my/isearch--direction nil))
  (define-advice isearch-repeat-forward (:after (_))
    (setq-local my/isearch--direction 'forward))
  (define-advice isearch-repeat-backward (:after (_))
    (setq-local my/isearch--direction 'backward))

  (keymap-sets isearch-mode-map
               '(("<return>" . my/isearch-repeat)
                 ("<escape>" . isearch-exit))))

(add-hooks '(emacs-lisp-mode lisp-mode)
           #'aggressive-indent-mode )

;;; indent yank
(require 'indent-yank)
(add-hook 'prog-mode-hook
         #'indent-yank-mode)

;;; Outline indent
(require 'init-outline-indent)



(provide 'init-edit)
