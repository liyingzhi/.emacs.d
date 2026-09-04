;;; init-corfu.el --- init corfu package             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun +complete ()
  "TAB complete."
  (interactive)
  (when (meow-insert-mode-p)
    (or ;; (tempel-complete t)
     (yas-expand)
     (if user/completion-preview-mode-use
         (completion-preview-insert))
     (ai-complete)
     (corfu-complete))))

(require 'corfu)
(setq corfu-auto t
      corfu-quit-no-match t
      corfu-auto-prefix 2
      corfu-preview-current nil
      corfu-on-exact-match nil
      corfu-auto-delay 0.2
      corfu-popupinfo-delay '(0.4 . 0.2))

(custom-set-faces
 '(corfu-border ((t (:inherit region :background unspecified)))))

(keymap-sets corfu-map
  '(("TAB" . +complete)
    ;; ("[tab]" . +complete)
    ("S-TAB" . corfu-previous)
    ;; ("[backtab]" . corfu-previous)
    ))

(pcase user/lsp-client
  ('eglot
   (add-hook 'after-init-hook #'global-corfu-mode)
   (advice-add #'meow-grab
               :before
               #'(lambda ()
                   (call-interactively #'corfu-mode))))
  ('lsp-bridge
   (add-hooks '(rust-mode sly-mrepl-mode scheme-mode sql-mode eshell-mode inferior-python-mode elvish-mode telega-chat-mode)
              #'corfu-mode)))

(add-hook 'global-corfu-mode-hook #'corfu-popupinfo-mode)

(corfu-history-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)


;;; cape
(setopt cape-dabbrev-buffer-function #'current-buffer)
(add-list-to-list 'completion-at-point-functions
                  '(cape-file
                    cape-elisp-block
                    cape-keyword
                    cape-dabbrev))

(require 'cape)

(advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

(with-eval-after-load 'eglot
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

(defun my/eglot-capf-with-dabbrev ()
  "Setup up eglot capf with dabbrev."
  (setq-local completion-at-point-functions
              `(cape-file
                ,@(when (bound-and-true-p citre-mode)
                    (list
                     (cape-capf-super
                      #'citre-completion-at-point
                      #'cape-dabbrev)))
                ,(cape-capf-super
                  #'eglot-completion-at-point
                  (cape-capf-prefix-length #'cape-dabbrev 5)))
              cape-dabbrev-buffer-function #'cape-same-mode-buffers))

(defun my/eglot-capf ()
  "Setup up eglot capf."
  (setq-local completion-at-point-functions
              `(cape-file
                ,@(when (bound-and-true-p citre-mode)
                    (list #'citre-completion-at-point))
                eglot-completion-at-point)))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf-with-dabbrev)

(defun my/elisp-capf ()
  "Setup up elisp capf."
  (setq-local completion-at-point-functions
              `(cape-file
                ,(cape-capf-super
                  #'elisp-completion-at-point
                  (cape-capf-prefix-length #'cape-dabbrev 5)))))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-capf)

;;; nerd icons corfu
(require 'nerd-icons-corfu)

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; Symbols Nerd Font Mono glyphs are 1-2px taller than the default font.
;; corfu sizes the child frame as n × (default-line-height), so adding a
;; small line-spacing *only* in the corfu buffer makes that value reflect
;; the true rendered line height without touching anything else.
(advice-add 'corfu--make-buffer :filter-return
            #'(lambda (buf)
                (with-current-buffer buf
                  (setq-local line-spacing 1))
                buf))

(provide 'init-corfu)
;;; init-corfu.el ends here
