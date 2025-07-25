;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:


;;; Ctags

;; (use-package citre
;;   :ensure t)

;;; Lsp Server
;; (require 'init-eglot)

;;; projection
(global-projection-hook-mode)

;;; Xref
(setq xref-show-xrefs-function 'consult-xref)
(setq xref-show-definitions-function 'consult-xref)

;;; check error
;; flycheck
(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)
(global-flycheck-mode)

;; flyover
(when user/flyover-start
  (require 'flyover)
  (add-hook 'flycheck-mode-hook #'flyover-mode)
  (setq flyover-use-theme-colors t
        flyover-checkers '(flycheck)
        flyover-show-at-eol t
        flyover-virtual-line-icon "-> "
        flyover-virtual-line-type nil))

;;; debug
(require 'init-dap)

;;; eldoc
(with-eval-after-load 'eldoc
  (when (childframe-workable-p)
    (require 'eldoc-box)
    (setq eldoc-box-lighter nil
          eldoc-box-only-multi-line t
          eldoc-box-clear-with-C-g t)

    (custom-set-faces
     '(eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
     '(eldoc-box-body ((t (:inherit tooltip)))))

    ;; (add-hook 'eglot-managed-mode-hook
    ;;           #'eldoc-box-hover-at-point-mode)

    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

;;; complile
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error nil)
(setq compilation-max-output-line-length nil)

(require 'alert)
(setq alert-default-style 'mode-line)

(defun get-first-compilation-error ()
  (when (compilation-buffer-p (current-buffer))
    (compilation--ensure-parse (point-min))
    (save-excursion
      (goto-char (point-min))
      (condition-case err
          (progn
            (compilation-next-error 1)
            (> (point)
               (point-min)))
        (error
         nil)))))

(defun ar/compile-autoclose-or-jump-first-error (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (with-current-buffer buffer
    (when (eq major-mode 'compilation-mode)
      (if (or (string-match "^.*warning.*" string)
              (get-first-compilation-error)
              (string-match ".*exited abnormally.*" string))
          (progn
            (message "Compilation %s" string)
            (goto-char (point-min))
            (call-interactively #'compilation-next-error)
            (when (or (not (get-buffer-window buffer 'visible))
                      (not (frame-focus-state)))
              (alert string :buffer buffer :severity 'high)))
        (message "Build finished :)")
        (run-with-timer 1 nil
                        (lambda ()
                          (when-let* ((multi-window (> (count-windows) 1))
                                      (live (buffer-live-p buffer))
                                      (window (get-buffer-window buffer t)))
                            (delete-window window))))
        (when (or (not (get-buffer-window buffer 'visible))
                  (not (frame-focus-state)))
          (alert string :buffer buffer :severity 'normal))))))

(setq compilation-finish-functions
      (list #'ar/compile-autoclose-or-jump-first-error))

;;; eat
(require 'init-eat)

;;; lisp
(add-hook 'before-save-hook
          #'(lambda ()
              (when (or (equal major-mode 'emacs-lisp-mode)
                        (equal major-mode 'lisp-mode)
                        (equal major-mode 'scheme-mode))
                (call-interactively #'check-parens))))

;;; language
(require 'init-elisp)
(require 'init-python)
;; (require 'init-haskell)
(require 'init-c++)
;; (require 'init-web)
;; (require 'init-common-lisp)
;; (require 'init-scheme)
;; (require 'init-rust)
;; (require 'init-sql)
;; (require 'init-go)

(provide 'init-program)
;;; init-program.el ends heres.
