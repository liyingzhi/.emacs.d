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

;;; check error
;; flymake
(add-hook 'after-init-hook
          #'flymake-mode)
(setq flymake-run-in-place nil)

;;; debug
(require 'init-dap)

;;; complile
(setq compilation-scroll-output nil)
(setq compilation-auto-jump-to-first-error nil)
(setq compilation-max-output-line-length nil)

(defun ar/compile-autoclose-or-jump-first-error (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (when (compilation-buffer-p buffer)
    (if (and (string-match "finished" string)
           (not (string-match "^.*warning.*" string)))
        (progn
          (message "Build finished :)")
          (run-with-timer 1 nil
                          (lambda ()
                            (when-let* ((multi-window (> (count-windows) 1))
                                        (live (buffer-live-p buffer))
                                        (window (get-buffer-window buffer t)))
                              (delete-window window)))))
      (progn
        (message "Compilation %s" string)
        (call-interactively #'compilation-next-error)))))

(require 'alert)
(setq alert-default-style 'mode-line)
(defun ar/alert-after-finish-in-background (buffer string)
  (when (and (compilation-buffer-p buffer)
           (or (not (get-buffer-window buffer 'visible))
              (not (frame-focus-state))))
    (if (and (string-match "finished" string)
           (not (string-match "^.*warning.*" string)))
        (alert string :buffer buffer :severity 'normal)
      (alert string :buffer buffer :severity 'high))))

(setq compilation-finish-functions
      (list #'ar/alert-after-finish-in-background
            #'ar/compile-autoclose-or-jump-first-error))

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
