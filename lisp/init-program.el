;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

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
(when user/flyoverp
  (require 'flyover)
  (add-hook 'flycheck-mode-hook #'flyover-mode)
  (setq flyover-use-theme-colors t
        flyover-checkers '(flycheck)
        flyover-show-at-eol t
        flyover-virtual-line-icon "-> "
        flyover-virtual-line-type nil))

;; flycheck posframe
(unless user/flyoverp
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook
            #'flycheck-posframe-mode))

;;; debug
(require 'init-dap)

(defun my/eldoc-box-or-other-window-scroll-down ()
  "If eldoc-box child frame and buffer exist, scroll down within the child frame.
Otherwise scroll down other windos."
  (interactive)
  (if (frame-visible-p eldoc-box--frame)
      (eldoc-box-scroll-down 3)
    (watch-other-window-internal "down"
                                 (/ (window-body-height) 3))))

(defun my/eldoc-box-or-other-window-scroll-up ()
  "If eldoc-box child frame and buffer exist, scroll up within the child frame.
Otherwise scroll up other window."
  (interactive)
  (if (frame-visible-p eldoc-box--frame)
      (eldoc-box-scroll-up 3)
    (watch-other-window-internal "up"
                                 (/ (window-body-height) 3))))

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
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)

    (with-eval-after-load 'eglot
      (keymap-sets eglot-mode-map
        '((("M-N" "s-N") . my/eldoc-box-or-other-window-scroll-up)
          (("M-P" "s-P") . my/eldoc-box-or-other-window-scroll-down))))))

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

(defun not-split-window (orig-fn &rest args)
  "Let ORIG-FN not split window.
ARGS is ORIG-FN args."
  (let ((split-height-threshold nil)
        (split-width-threshold nil))
    (apply orig-fn args)))

(advice-add #'next-error-no-select :around #'not-split-window)
(advice-add #'previous-error-no-select :around #'not-split-window)
(advice-add #'compile-goto-error :around #'not-split-window)

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
(when user/python
  (require 'init-python))
(when user/haskell
  (require 'init-haskell))
(when user/c++
  (require 'init-c++))
(when user/web
  (require 'init-web))
(when user/common-lisp
  (require 'init-common-lisp))
(when user/scheme
  (require 'init-scheme))
(when user/rust
  (require 'init-rust))
(when user/golang
  (require 'init-go))
(when user/sql
  (require 'init-sql))

(provide 'init-program)
;;; init-program.el ends heres.
