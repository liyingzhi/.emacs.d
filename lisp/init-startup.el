;; package repositories
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/") t)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; load custom
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq-default lexical-binding t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(require 'no-littering)

(let ((path "~/.emacs.d/tmp/"))
  (unless (file-directory-p path)
    (make-directory path))
  (setq temporary-file-directory path))

;;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(setq default-process-coding-system '(utf-8 . utf-8))
(setq default-buffer-file-coding-system 'utf-8)

(if (not sys/win32p)
    (progn    (set-clipboard-coding-system 'utf-8)
              (set-selection-coding-system 'utf-8))
  (set-clipboard-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  ;;; reference: https://github.com/protesilaos/denote/issues/150
  ;; scoop install findutils
  (setq process-coding-system-alist
        '(("[pP][lL][iI][nN][kK]" utf-8-dos . gbk-dos)
          ("[cC][mM][dD][pP][rR][oO][xX][yY]" utf-8-dos . gbk-dos)
          ("*" utf-8 . utf-8))))

(setq visible-bell nil
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t ;按照中文折行
      ring-bell-function 'ignore ; 禁止响铃
      )


(customize-set-variable 'kill-do-not-save-duplicates t)
(customize-set-variable 'auto-revert-interval 1)
(setq ad-redefinition-action 'accept)

(setq delete-old-versions t)
(setq create-lockfiles nil)

(setq confirm-kill-processes nil)

;; (setq truncate-partial-width-windows nil)

(setq mouse-yank-at-point t)

(provide 'init-startup)
