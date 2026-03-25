;;; init.el --- init                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; init and load custom
(require 'init-const)
(require 'init-custom)
(and (file-readable-p custom-file) (load custom-file))

;;; install early basic packages
(defvar *package-early-install-list*
  '(no-littering
    benchmark-init
    exec-path-from-shell

    pretty-mode
    doom-themes

    (lazy-load :host github :repo "manateelazycat/lazy-load")
    (one-key :host github :repo "manateelazycat/one-key")))

(packages! *package-early-install-list*)

;;; benchmark activate
;; (require 'benchmark-init)
(benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;;; init
(require 'init-startup)
(require 'lazy-load)
(require 'one-key)

;; 启动必须加载
;; Need install packages
(require 'init-packages)

(unless (fboundp #'igc-stats)
  (require 'init-gcmh))

(require 'init-font)
;; (require 'init-auto-save)
(require 'init-super-save)

(require 'init-hydra)
(require 'init-ui)
(require 'init-meow)
(require 'init-key)
(require 'init-transient)

(require 'init-edit)
(require 'init-auto-insert)
(require 'init-separedit)

(require 'init-minibuffer)
(require 'init-completion)

(require 'init-snippet)
(require 'init-color-rg)

(require 'init-tramp)

(require 'init-toolkit)

(require 'init-dired)
(require 'init-ibuffer)
(require 'init-helpful)
(require 'init-calendar)
(require 'init-tools)


(require 'init-org)
(require 'init-denote)
(require 'init-plantuml)

;;(require 'crefactor)
(require 'init-writer)

(require 'init-language)

(when user/rsync-project-mode
  (require 'init-rsync))

;; (require 'init-code-stats)

(require 'axis)
(setq axis-db-location
      (expand-file-name "var/axis-data.sqlite"
                        user-emacs-directory))

(with-hook (prog-mode text-mode)
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name)))
    (axis-mode)))

(require 'init-ai)

;;; Programming
(require 'init-git)
(require 'init-citre)
(require 'init-program)
(require 'init-lsp)

;; ;; Enable Completion Preview mode in code buffers
;; (add-hook 'prog-mode-hook #'completion-preview-mode)
;; (setq completion-preview-minimum-symbol-length 2)

(when user/telegap
  (require 'init-telega))

(when user/mms
  (require 'init-mms))

(when user/load-eaf
  (require 'init-eaf))

(when user/load-elfeed
  (require 'init-elfeed))

;; browse support
(setq browse-url-browser-function
      (if sys/macp
          #'browse-url-default-macosx-browser
        #'browse-url-firefox))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here.
