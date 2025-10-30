;;; init.el --- init                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(and (file-readable-p custom-file) (load custom-file))

(when user/start-fullscreen
  (unless sys/macp
    (toggle-frame-fullscreen)))

;; 启动必须加载
;; Need install packages
(require 'init-packages)

(unless (fboundp #'igc-stats)
  (require 'init-gcmh))

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
(require 'init-blink-search)
(require 'init-tramp)

(require 'init-toolkit)

(require 'init-dired)
(require 'init-ibuffer)
(require 'init-helpful)
(require 'init-calendar)
(require 'init-tools)


(require 'init-org)
(require 'init-denote)
(require 'init-pangu)
(require 'init-plantuml)

;;(require 'crefactor)
(require 'init-writer)

(require 'init-language)
(require 'init-rsync)
;; (require 'init-code-stats)

(require 'init-ai)

;;; Programming
(require 'init-git)
(require 'init-lsp)
(require 'init-citre)
(require 'init-program)

;; (require 'codeium)
;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

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

(setq native-comp-jit-compilation t)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here.
