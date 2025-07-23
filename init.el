
;; (setq-default inhibit-redisplay t
;;               inhibit-message t)
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (setq-default inhibit-redisplay nil
;;                           inhibit-message nil)
;;             (redisplay)))

;; (toggle-debug-on-error)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; 启动必须加载
;; Need install packages
(require 'init-packages)


(require 'init-mode)
(require 'init-hook)
(require 'init-gcmh)
;; (require 'init-auto-save)
(require 'init-super-save)


(require 'init-key)
(require 'init-hydra)
(require 'init-meow)
(require 'init-transient)
(require 'init-ui)

(require 'init-edit)
(require 'init-spell)
(require 'init-input)
(require 'init-auto-insert)
(require 'init-separedit)
(require 'init-auto-revert)

(require 'init-minibuffer)
(require 'init-corfu)
(require 'init-completion-preview)
(unless (or (not user/completion-preview-mode-use) user/ai-completion)
  ;; Enable Completion Preview mode in code buffers
  (add-hook 'prog-mode-hook #'completion-preview-mode)
  (add-hook 'text-mode-hook #'completion-preview-mode))
(require 'init-snippet)
(require 'init-color-rg)
(require 'init-blink-search)
(require 'init-tramp)

(require 'init-toolkit)

(require 'init-dired)
(require 'init-helpful)
(require 'init-calender)

(require 'init-org)
(require 'init-denote)
(require 'init-pangu)
(require 'init-plantuml)
;; (require 'init-hugo)
;; (require 'init-reader)
;; (require 'init-paper)
;;(require 'crefactor)

(require 'init-gt)

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


(when (and user/telega-start (display-graphic-p))
  (message "start telega")
  (autoload '+lizqwer/toggle-telega "init-telega" nil t)
  (+lizqwer/toggle-telega))

(when user/load-eaf
  (require 'init-eaf))

(when user/load-elfeed
  (require 'init-elfeed))

;;; init.el ends here.
