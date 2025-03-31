
(setq user-emacs-directory "D:\\EmacsConfig\\.emacs.d\\")

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lisp")))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lib"))))


(require 'init-const)
(require 'init-startup)

(require 'init-utils)
(require 'init-mode)
(require 'init-gcmh)
(require 'init-super-save)

(require 'lazy-load)
(require 'one-key)
(require 'consult)
(require 'nerd-icons)

(require 'init-key)
(require 'mini-emacs)
(require 'init-font)
(require 'init-theme)
(require 'init-ui)
(require 'init-auto-revert)
(require 'init-corfu)
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(require 'init-minibuffer)
(require 'init-edit)
(require 'init-magit)
(require 'init-difftastic)
(require 'init-fingertip)
(require 'init-color-rg)
(require 'init-modeline)
(require 'init-citre)
(require 'init-dired)
(require 'init-helpful)
(require 'init-transient)
(require 'init-go-translate)
(require 'init-input)
(require 'init-program)
(require 'init-eglot)
(unless (or (not user/completion-preview-mode-use) user/ai-completion)
  (require 'init-completion-preview))
(require 'init-snippet)
(require 'init-separedit)
(require 'init-org)
(require 'init-denote)
(require 'init-pangu)
(require 'init-eshell)
(require 'init-calender)
(server-mode 1)
