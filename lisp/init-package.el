;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>


;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(defun packages! (packages)
  (dolist (package packages)
    (if (listp package)
        (quelpa package)
      (unless (package-installed-p package)
        (condition-case-unless-debug err
            (if (assoc package package-archive-contents)
                (package-install package)
              (package-refresh-contents)
              (package-install package))
          (error
           (display-warning 'package
                            (format "Failed to install %s: %s"
                                    package (error-message-string err))
                            :error)))))))

(packages! '(quelpa
             ;; quelpa-use-package
             ))

(setq quelpa-update-melpa-p nil)
;; (quelpa-use-package-activate-advice)

;; (update-load-path)
(defun site-lisp-update ()
  "Update site-lisp packages."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Update site lisp*")))
    (async-shell-command
     (concat "cd "
             user-emacs-directory
             " && git submodule foreach git pull")
     output-buffer)
    (switch-to-buffer-other-window output-buffer)))

;; (use-package quelpa
;;   :ensure t
;;   :custom
;;   (quelpa-update-melpa-p nil)
;;   :config
;;   (use-package quelpa-use-package
;;     :ensure t)
;;   (quelpa-use-package-activate-advice))

(defun emacs-update ()
  "Update Emacs all packages."
  (interactive)
  (site-lisp-update)
  (when (version<= "29" emacs-version)
    (package-upgrade-all))
  (quelpa-upgrade-all))

;;; install all package

(defvar *package-early-install-list*
  '(no-littering
    benchmark-init
    exec-path-from-shell
    pretty-mode
    (standard-themes :fetcher github :repo "protesilaos/standard-themes")
    (lazy-load :fetcher github :repo "manateelazycat/lazy-load")
    (one-key :fetcher github :repo "manateelazycat/one-key")
    (fingertip :fetcher github :repo "manateelazycat/fingertip")
    (color-rg :fetcher github
              :repo "manateelazycat/color-rg")
    (awesome-tray :fetcher github
                  :repo "manateelazycat/awesome-tray")
    doom-modeline
    (meow :fetcher github :repo "meow-edit/meow")
    meow-tree-sitter
    highlight-function-calls
    lisp-extra-font-lock
    diff-hl
    hungry-delete
    org-modern
    hydra
    pretty-hydra
    outshine
    aggressive-indent
    difftastic
    dashboard
    gcmh
    yasnippet
    consult-yasnippet
    macrostep
    eglot
    consult-eglot
    (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
    apheleia
    visual-replace
    corfu
    cape
    eldoc-box
    markdown-mode
    nerd-icons
    rainbow-delimiters
    colorful-mode
    color-identifiers-mode
    (pulsar :fetcher github :repo "protesilaos/pulsar")
    (lazy-revert :fetcher github :repo "yilin-zhang/lazy-revert")
    page-break-lines
    dired-quick-sort
    dired-rsync
    dired-rsync-transient
    dired-git-info
    diredfl
    dired-subtree
    diredfl
    symbol-overlay
    symbol-overlay-mc
    separedit
    casual
    casual-symbol-overlay
    fanyi
    posframe
    outline-indent
    pyim
    pyim-basedict
    go-translate
    (breadcrumb :fetcher github :repo "joaotavora/breadcrumb")
    (watch-other-window :fetcher github :repo "manateelazycat/watch-other-window")
    (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    consult-notes
    denote
    denote-menu
    consult-denote
    valign
    org-fancy-priorities
    org-journal
    org-appear
    org-roam
    (pangu-spacing
     :fetcher github
     :repo  "nailuoGG/pangu-spacing"
     :branch "remove-old-version-support"
     :files ("*.el"))
    (indent-bars :fetcher github :repo  "jdtsmith/indent-bars")
    (image-slicing :fetcher github :repo "ginqi7/image-slicing")))

(packages! *package-early-install-list*)

(defvar *package-miss-install-list*
  '(
    projection
    doom-themes
    dracula-theme
    vundo
    ace-window
    highlight-parentheses
    shackle
    popper
    buffer-name-relative
    vertico
    nerd-icons-corfu
    marginalia
    orderless
    nerd-icons-completion
    nerd-icons-dired
    citre
    magit
    magit-delta
    helpful
    org-bullets
    fussy
    (flx-rs
     :repo "jcs-elpa/flx-rs"
     :fetcher github
     :files (:defaults "bin"))
    (pyim-tsinghua-dict
     :fetcher github
     :repo "redguardtoo/pyim-tsinghua-dict"
     :files ("pyim-tsinghua-dict.el" "pyim-tsinghua-dict.pyim"))
    super-save
    eshell-prompt-extras
    elisp-demos
    modus-themes
    cal-china-x))

(packages! *package-miss-install-list*)

(defvar *package-toolkit-install-list*
  '((thing-edit :fetcher github :repo "manateelazycat/thing-edit")
    (delete-block :fetcher github :repo "manateelazycat/delete-block")
    (move-text :fetcher github :repo "manateelazycat/move-text")
    (open-newline :fetcher github :repo "manateelazycat/open-newline")
    (duplicate-line :fetcher github :repo "manateelazycat/duplicate-line")
    (markmacro :fetcher github :repo "manateelazycat/markmacro")))

(packages! *package-toolkit-install-list*)


(provide 'init-package)
;;; init-package.el ends here
