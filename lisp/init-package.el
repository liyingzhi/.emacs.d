;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>


;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; HACK: DO NOT save `package-selected-packages' to `custom-file'
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;; More options
(setq package-install-upgrade-built-in t)

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
    doom-themes
    dracula-theme
    modus-themes
    ef-themes
    (standard-themes :fetcher github :repo "protesilaos/standard-themes")
    (lazy-load :fetcher github :repo "manateelazycat/lazy-load")
    (one-key :fetcher github :repo "manateelazycat/one-key")))

(packages! *package-early-install-list*)

(provide 'init-package)
;;; init-package.el ends here
