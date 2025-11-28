;;; early-init.el --- early init                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer noninteractive)
(setq native-comp-async-query-on-exit t)


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)
;; 增加IO性能
(setq read-process-output-max (* 1024 1024 10))
(setq gc-cons-threshold most-positive-fixnum)

(setq load-path-filter-function #'load-path-filter-cache-directory-files)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding) (defvaralias)))

;;; straight package manager

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications
        '(find-at-startup find-when-checking)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(defun packages! (packages)
  "Install PACKAGES."
  (dolist (package packages)
    (straight-use-package package)))

(defalias 'wait-packages! #'packages!)

(defun get-repo-info-from-url (url)
  "Get repo info from URL.
return (HOSTING-SITE OWNER REPO-NAME)。"
  (let ((patterns
         '((github . "https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?")
           (gitlab . "https?://gitlab\\.com/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?")
           (bitbucket . "https?://bitbucket\\.org/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?")
           (sourcehut . "https?://git\\.sr\\.ht/~\\([^/]+\\)/\\([^/]+\\)")
           (codeberg . "https?://codeberg\\.org/\\([^/]+\\)/\\([^/]+\\)\\(?:\\.git\\)?"))))
    (catch 'found
      (dolist (pattern patterns)
        (let ((site (car pattern))
              (regexp (cdr pattern)))
          (when (string-match regexp url)
            (throw 'found (list site
                                (match-string 1 url)
                                (match-string 2 url))))))
      nil)))

(defun straight-get-recipe-from-clipboard-url ()
  "Straight get recipe from clipboard url."
  (interactive)
  (cl-assert (or (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0))
                 (string-match-p "^git@.*:" (current-kill 0)))
             nil "No URL in clipboard")
  (when-let* ((url (current-kill 0))
              (info (get-repo-info-from-url url))
              (host (car info))
              (owner (elt info 1))
              (repo-name (elt info 2))
              (repo-name (replace-regexp-in-string "\\.git$" "" repo-name))
              (recipe (format "(%s :host %s :repo \"%s/%s\")"
                              repo-name
                              host
                              owner
                              repo-name)))
    (insert recipe)))

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lisp")))
  (add-to-list 'load-path
               (expand-file-name
                (concat user-emacs-directory "lib")))

  (require 'lib-elisp-utils)
  (require 'lib-utils)

  (add-subdirs-to-load-path
   (concat user-emacs-directory
           "site-lisp/"))

  (defvar *package-early-install-list*
    '(no-littering
      benchmark-init
      exec-path-from-shell

      pretty-mode
      doom-themes

      (lazy-load :host github :repo "manateelazycat/lazy-load")
      (one-key :host github :repo "manateelazycat/one-key")))

  (packages! *package-early-install-list*)

  ;; (require 'benchmark-init)
  ;; (benchmark-init/activate)

  (require 'init-const)
  (require 'init-custom)
  (require 'init-startup)
  (require 'lazy-load)
  (require 'one-key)
  (require 'init-font))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; early-init.el ends here
