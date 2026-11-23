;;; init-straight.el --- straight package manager    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  fly_lilee

;; Author: fly_lilee <liyl2015@mail.ustc.edu.cn>
;; Keywords: tools

;;; Commentary:

;;; Code:

;;; straight package manager
;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setopt straight-check-for-modifications '(watch-files find-when-checking))
  (setopt straight-check-for-modifications
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

(defun straight--normalize-recipe (package)
  "Normalize PACKAGE into a recipe straight.el understands.
Rename :fetcher to :host, and expand :url into :host plus :repo via
`get-repo-info-from-url'.  An explicit :host or :repo wins over the
value derived from :url.  PACKAGES that are not recipes are returned
unchanged."
  (if (not (and (listp package) (keywordp (car-safe (cdr package)))))
      package
    (let* ((plist (cdr package))
           (host (plist-get plist :host))
           (repo (plist-get plist :repo))
           (result nil))
      (while plist
        (let ((prop (pop plist))
              (val (pop plist)))
          (pcase prop
            (:fetcher
             (unless host (setq host val)))
            (:url
             (let ((info (get-repo-info-from-url val)))
               (if (null info)
                   ;; Unrecognised URL: leave it alone
                   (setq result (append result (list prop val)))
                 (unless host (setq host (nth 0 info)))
                 (unless repo
                   (setq repo (format "%s/%s"
                                      (nth 1 info)
                                      (replace-regexp-in-string
                                       "\\.git\\'" "" (nth 2 info))))))))
            (_
             (setq result (append result (list prop val)))))))
      (when (and host (not (plist-get result :host)))
        (setq result (append result (list :host host))))
      (when (and repo (not (plist-get result :repo)))
        (setq result (append result (list :repo repo))))
      (cons (car package) result))))

(defun packages! (packages)
  "Install PACKAGES, normalizing each recipe with `straight--normalize-recipe'."
  (dolist (package packages)
    (straight-use-package (straight--normalize-recipe package))))

(defalias 'wait-packages! #'packages!)

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

(setopt magit-repository-directories
        `((,(expand-file-name "straight/repos/" user-emacs-directory) . 1)))

;;; straight-overview
(setopt straight-overview-build-on-pull t
        straight-overview-pinned-file (expand-file-name
                                       "straight/versions/straight-overview-pinned.eld"
                                       user-emacs-directory))

(with-eval-after-load 'straight-overview
  (keymap-binds straight-overview-mode-map
    ("S" . hydra-straight-helper/body)
    ("s" . tabulated-list-sort)))

(provide 'init-straight)
;;; init-straight.el ends here
