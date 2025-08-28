;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Base Org

;;; Org base
(require 'org)

(setq org-default-notes-file "~/Documents/Org/index.org")

(setq org-tags-column -80
      org-log-done 'time
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-startup-folded 'show2levels
      org-pretty-entities nil
      org-hide-emphasis-markers t)

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
        (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")
        (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c/!)")
        (sequence "WAITING(w/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c/!)")))
;; (setq org-todo-keywords
;;       '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
;;         (sequence "REPORT(r!)" "BUG(b@/!)" "|" "FIXED(f@/!)")))

;; (setq org-list-demote-modify-bullet
;;       (quote (("+" . "-")
;;               ("-" . "+")
;;               ("*" . "-")
;;               ("1." . "-")
;;               ("1)" . "-")
;;               ("A)" . "-")
;;               ("B)" . "-")
;;               ("a)" . "-")
;;               ("b)" . "-")
;;               ("A." . "-")
;;               ("B." . "-")
;;               ("a." . "-")
;;               ("b." . "-"))))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0))
(setq org-enforce-todo-dependencies t)

;;; Org function
;; space ww
(defun open-my-org-file ()
  "打开我的org文件."
  (interactive)
  (dired "~/Documents/Org/"))

(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (expand-file-name "config/template/template.docx" user-emacs-directory)))
    (shell-command
     (format "pandoc %s -o %s --reference-doc=%s"
             (buffer-file-name)
             docx-file
             template-file))
    (message "Convert finish: %s" docx-file)))

(defun org-insert-html-head ()
  (interactive)
  (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>"))

(defun org-insert-html-theme-bigblow ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))

(defun org-insert-html-theme-readtheorg ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))

;;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (latex . t)
   (shell . t)
   (plantuml . t)))

(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (let* ((context (org-element-context))
               (parameters (org-element-property :parameters context)))
          ;; check if exist :eval yes
          (if parameters
              (not (string-match-p ":eval +yes" parameters))
            t))))

;;; UI

;;; Make invisible parts of Org elements appear visible
(add-hook 'org-mode-hook 'org-appear-mode)

;;; 中文标记隐藏空格
(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                          ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                           (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                        'append)

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Make verbatim with highlight text background.
;; (add-to-list 'org-emphasis-alist
;;              '("=" (:background "#fef7ca")))
;; Make deletion(obsolote) text foreground with dark gray.
;; (add-to-list 'org-emphasis-alist
;;            '("+" (:foreground "dark gray"
;;                   :strike-through t)))
;; Make code style around with box.
;; (add-to-list 'org-emphasis-alist
;;              '("~" (:box (:line-width 1
;;                                       :color "grey75"
;;                                       :style released-button))))

(require 'valign)
(setq valign-facy-bar t)
(add-hook 'org-mode-hook #'valign-mode)

(require 'org-fancy-priorities)
(setq org-fancy-priorities-list
      '((?A . "A")
        (?B . "⬆")
        (?C . "⬇")
        (?D . "☕")
        (?1 . "⚡")
        (?2 . "2")
        (?3 . "3")
        (?4 . "☕")
        (?I . "Important")))
(add-hook 'org-mode-hook #'org-fancy-priorities-mode)

;; (require 'org-bars)
;; (add-hook 'org-mode-hook
;;           #'org-bars-mode)
;; (add-hook 'org-mode-hook
;;           #'org-num-mode)

;; (setq org-bars-stars '(:empty "◉"
;;                        :invisible "▶"
;;                        :visible "▼"))

(add-hook 'org-mode-hook
          #'visual-line-mode)

(add-hook 'org-mode-hook
          #'org-modern-indent-mode 90)

;;; keymap
(keymap-sets org-mode-map
  '(("C-c TAB" . org-insert-item)
    ("M-P" . org-metaup)
    ("M-N" . org-metadown)
    ("M-H" . org-metaleft)
    ("M-L" . org-metaright)
    ("C-c C-'" . separedit/edit-org-any-block)
    ("M-g o" . consult-org-heading)))

(keymap-unset org-mode-map "M-<left>")
(keymap-unset org-mode-map "M-<right>")

;;; Org capf
(defun my/org-capf ()
  (setq-local completion-at-point-functions
              `(cape-file
                cape-emoji
                ,(cape-capf-super
                  #'cape-dict
                  #'cape-dabbrev)
                pcomplete-completions-at-point)))

(add-hook 'org-mode-hook #'my/org-capf)

;;; Org consult
(setq consult-notes-file-dir-sources
      '(("Org"             ?o "~/Documents/Org/")))
(setq consult-notes-org-headings-files
      '("~/Documents/Org/idea.org"
        "~/Documents/Org/quote.org"))

(consult-notes-org-headings-mode)
(consult-notes-org-roam-mode)

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("xelatex %f"))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
           (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
;; Copy Done To-Dos to Today
(defun org-roam-copy-todo-to-today ()
  (interactive)
  (when (and (or (equal org-state "DONE") (equal org-state "CANCELLED")) (not (org-find-property "STYLE")))
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies-capture-today t "t")
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos))))))
;; C-x d 进入 dired 模式，m 来标记对应需要复制链接的图片，C-c n m 即可复制到需要的图片插入文本。
;; source: https://org-roam.discourse.group/t/is-there-a-solution-for-images-organization-in-org-roam/925
(defun dired-copy-images-links ()
  "Works only in dired-mode, put in kill-ring,
  ready to be yanked in some other org-mode file,
  the links of marked image files using file-name-base as #+CAPTION.
  If no file marked then do it on all images files of directory.
  No file is moved nor copied anywhere.
  This is intended to be used with org-redisplay-inline-images."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let* ((marked-files (dired-get-marked-files))
             (number-marked-files (string-to-number
                                   (dired-number-of-marked-files))))
        (when (= number-marked-files 0)
          (dired-toggle-marks)
          (setq marked-files (dired-get-marked-files)))
        (message "Files marked for copy")
        (dired-number-of-marked-files)
        (kill-new "\n")
        (dolist (marked-file marked-files)
          (when (org-file-image-p marked-file)
            (kill-append
             (concat "#+CAPTION: "
                     (file-name-base marked-file)
                     "\n#+ATTR_ORG: :width 800"
                     "\n[[file:"
                     ;; 需要绝对路径则直接用 marked-file
                     (replace-regexp-in-string "^\\(~/\\|/Users/[^/]+/\\)Library/CloudStorage/Dropbox/org/[^/]*/" "" marked-file)
                     "]]\n\n")
             nil)))
        (when (= number-marked-files 0)
          (dired-toggle-marks)))
    (message "Error: Does not work outside dired-mode")))

(require 'init-org-roam)

(require 'init-org-capture)

(require 'init-org-agenda)

(require 'init-org-reveal)

;; (require 'init-org-media-note)

(provide 'init-org)
