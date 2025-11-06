;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Base Org
(require 'org)

(setq org-default-notes-file "~/Documents/Org/index.org")

(setq org-tags-column -80
      org-log-done 'time
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-startup-folded 'show2levels
      org-pretty-entities nil
      org-hide-emphasis-markers t
      org-link-keep-stored-after-insertion t)

(setq org-enforce-todo-dependencies t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
        (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")
        (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c/!)")
        (sequence "WAITING(w/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c/!)")))
;; (setq org-todo-keywords
;;       '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
;;         (sequence "REPORT(r!)" "BUG(b@/!)" "|" "FIXED(f@/!)")))

(setq org-format-latex-options (plist-put org-format-latex-options :scale user/org-format-latex-options-scale))

(setopt
 org-latex-compiler "xelatex"
 ;; org-latex-src-block-backend 'minted ;; ‘org-latex-listings’ is obsolete since 9.6; use ‘org-latex-src-block-backend’ instead.
 ;; org-latex-minted-options '(("breaklines")
 ;;                            ("linenos")
 ;;                            ("frame" "lines")
 ;;                            ("bgcolor" "lightgray")
 ;;                            ("numbersep=" "5pt"))
 org-latex-packages-alist
 '(;; hook right arrow with text above and below
   ;; https://tex.stackexchange.com/questions/186896/xhookrightarrow-and-xmapsto
   ("" "svg" t)
   ("" "svg-extract" t)
   ("" "minted" nil)
   ;; for chinese preview
   ;; ("fontset=LXGW WenKai,UTF8" "ctex" t)
   ("UTF8" "ctex" t))
 org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{xeCJK}
\\usepackage{amsmath}
\\setCJKmainfont{LXGW WenKai}
\\setmainfont{PragmataPro}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}"
 )

(add-hook 'org-mode-hook #'org-cdlatex-mode)

;; Add new template
(add-list-to-list 'org-structure-template-alist
                  '(("n" . "sidenote")
                    ("T" . "typeit")
                    ("!" . "alert")
                    ("L" . "lead")))
;; yank
(with-eval-after-load 'yank-media
  (add-to-list 'yank-media-preferred-types 'image/tiff))

;;; org export
;; Generic Org Export Settings
(with-eval-after-load 'org
  (setq org-export-with-drawers nil
        org-export-with-todo-keywords nil
        org-export-with-toc nil
        org-export-with-smart-quotes t
        org-export-date-timestamp-format "%e %B %Y"))

(require 'ox-org)

;; LaTeX PDF Export settings

(with-eval-after-load 'ox-latex
  (add-list-to-list
   'org-latex-classes
   (let ((filename (expand-file-name "config/template/org-latex-class.el" user-emacs-directory)))
     (when (file-exists-p filename)
       (with-temp-buffer
         (insert-file-contents filename)
         (read (current-buffer)))))))

;; Multiple LaTeX passes for bibliographies
(setq org-latex-pdf-process
      '("%latex -interaction nonstopmode -output-directory %o %f"
        "%bib %b"
        "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Clean temporary files after export
(setq org-latex-logfiles-extensions
      (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
              "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
              "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
              "tex" "bcf")))

(with-eval-after-load 'ox
  (require 'ox-hugo)
  (setq org-hugo-base-dir
        (file-truename user/org-hugo-base-dir-path)
        org-hugo-front-matter-format "yaml"
        org-hugo-auto-set-lastmod t)
  (add-to-list 'org-hugo-special-block-type-properties
               '("sidenote" . (:trim-pre t :trim-post t))))

(with-eval-after-load 'ox
  (require 'ox-reveal)
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(with-eval-after-load 'ox
  (require 'ox-epub))

(setq org-html-validation-link nil)

;;; Org function
(defun open-my-org-file ()
  "Open the default Org directory in dired mode."
  (interactive)
  (dired "~/Documents/Org/"))

(defun org-export-docx (&optional template-select)
  "Use pandoc convert org to Docx.

if TEMPLATE-SELECT is not nil, select a template file."
  (interactive "P")
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (file-truename
                        (if template-select
                            (read-file-name "Select Template file: "
                                            (expand-file-name "config/template/"
                                                              user-emacs-directory))
                          (expand-file-name "config/template/template.docx" user-emacs-directory)))))
    (shell-command
     (format "pandoc %s -o %s --reference-doc=%s"
             (buffer-file-name)
             docx-file
             template-file)
     (get-buffer-create "*Org export docx*")
     (get-buffer-create "*Org export docx error*"))
    (message "Convert finish: %s" docx-file)))

(defun org-toggle-display-emphasis-markers ()
  "Toggle show emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers
        (not org-hide-emphasis-markers))
  (revert-buffer-quick))

(defun latex-math-from-calc ()
  "Evaluate `calc' on the contents of line at point."
  (interactive)
  (cond ((region-active-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (string (buffer-substring-no-properties beg end)))
           (kill-region beg end)
           (insert (calc-eval `(,string calc-language latex
                                        calc-prefer-frac t
                                        calc-angle-mode rad)))))
        (t (let ((l (thing-at-point 'line)))
             (end-of-line 1) (kill-line 0)
             (insert (calc-eval `(,l
                                  calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad)))))))

(defun org-insert-item-auto-checkbox ()
  "Org insert auto-checkbox item."
  (interactive)
  (org-insert-item
   (and (org-in-item-p)
        (save-excursion
          (looking-back "\\].**" (line-beginning-position))))))

(defun org-meta-return-auto (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item-auto-checkbox' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-heading'."
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				                ((org-at-table-p) #'org-table-wrap-region)
				                ((org-in-item-p) #'org-insert-item-auto-checkbox)
				                (t #'org-insert-heading)))))

;;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (latex . t)
   (shell . t)
   (dot . t)))

(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (let* ((context (org-element-context))
               (parameters (org-element-property :parameters context)))
          ;; check if exist :eval yes
          (if parameters
              (not (string-match-p ":eval +yes" parameters))
            t))))

;;; UI

;; Make invisible parts of Org elements appear visible
(add-hook 'org-mode-hook 'org-appear-mode)

;; for emacs built-in org show raw latex fragments when at preview point
;;================start===================
(add-hook 'org-mode-hook 'org-fragtog-mode)
;;================end===================

;; for latex-preview enhanced branch
;;================start===================
;; (require 'org-latex-preview)
;; (plist-put org-latex-preview-appearance-options
;;            :page-width 0.8)
;; (add-hook 'org-mode-hook 'org-latex-preview-mode)
;; (setq org-latex-preview-mode-display-live t)
;; (setq org-latex-preview-mode-update-delay 0.25)
;;================end===================

;; 中文标记隐藏空格
(unless sys/macp
  (font-lock-add-keywords 'org-mode
                          '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                             (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                            ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                             (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                          'append))

(with-eval-after-load 'org-superstar
  (add-list-to-list 'org-superstar-todo-bullet-alist
                    '(("TODO"   . ?☐)
                      ("DOING"  . ?▶)
                      ("HANGUP" . ?⏸)
                      ("CANCEL" . ?✖)
                      ("NEXT"   . ?➡))))
(setq org-superstar-special-todo-items t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

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

(setq valign-facy-bar t)
(add-hook 'org-mode-hook #'valign-mode)

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

(add-hook 'org-mode-hook
          #'visual-line-mode)

(add-hook 'org-mode-hook
          #'org-modern-indent-mode 90)

;; count word
(with-hook org-mode
  (when (buffer-file-name)
    (org-count-words-mode)))

;; disable org-count-words-mode for capture narrow buffer
(advice-add #'org-count-words-update-buffer-count
            :before (lambda (_ &rest _)
                      (when-let* ((name (buffer-name))
                                  ((or (string-match-p "\\*Capture\\*" name)
                                       (string-match-p "^CAPTURE-.*" name))))
                        (org-count-words-mode -1))))

;;; org rich yank
(defun my-org-rich-yank-format-paste (language contents link)
  "Based on `org-rich-yank--format-paste-default'."
  (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n#+comment: %s"
          language
          (org-rich-yank--trim-nl contents)
          link))

;;; org-download
(with-hook (org-mode dired-mode)
  (org-download-enable))

(with-eval-after-load 'org-download
  (setopt org-download-image-dir (concat denote-directory "/assets/")
          org-download-screenshot-method "flameshot gui --raw > %s"
          org-download-heading-lvl nil))

;;; org-sliced-images
(require 'org-sliced-images)
(setq org-sliced-images-round-image-height t)
(setq org-sliced-images-consume-dummies t)
;; (org-sliced-images-mode 1)

;; fix: close super-save-delete-trailing-whitespace when org-sliced-images-mode is activated
(with-eval-after-load 'init-super-save
  (defvar org-sliced-images/super-save-delete-trailing-whitespace super-save-delete-trailing-whitespace)
  (with-hook (org-mode org-sliced-images-mode)
    (when (and (bound-and-true-p super-save-mode) (equal major-mode 'org-mode))
      (if org-sliced-images-mode
          (setq-local super-save-delete-trailing-whitespace nil)
        (setq-local super-save-delete-trailing-whitespace org-sliced-images/super-save-delete-trailing-whitespace)))))

;;; menu
(defun hot-expand (str &optional mod)
  "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end)))
    (insert str)
    (if (fboundp 'org-try-structure-completion)
        (org-try-structure-completion) ; < org 9
      (progn
        ;; New template expansion since org 9
        (require 'org-tempo nil t)
        (org-tempo-complete-tag)))
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(with-eval-after-load 'oxr
  ;; oxr insert absolute figure
  (defun oxr-insert-absolute-figure ()
    "Insert a new figure, with name and caption."
    (interactive)
    ;; TODO this inserts absolute paths ATM, which is not ideal.
    (let ((image_file (read-file-name "Image file: ")))
      (insert (oxr--metadata-prompt (oxr--get-name-prefix 'figure) t))
      (org-insert-link 'file image_file))))

(autoload #'oxr-insert-absolute-figure "oxr" nil t)

(pretty-hydra-define hydra-org-template
  (:title (pretty-hydra-title "Org Template" 'sucicon "nf-custom-orgmode" :face 'nerd-icons-green)
          :color blue :quit-key ("q" "C-g"))
  ("Basic"
   (("a" (hot-expand "<a") "ascii")
    ("c" (hot-expand "<c") "center")
    ("C" (hot-expand "<C") "comment")
    ("e" (hot-expand "<e") "example")
    ("E" (hot-expand "<E") "export")
    ("l" (hot-expand "<l") "latex")
    ("x" (hot-expand "<q") "quote")
    ("v" (hot-expand "<v") "verse")
    ("b" (hot-expand "<s" "bash") "bash"))
   "Head"
   (("i" (hot-expand "<i") "index")
    ("A" (hot-expand "<A") "ASCII")
    ("I" (hot-expand "<I") "INCLUDE")
    ("S" (insert "#+STARTUP: ") "Startup")
    ("O" (insert "#+OPTIONS: ") "Options")

    ("H" (yas-expand-snippet (yas-lookup-snippet "hugo")) "Hugo")

    ("L" (hot-expand "<L") "LaTeX")
    ("X" (yas-expand-snippet (yas-lookup-snippet "latex-chinese")) "Latex chinese")
    ("P" (insert "#+STARTUP: latexpreview ") "Latex Preview")

    ("Mb" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup") "Html Bigblow Theme")
    ("Mr" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup") "Html Readtheorg Theme")
    ("Mn" (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>") "Html Normal Css"))

   "Source"
   (("ss" (hot-expand "<s") "src")
    ("se" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
    ("sp" (hot-expand "<s" "python") "python")
    ("sP" (hot-expand "<s" "python :results output") "python")
    ("sc" (hot-expand "<s" "c++") "c++")
    ("sr" (hot-expand "<s" "rust") "rust")
    ("sS" (hot-expand "<s" "sh") "sh")
    ("sg" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang")
    ("sx" (hot-expand "<s" "xml") "xml")
    ("sy" (hot-expand "<s" "yaml-ts") "yaml")
    ("sh" (hot-expand "<h") "html"))
   "Oxr"
   (("f" oxr-insert-absolute-figure "Figure")
    ("t" oxr-insert-table "Table")
    ("on" oxr-insert-name "Name")
    ("oe" oxr-insert-equation "Equation")
    ("os" oxr-insert-section "Section"))
   "Hugo"
   (("hn" (hot-expand "<n") "sidenote")
    ("ht" (org-insert-structure-template "typeit") "typeit")
    ("ha" (org-insert-structure-template "alert") "alert")
    ("hl" (org-insert-structure-template "lead") "lead"))
   "Misc"
   (("m" (hot-expand "<s" "mermaid :file chart.png") "mermaid")
    ("u" (hot-expand "<s" "plantuml :file chart.png") "plantuml")
    ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
    ("G" (hot-expand "<s" "gnuplot :results output :file ./result.png") "gnuplot")
    ("<" self-insert-command "ins"))))

(require 'lib-transient)
(pretty-transient-define-prefix transient-org-toggles ()
  "Transient org menu."
  :transient-non-suffix 'transient--do-stay
  [["Display"
    :if (lambda ()
          (equal major-mode 'org-mode))
    ("l" "Display Link" org-toggle-link-display :toggle (lambda () (not org-link-descriptive)) :transient t)
    ("m" "Hide Emphasis Markers" org-toggle-display-emphasis-markers :toggle (lambda () org-hide-emphasis-markers) :transient t)
    ("e" "Display Pretty Entities" org-toggle-pretty-entities :toggle (lambda () org-pretty-entities) :transient t)
    ("i" "Display inline images" org-toggle-inline-images :toggle (lambda () org-inline-image-overlays) :transient t)
    ("v" "Toggle Valign" valign-mode :toggle t :transient t)
    ("s" "Toggle sliced image" org-sliced-images-mode :toggle t :transient t)]
   ["Org Management"
    :if (lambda ()
          (equal major-mode 'org-mode))
    ("p" "Set Property" org-set-property)
    ("E" "Export" org-export-dispatch)]]
  [["Only work for org-mode buffer"
    :if (lambda ()
          (not (equal major-mode 'org-mode)))
    ("q" "Quit" transient-quit-one)]
   [:if (lambda ()
          (equal major-mode 'org-mode))
        ("q" "Quit" transient-quit-one)]])

(pretty-transient-define-prefix transient-org-line-template ()
  "Transient org line menu."
  [["Link"
    ("r" "Ref" oxr-insert-ref )
    ("l" "Normal" org-insert-link)
    ("c" "Citre" org-cite-insert)
    ("d" "Denote" denote-insert-link)]
   ["Emphasize"
    ("=" "Verbatim" (lambda ()
                      (interactive)
                      (org-emphasize ?=)))
    ("~" "Code" (lambda ()
                  (interactive)
                  (org-emphasize ?~)))
    ("+" "Delete" (lambda ()
                    (interactive)
                    (org-emphasize ?+)))
    ("_" "Underline" (lambda ()
                       (interactive)
                       (org-emphasize ?_)))

    ("/" "Italic" (lambda ()
                    (interactive)
                    (org-emphasize ?/)))
    ("*" "Bold" (lambda ()
                  (interactive)
                  (org-emphasize ?*)))
    ("e" "Emphasize" org-emphasize)]
   ["Latex"
    ("i" "Inline math" (lambda ()
                         (interactive)
                         (insert "\\(  \\)")
                         (backward-char 3)))
    ("I" "Display math" (lambda ()
                          (interactive)
                          (insert "\\[  \\]")
                          (backward-char 3)))
    ("L" "Convert to latex" latex-math-from-calc :if region-active-p)]
   ["Misc"
    ("v" "add-file-local-variable" add-file-local-variable)
    (">" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

;;; keymap
(keymap-sets org-mode-map
  `(("C-c TAB" . org-insert-item-auto-checkbox)
    ("M-K" . org-metaup)
    ("M-J" . org-metadown)
    ("M-H" . org-metaleft)
    ("M-L" . org-metaright)
    (("M-RET" "s-<return>") . org-meta-return-auto)

    ("C-c C-'" . separedit/edit-org-any-block)

    ("M-g n" . org-next-visible-heading)
    ("M-g p" . org-previous-visible-heading)

    ("C-M-y" . org-rich-yank)
    ("M-g o" . consult-org-heading)

    ("<" . ,(lambda ()
              "Insert org template."
              (interactive)
              (if (or (region-active-p) (looking-back "^\s*" (line-beginning-position)))
                  (hydra-org-template/body)
                (self-insert-command 1))))
    (">" . transient-org-line-template)))

(keymap-unset org-mode-map "M-<left>")
(keymap-unset org-mode-map "M-<right>")
(keymap-unset org-mode-map "S-<return>")

(global-set-keys
 '(("C-c L" . org-store-link)
   ("C-c C-o" . org-open-at-point)
   ("M-s n" . consult-notes)
   ("M-s N" . consult-notes-search-in-all-notes)))

;;; Org capf
(defun my/org-capf ()
  (setq-local completion-at-point-functions
              `(cape-file
                pcomplete-completions-at-point
                cape-elisp-block
                ,(cape-capf-super
                  #'cape-dabbrev
                  #'cape-dict))))

(add-hook 'org-mode-hook #'my/org-capf)

;;; Org consult
(setq consult-notes-file-dir-sources
      '(("Org"             ?o "~/Documents/Org/")))
(setq consult-notes-org-headings-files
      '("~/Documents/Org/idea.org"
        "~/Documents/Org/quote.org"))

(consult-notes-org-headings-mode)
(consult-notes-org-roam-mode)

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
;; (require 'init-org-media-note)

;;; Local Variables

;; Local Variables:
;; eval: (when user/hidden-outline (outline-hide-sublevels 2))
;; End:

(provide 'init-org)
