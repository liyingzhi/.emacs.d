;;; init-minibuffer.el --- init minibuffer           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; minibuffer

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)

;;; Vertico
(require 'vertico)

(setq vertico-count 15)

(defun my/vertico-insert ()
  (interactive)
  (let* ((mb (minibuffer-contents-no-properties))
         (lc (if (string= mb "") mb (substring mb -1))))
    (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
          ((file-directory-p (vertico--candidate)) (vertico-insert))
          (t (self-insert-command 1 ?/)))))

;; Configure the display per command.
;; Use a buffer with indices for imenu
;; and a flat (Ido-like) menu for M-x.
(setopt vertico-multiform-commands
        '(;; (consult-imenu buffer indexed)
          ))

;; Configure the display per completion category.
;; Use the grid display for files and a buffer
;; for the consult-grep commands.
(setq vertico-multiform-categories
      '(;; (file grid)
        ;; (project-file grid)
        (consult-grep buffer)))

;; highlight
(defvar +vertico-transform-functions nil)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))

(defun +vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
    file))

;; function to highlight enabled modes similar to counsel-M-x
(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
        (propertize cmd 'face 'font-lock-constant-face)
      cmd)))

;; add-to-list works if 'file isn't already in the alist
;; setq can be used but will overwrite all existing values
;; (add-to-list 'vertico-multiform-categories
;;              '(file
;;                ;; this is also defined in the wiki, uncomment if used
;;                ;; (vertico-sort-function . vertico-sort-directories-first)
;;                (+vertico-transform-functions . +vertico-highlight-directory)))
(add-to-list 'vertico-multiform-commands
             '(execute-extended-command
               (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

(defcustom vertico-show-arrow t
  "Vertico arrow."
  :group 'vertico
  :type 'boolean)

(defvar +vertico-current-arrow t)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                 (not (bound-and-true-p vertico-flat-mode)))
                                            (eql t)))
  (setq cand (cl-call-next-method cand prefix suffix index start))
  (let ((arrow (propertize (if (char-displayable-p ?») "» " "> ") 'face 'font-lock-keyword-face)))
    (if (and (not (bound-and-true-p vertico-grid-mode))
             vertico-show-arrow)
        (if (= vertico--index index)
            (concat arrow cand)
          (concat "  " cand))
      cand)))

;; Configure directory extension.
(keymap-sets vertico-map
  '(("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    (("M-DEL" "s-<backspace>") . vertico-directory-delete-word)
    (("M-RET" "s-<return>") . vertico-exit-input)
    ("C-<backspace>" . delete-backward-char)
    ("C-j" . vertico-exit-input)
    ("/" . my/vertico-insert)))

(add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(vertico-mode 1)

;; Enable vertico-multiform
(vertico-multiform-mode)

;;; marginalia
(marginalia-mode)
(add-hook 'marginalia-mode-hook
          #'nerd-icons-completion-marginalia-setup)
(nerd-icons-completion-mode)

;;; consult
(require 'consult)

;; (dolist (src consult-buffer-sources)
;;   (unless (or (eq src 'consult--source-buffer) (string-match "denote" (symbol-name src) ))
;;     (set src (plist-put (symbol-value src) :hidden t))))

;; use narrow
(setq consult-narrow-key "<")
;; not auto preview
(setq consult-preview-key "C-o")

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(defun buffer-list-filter ()
  "Get buffer list with filter."
  (let ((buffers (buffer-list))
        (res))
    (dolist (buffer buffers)
      (unless (string-match-p "*help" (buffer-name buffer))
        (push buffer res)))
    res))

(setq consult-buffer-list-function #'buffer-list-filter)

(defun consult-fd-dir ()
  (interactive)
  (let ((consult-fd-args (append consult-fd-args
                                 (list
                                  "--type directory"))))
    (consult-fd "~/")))

(defun consult-buffer-with-target (target &optional sources)
  "Enhanced `switch-to-buffer' command with support for virtual buffers.

TARGET is consult buffer target place.

The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources."
  (interactive)
  (let ((selected (consult--multi (or sources consult-buffer-sources)
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt (format "Switch to in other %s: "
                                                  (if target
                                                      (symbol-name target)
                                                    ""))
                                  :history 'consult--buffer-history
                                  :sort nil)))
    ;; For non-matching candidates, fall back to buffer creation.
    (unless (plist-get (cdr selected) :match)
      (consult--buffer-action (car selected)))))

(defun consult-buffer-other-window ()
  "Variant of `consult-buffer', switching to a buffer in another window."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-buffer-with-target 'window)))

;; for orderless
(defun consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (cdr (orderless-compile input)))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str) (orderless--highlight input t str))))

(setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

(defun define-minibuffer-key (key &rest defs)
  "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
  (define-key minibuffer-local-map key
              (list 'menu-item nil defs :filter
                    (lambda (d)
                      (plist-get d (completion-metadata-get
                                    (completion-metadata (minibuffer-contents)
                                                         minibuffer-completion-table
                                                         minibuffer-completion-predicate)
                                    'category))))))

(defun consult-find-for-minibuffer ()
  "Search file with find, enter the result in the minibuffer."
  (interactive)
  (pcase-let* ((enable-recursive-minibuffers t)
               (default-directory (file-name-directory (minibuffer-contents)))
               (`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" default-directory))
               (default-directory dir)
               (builder (consult--fd-make-builder paths))
               (file (consult--find prompt builder (file-name-nondirectory (minibuffer-contents)))))
    (delete-minibuffer-contents)
    (insert (expand-file-name file default-directory))
    (exit-minibuffer)))

(define-minibuffer-key "\C-s"
                       'file #'consult-find-for-minibuffer)

;;; consult dir
(require 'consult-dir)

;; (setq consult-dir-default-command #'consult-dir-dired)

(defun consult-dir--zoxide-dirs ()
  "Return list of zoxide dirs."
  (split-string (shell-command-to-string "zoxide query -l") "\n" t))

(defvar consult-dir--source-zoxide
  `(:name "zoxide"
          :narrow ?z
          :category file
          :face consult-file
          :history file-name-history
          :enabled ,(lambda () (executable-find "zoxide"))
          :items ,#'consult-dir--zoxide-dirs)
  "zoxide directory source for `consult-dir'.")

(add-to-list 'consult-dir-sources 'consult-dir--source-zoxide)

;; Moving ahead consult-dir--source-recentf
(let ((target 'consult-dir--source-recentf)
      (sources consult-dir-sources))
  (setq consult-dir-sources
        (cons target (remove target sources))))

;; A function that returns a list of directories
(defun consult-dir--quick-dir ()
  "Return list of fasd dirs."
  user/quickdir)

;; A consult source that calls this function
(defvar consult-dir--source-quick
  `(
    :name     "quick"
    :narrow   ?q
    :category file
    :face     consult-file
    :history  file-name-history
    ;; :enabled  t
    :items    ,#'consult-dir--quick-dir)
  "Fasd directory source for `consult-dir'.")

;; Adding to the list of consult-dir sources
(add-to-list 'consult-dir-sources 'consult-dir--source-quick)

(global-set-keys
 '(("C-x C-d" . consult-dir)))

;;; consult color
(autoload #'consult-colors-web "consult-colors" nil t)
(autoload #'consult-colors-emacs "consult-colors" nil t)

;;; bufferlo

;; modeline
(with-eval-after-load 'bufferlo
  (setq bufferlo-mode-line-prefix "🐃") ; "🐮"
  (setq bufferlo-mode-line-set-active-prefix "Ⓢ")
  (setq bufferlo-mode-line-frame-prefix "Ⓕ")
  (setq bufferlo-mode-line-tab-prefix "Ⓣ")
  (setq bufferlo-mode-line-left-prefix nil)
  (setq bufferlo-mode-line-right-suffix nil))

;; with consult
(defvar my:bufferlo-consult--source-local-buffers
  (list :name "Bufferlo Local Buffers"
        :narrow   ?l
        :category 'buffer
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :default  t
        :items    (lambda () (consult--buffer-query
                              :predicate #'bufferlo-local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))
  "Local Bufferlo buffer candidate source for `consult-buffer'.")

(defvar my:bufferlo-consult--source-other-buffers
  (list :name "Bufferlo Other Buffers"
        :narrow   ?o
        :category 'buffer
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :items    (lambda () (consult--buffer-query
                              :predicate #'bufferlo-non-local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))
  "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

(defvar my:bufferlo-consult--source-all-buffers
  (list :name "Bufferlo All Buffers"
        :narrow   ?a
        :hidden   t
        :category 'buffer
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :items    (lambda () (consult--buffer-query
                              :sort 'visibility
                              :as #'buffer-name)))
  "All Bufferlo buffer candidate source for `consult-buffer'.")

;; add in the reverse order of display preference
(add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-all-buffers)
(add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
(add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers)

(with-eval-after-load 'consult
  (delq 'consult--source-buffer consult-buffer-sources))

;; with tab bar
(setopt tab-bar-new-tab-choice "*scratch*"
        bufferlo-bookmark-tab-replace-policy 'new)


(global-bind-keys
 ("C-c l L" . ("Load save tab" . bufferlo-bookmark-tab-load))
 ("C-c l K" . ("Kill save tab" . bufferlo-bookmark-close-current))
 ("C-c l S" . ("Store save tab" . bufferlo-bookmark-tab-save-current))

 ("C-c ;" . ("Switch scratch" . bufferlo-switch-to-scratch-buffer)))

(bufferlo-mode)
(bufferlo-anywhere-mode)

;;; embark

;; embark with which-key
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(add-hook 'embark-collect-mode
          #'consult-preview-at-point-mode)

(defun my/embark-duckduckgo-search (term)
  "Embark search TERM in duckduckgo."
  (interactive "sSearch Term: ")
  (browse-url
   (format "http://duckduckgo.com/?q=%s" term)))

(with-eval-after-load 'embark
  (setq embark-cycle-key "SPC")

  (keymap-binds embark-general-map
    ("G" . my/embark-duckduckgo-search))

  (keymap-sets embark-library-map
    '(("b" . straight-visit-package-website)
      ("v" . straight-visit-package)))
  (keymap-sets embark-buffer-map
    '(("l" . eval-buffer)
      ("B" . switch-to-buffer-other-window)))
  (keymap-sets embark-file-map
    '(("S" . sudo-edit-find-file)
      ("F" . find-file-other-window)))
  (keymap-sets embark-bookmark-map
    '(("B" . bookmark-jump-other-window))))

(global-set-keys
 '(("C-s-;" . embark-dwim)
   ("C-M-;" . embark-dwim)
   ("M-;" . embark-dwim)
   ("M-." . embark-act)
   ("C-h B" . embark-bindings)))

;;; Key-Binding
(keymap-sets minibuffer-local-map
  '(("M-s" . consult-history)
    ("M-r" . consult-history)
    ("C-i" . (lambda ()
               "Insert the current symbol."
               (interactive)
               (insert (save-excursion
                         (set-buffer (window-buffer (minibuffer-selected-window)))
                         (or (thing-at-point 'symbol t) "")))))
    ("C-x C-d" . consult-dir)
    ("C-x C-j" . consult-dir-jump-file)
    ("C-c C-c" . embark-collect)
    ("C-c C-e" . embark-export)))

;;; Local Variables

;; Local Variables:
;; eval: (when user/hidden-outline (outline-hide-sublevels 2))
;; End:

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
