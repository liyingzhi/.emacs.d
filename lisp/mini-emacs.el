;; A minimum .emacs config to test Emacs plugins
;; (show-paren-mode 1)
;; (eval-when-compile (require 'cl))

;; test elisps download from internet here
;; (setq test-elisp-dir "~/test-elisp/")
;; (unless (file-exists-p (expand-file-name test-elisp-dir))
;;     (make-directory (expand-file-name test-elisp-dir)))

;; (setq load-path
;;       (append
;;         (loop for dir in (directory-files test-elisp-dir)
;;               unless (string-match "^\\." dir)
;;               collecting (expand-file-name (concat test-elisp-dir dir)))
;;         load-path))

(repeat-mode)

;; 增加长行处理性能
;; (setq-default bidi-display-reordering nil)
;; (setq bidi-inhibit-bpa t)
;; (setq bidi-paragraph-direction 'left-to-right)

;; from doom emacs
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;;; performance
;; Disable garbage collection when entering commands.
(setq garbage-collection-messages t)	;gc时显示消息
(setq byte-compile-warnings nil)	;关闭字节编译警告

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (setq gc-cons-threshold 800000)))

;;; Imenu
(setq imenu-max-item-length 100)
(setq breadcrumb-imenu-max-length 100)



;;; History
(require 'recentf)
(setq recentf-max-saved-items 300
      recentf-exclude
      '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks" "bookmark-default"

        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
        "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/elfeed/"
        "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
        (lambda (file) (file-in-directory-p file package-user-dir))))
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(push (expand-file-name recentf-save-file) recentf-exclude)
(add-to-list 'recentf-filename-handlers #'abbreviate-file-name)

;;; savehist
(setq enable-recursive-minibuffers t  ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300)
(savehist-mode 1)
;;; saveplace
(setq save-place-forget-unreadable-files nil)

(setq-default fill-column 80
			  tab-width 4
			  indent-tabs-mode nil)
;;; vc
(setq vc-handled-backends '(Git))

(defun get-file-path ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))


;;;###autoload
(defun +lizqwer/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (car (last (file-name-split (get-file-path))))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
(defun +lizqwer/copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (get-file-path)))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))

 ;;;###autoload
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;;###autoload
(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))


;;; meow
(require 'meow)
(require 'init-project)
(require 'init-hydra)

(defun kill-now-buffer ()
  "Close the current buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defun match-in (pred lst)
  (catch 'found
    (dolist (x lst)
      (when (funcall pred x)
        (throw 'found t)))))

(defun my/meow-quit ()
  (interactive)
  (if (match-in #'(lambda (regex)
                    (buffer-match-p (if (symbolp regex)
                                        (cons 'derived-mode regex)
                                      regex)
                                    (buffer-name)))
                popper-reference-buffers)
      (popper--delete-popup (selected-window))
    (if (equal major-mode 'blink-search-mode)
        (blink-search-quit)
      (if (bound-and-true-p citre-peek--mode)
          (citre-peek-abort)
        ;; 检查非 popper 窗口数量
        (let* ((all-windows (window-list (selected-frame)))
               (non-popper-windows
                (seq-filter (lambda (win)
                              (let ((buf (window-buffer win)))
                                (not (match-in (lambda (regex)
                                               (buffer-match-p (if (symbolp regex)
                                                                   (cons 'derived-mode regex)
                                                                 regex)
                                                               (buffer-name buf)))
                                             popper-reference-buffers))))
                            all-windows)))
          (if (> (length non-popper-windows) 1)
              (delete-window)
            (meow-quit)))))))

(defun my/help-lisp ()
  (interactive)
  (when (or (equal major-mode 'emacs-lisp-mode)
           (equal major-mode 'lisp-interaction-mode))
    (helpful-at-point)))

(defun help-helpful-lsp-sly ()
  "Help function with lsp and sly info"
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-documentation)
    (if (or (equal major-mode 'emacs-lisp-mode)
           (equal major-mode 'lisp-interaction-mode))
        (helpful-at-point)
      (pcase user/lsp-client
        ('eglot
         (eldoc-box-help-at-point))))))

;;; comment line selection
(defun line-comment-p ()
  (save-excursion
    (back-to-indentation)
    (or (memq (get-text-property (point) 'face)
             '(font-lock-comment-face font-lock-comment-delimiter-face web-mode-comment-face)))))

(defun line-empty-p ()
  (string= (string-trim
            (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position)))
           ""))
(defun find-comment-area (forwardp)
  (let ((findp t)
        (first-find t)
        (first-comment-pos nil)
        (last-comment-pos nil))
    (while (and findp
              (not (if forwardp
                     (= (line-beginning-position) (point-min))
                   (= (line-end-position) (point-max)))))
      (if (line-comment-p)
          (progn
            (when first-find
              (setf first-find nil)
              (setf first-comment-pos
                    (if forwardp
                        (line-end-position)
                      (line-beginning-position))))
            (setf last-comment-pos
                  (if forwardp
                      (line-beginning-position)
                    (line-end-position))))
        (when (not (line-empty-p))
          (setf findp nil)))
      (if forwardp
          (previous-line)
        (next-line)))
    (list first-comment-pos
          last-comment-pos
          (and first-comment-pos
             last-comment-pos))))

(defun my/gn-key-function()
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively #'org-insert-todo-heading)
    (fingertip-jump-out-pair-and-newline)))

(defun mark-next-comment ()
  "Mark next line comment"
  (interactive)
  (when (or (line-comment-p)
           (line-empty-p))
    (let ((p (point)))
      (setq pre-comment-pos (find-comment-area nil))

      (if (and (cl-second pre-comment-pos)
             (cl-first pre-comment-pos))
          (progn
            (set-mark (cl-second pre-comment-pos))
            (goto-char (cl-first pre-comment-pos)))
        (progn
          (goto-char p)
          (message "pos: %s" pre-comment-pos))))))

(setq meow-esc-delay 0.001)
(setq meow-keypad-leader-dispatch "C-c")

(setq meow-mode-state-list
      '((fundamental-mode . normal)
        (text-mode . normal)
        (prog-mode . normal)
        (conf-mode . normal)
        (compilation-mode . normal)
        (helpful-mode . normal)
        (message-mode . normal)
        (messages-buffer-mode . normal)
        (Info-mode . motion)
        (help-mode . normal)
        (color-rg-mode . insert)
        (fanyi-mode . normal)))

(setq meow-use-clipboard t)
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
(meow-thing-register 'url 'url 'url)
(add-to-list 'meow-char-thing-table '(?u . url))

(defun meow-setup ()
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . comment-or-uncomment-region)
   '("s" . meow-delete)
   '("S" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   ;; '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . mark-next-comment)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . my/meow-quit)
   ;;   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("d" . meow-kill)
   '("D" . meow-kill-append)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;; '("v" . meow-visit)
   '("v" . meow-cancel-selection)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   ;; '("X" . meow-goto-line)
   '("y" . meow-save)
   ;; '("Y" . meow-sync-grab)
   '("Y" . meow-clipboard-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-normal-define-key
   '("C-y" . meow-clipboard-yank)
   '("Q" . kill-now-buffer)
   '("gf" . find-file-at-point)
   '("gp" . goto-percent)
   '("gl" . consult-goto-line)
   '("gL" . avy-goto-line)
   '("?" . help-helpful-lsp-sly))
  (meow-normal-define-key
   '("ge" . my/copy-select-utils-dispatch)
   '("gE" . one-key-menu-thing-edit)
   '("gG" . one-key-menu-mark-macro)
   '("gt" . one-key-menu-tool-kit)
   '("gn" . my/gn-key-function)
   '("gc" . my/string-case-cycle-auto)
   '("gC" . my/string-convert-dispatch)
   '("go" . casual-editkit-main-tmenu)

   )

  (meow-normal-define-key
   '("gr" . xref-find-references)
   '("gd" . xref-find-definitions)
   '("gD" . xref-find-definitions-other-window)
   '("gi" . eglot-find-implementation)
   '("gI" . eglot-find-implementation)
   '("C-o" . xref-go-back))

  (meow-leader-define-key
   '("b" . one-key-menu-buffer)
   '("f" . one-key-menu-file)
   '("s" . one-key-menu-search)
   '("p" . one-key-menu-project)
   '("v" . git-dispatch)
   '("z" . hydra-language/body)
   '("j" . one-key-menu-code)
   '("d" . one-key-menu-directory)
   '("o" . one-key-menu-org)
   '("a" . one-key-menu-agenda)
   '("w" . hydra-window/body)
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("3" . split-window-horizontally)
   '("0" . delete-window)
   '("?" . meow-cheatsheet)))


(meow-setup)
(meow-global-mode 1)

;; (with-current-buffer "*Messages*"
;;   (meow-normal-mode 1))

;; (add-hook #'after-find-file-hook  #'(lambda ()
;;                                       (meow-normal-mode 1)))
;; (add-hook #'after-init-hook  #'(lambda ()
;;                                  (meow-normal-mode 1)))


;;; vundo
(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;;; ace window
;; (require 'ace-window)
(global-set-key (kbd "M-y") #'consult-yank-pop)

;;; goto precent
;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

;;; global key settings
(defun scroll-up-1/3 ()
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(defun scroll-down-1/3 ()
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

;;;###autoload
(defmacro keymap-sets (key-map key-bindings)
  `(dolist (key-b ,key-bindings)
     (when-let* ((keys (car key-b))
                 (command (cdr key-b)))
       (if (listp keys)
           (dolist (key keys)
             (keymap-set ,key-map
                         key
                         command))
         (keymap-set ,key-map
                     keys
                     command)))))
;;;###autoload
(defun global-set-keys (key-bindings)
  (keymap-sets (current-global-map)
               key-bindings))

(require 'simple)
(global-set-keys
 '(("RET" . newline-and-indent)
   ("S-<return>" . comment-indent-new-line)
   (("s-o" "M-o") . ace-window)
   (("s-n" "M-n") . scroll-up-1/3)
   (("s-p" "M-p") . scroll-down-1/3)
   (("s-x" "M-x") . execute-extended-command)))

(global-set-keys
 '(("M-<left>" . previous-buffer)
   ("M-<right>" . next-buffer)))

(save-place-mode t)
(recentf-mode t)

;; (fido-vertical-mode)

;; (toggle-frame-fullscreen)
;; (toggle-frame-maximized)

;;; start emacs cmd
;; emacs -q -l ~/.emacs.d/mini-emacs.el

;;; install package items
;; no-littering
;; meow
;; one-key
;; lazy-load
;; ace-window
;; vundo
;; magit
;; helpful
(provide 'mini-emacs)
