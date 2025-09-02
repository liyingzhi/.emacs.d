;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)

(advice-add #'meow-grab
            :before
            #'(lambda ()
                (call-interactively #'electric-pair-mode)
                (call-interactively #'fingertip-mode)))

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

;;; fingertip
(require 'init-fingertip)

;;; vundo
(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;; like replace-regexp with live visual feedback in the buffer
(require 'visual-regexp)
;; extension to visual-regexp which enables the use of modern regexp engines
(require 'visual-regexp-steroids)

;;; Visual Replace
(require 'visual-replace)
(global-set-key (kbd "C-c C-r") #'visual-replace)
(keymap-sets visual-replace-mode-map
  '(("C-r" . visual-replace-toggle-regexp)
    ("C-q" . visual-replace-toggle-query)
    ("C-w" . visual-replace-toggle-word)
    ("C-f" . visual-replace-toggle-case-fold)
    ("C-u" . visual-replace-undo)
    ("C-s" . visual-replace-toggle-scope)))
(setq visual-replace-min-length 1)

(keymap-sets isearch-mode-map
  '(("C-c C-r" . visual-replace-from-isearch)))

(transient-define-prefix visual-replace-dispatch ()
  "Visual replace menu."
  ["Toggles"
   ("r" "Regexp" visual-replace-toggle-regexp)
   ("s" "Scope" visual-replace-toggle-scope)
   ("q" "Query" visual-replace-toggle-query)
   ("w" "Word" visual-replace-toggle-word)
   ("c" "Case Fold" visual-replace-toggle-case-fold)
   ("l" "Lax ws" visual-replace-toggle-lax-ws)])

(keymap-set visual-replace-mode-map
            "C-o" #'visual-replace-dispatch)

(visual-replace-global-mode 1)

;;; isearch
(setq isearch-lazy-count t
      isearch-case-fold-search t
      lazy-count-prefix-format "%s/%s "
      search-whitespace-regexp ".*?")

(defun my-isearch-consult-line-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (consult-line query)))

(defun my-occur-from-isearch ()
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (occur query)))

(with-eval-after-load 'isearch
  (defun my/isearch-forward-symbol-at-point ()
    "Search for symbol at point and ensure escape exits isearch."
    (interactive)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (isearch-forward-symbol-at-point))

  (keymap-sets isearch-mode-map
    '(("<escape>" . isearch-exit)
      ("C-d" . my/isearch-forward-symbol-at-point)
      ("C-l" . my-isearch-consult-line-from-isearch)
      ("C-o" . my-occur-from-isearch)))

  ;; (with-eval-after-load 'casual-isearch
  ;;   (transient-define-suffix isearch-consult-line ()
  ;;     (interactive)
  ;;     (call-interactively #'my-isearch-consult-line-from-isearch))
  ;;   (transient-append-suffix 'casual-isearch-tmenu "u"
  ;;     '("c" "Use consult line" isearch-consult-line)))
  )

;;; nxml
(with-eval-after-load 'nxml-mode
  (keymap-sets nxml-mode-map
    '(("C-c C-f" . nxml-down-element)
      ("C-c C-n" . nxml-forward-element)
      ("C-c C-p" . nxml-backward-element)
      ("C-c C-b" . nxml-backward-up-element))))

;;; aggressive-indent
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'aggressive-indent-mode )

;;; indent yank
(require 'indent-yank)
(add-hook 'prog-mode-hook
          #'indent-yank-mode)

;;; Outline indent
(require 'init-outline-indent)

;;; Auto rename tag
(add-hooks '(html-mode web-mode)
           #'auto-rename-tag-mode)

;;; insert trailing semi
(defun insert-or-remove-trailing-char (&optional ch)
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (if (eq (char-before) ch)
                  (delete-backward-char 1)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (next-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-or-remove-trailing-semi ()
  (interactive)
  (insert-or-remove-trailing-char ?\;))

(defun insert-or-remove-trailing-comma ()
  (interactive)
  (insert-or-remove-trailing-char ?,))

(defun insert-trailing-char (&optional ch)
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (unless (eq (char-before) ch)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (next-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-trailing-semi ()
  (interactive)
  (insert-trailing-char ?\;))

(defun insert-trailing-semi-and-indent ()
  (interactive)
  (insert-trailing-char ?\;)
  (forward-char)
  (newline-and-indent))

;;; edit buffer content
(defun my/copy-current-line ()
  "Copy the current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (line-beginning-position)
                      (line-end-position)))))

(defun my/copy-from-point-to-end-of-current-line ()
  "Copy the content from point to the end of current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (point)
                      (line-end-position)))))

(defun my/select-end-of-buffer-to-point ()
  "Select contents from the end of the buffer to the current point."
  (interactive)
  (push-mark (point-max) t t)
  (goto-char (point)))

(defun my/select-end-of-current-line-to-point ()
  "Select contents from the end of the current line to the current point."
  (interactive)
  (push-mark (line-end-position) t t)
  (goto-char (point)))

(defun my/copy-current-buffer-name ()
  "将当前缓冲区的名称复制到 kill-ring 中。"
  (interactive)
  (let ((name (buffer-name)))
    (when name
      (kill-new name)
      (message "缓冲区名称 \"%s\" 已复制到 kill-ring。" name))))

(transient-define-prefix my/copy-select-utils-dispatch ()
  "Select and copy content menu."
  ["Utils"
   ("y" "Yank from point to endline" my/copy-from-point-to-end-of-current-line)
   ("Y" "Yank current line" my/copy-current-line)
   ("q" "Mark from point to endline" my/select-end-of-current-line-to-point)
   ("w" "Mark from point to endbuffer" my/select-end-of-buffer-to-point)
   ("c" "Yank name of current buffer" my/copy-current-buffer-name)
   ("r" "visual-replace" visual-replace)])

;;; string inflection
(defun my/string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    ;; "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(defun my/string-case-cycle-auto ()
  "Intelligently cycle through word case formats:
- If all lowercase → convert to capitalized
- If capitalized → convert to all uppercase
- If all uppercase → convert to all lowercase
Moves cursor to word start (virtually, without affecting actual position) before conversion."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (cond
     ((null word) (user-error "No word at point"))
     (t
      (save-excursion
        (goto-char (car bounds))
        (cond
         ((string-equal word (downcase word))   ; All lowercase → Capitalized
          (call-interactively #'capitalize-dwim))
         ((string-equal word (capitalize word)) ; Capitalized → All uppercase
          (call-interactively #'upcase-dwim))
         ((string-equal word (upcase word))     ; All uppercase → All lowercase
          (call-interactively #'downcase-dwim))
         (t (call-interactively #'downcase-dwim))))))))

(defun my/string-customize-convert-with-parameter (&optional args)
  "string customize convert with parameter"
  (interactive (list (transient-args 'my/string-convert-dispatch)))
  (cond ((member "--styleCamel=foo_bar" args)
         (string-inflection-underscore))
        ((member "--styleCamel=FOO_BAR" args)
         (string-inflection-upcase))
        ((member "--styleCamel=Foo_Bar" args)
         (string-inflection-capital-underscore))
        ((member "--styleCamel=foo-bar" args)
         (string-inflection-kebab-case))
        ((member "--styleCamel=FooBar" args)
         (string-inflection-camelcase))
        ((member "--styleCamel=fooBar" args)
         (string-inflection-lower-camelcase))
        ((member "--styleCase=foobar" args)
         (call-interactively #'downcase-dwim))
        ((member "--styleCase=Foobar" args)
         (call-interactively #'capitalize-dwim))
        ((member "--styleCase=FOOBAR" args)
         (call-interactively #'upcase-dwim))))

(transient-define-prefix my/string-convert-dispatch ()
  "Sting convertion menu"

  ["Customize Arguments & Convert"
   ("-c " "UpDownCase" "--styleCase="
    :choices ("foobar" "FOOBAR" "Foobar"))
   ("-s " "Camel style" "--styleCamel="
    :choices ("foo_bar" "FOO_BAR" "Foo_Bar" "foo-bar" "FooBar" "fooBar"))
   ("RET" "Customize Convert" my/string-customize-convert-with-parameter)]

  [:description
   my-string--cycle-description
   ("s" "string-inflection-cycle-auto" my/string-inflection-cycle-auto :transient t)
   ("c" "string-case-cycle-auto [foobar => FOOBAR => Foobar]" my/string-case-cycle-auto :transient t)]

  ["Fast Convert"
   ("C" "capitalize-dwim" capitalize-dwim)
   ("U" "upcase-dwim" upcase-dwim)
   ("D" "downcase-dwim" downcase-dwim)
   ("u" "subword-upcase" subword-upcase)
   ("d" "subword-downcase" subword-downcase)]

  [("q" "Quit"           transient-quit-one)])

(defun my-string--cycle-description ()
  "Return a Transient menu headline to indicate the currently selected project."
  (format (propertize "Cycle Style: \n [%s %s \n %s %s]"
                      'face 'transient-heading)
          (propertize "string-inflection-cycle-auto:"
                      'face 'transient-inapt-suffix)
          (cond
           ;; for emacs-lisp-mode
           ((eq major-mode 'emacs-lisp-mode)
            ;; "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
            (propertize "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
                        'face 'transient-inapt-suffix))
           ;; for python
           ((eq major-mode 'python-mode)
            (propertize "foo_bar => FOO_BAR => FooBar => foo_bar"
                        'face 'transient-inapt-suffix))
           ;; for java
           ((eq major-mode 'java-mode)
            (propertize "fooBar => FOO_BAR => FooBar => fooBar"
                        'face 'transient-inapt-suffix))
           ;; for elixir
           ((eq major-mode 'elixir-mode)
            (propertize "foo_bar => FooBar => foo_bar"
                        'face 'transient-inapt-suffix))
           (t
            ;; default
            (propertize "foo_bar => FOO_BAR => FooBar => foo_bar"
                        'face 'transient-inapt-suffix)))
          (propertize "string-case-cycle-auto:"
                      'face 'transient-inapt-suffix)
          (propertize "foobar => FOOBAR => Foobar"
                      'face 'transient-inapt-suffix)))

;;; grugru
(grugru-default-setup)

(grugru-define-multiple
  ((c-ts-mode c++-ts-mode)
   (non-alphabet "&&" "||")
   (non-alphabet "&" "|")
   (non-alphabet "+" "-")
   (non-alphabet "*" "/" "%")
   (non-alphabet ">>" "<<")
   (non-alphabet "+=" "-=")
   (non-alphabet "*=" "/=" "%=")
   (non-alphabet "&=" "|=" "^=")
   (non-alphabet ">>=" "<<=")
   (non-alphabet "++" "--")
   (non-alphabet "==" "!=")
   (non-alphabet "<" "<=" ">" ">=")
   (symbol "break" "continue")
   (symbol "signed" "unsigned"))
  (c++-ts-mode
   (symbol "true" "false")
   (symbol "vector" "array" "deque")
   (symbol "class" "struct")
   (symbol "float" "double")
   (symbol "private" "public" "protected")
   (symbol "pair" "tuple")
   (symbol "static_cast" "dynamic_cast" "reinterpret_cast" "const_cast"))
  ((python-mode python-ts-mode)
   (symbol "True" "False"))
  ((emacs-lisp-mode lisp-mode)
   (symbol "when-let" "if-let")))

(keymap-sets prog-mode-map
  '(("C-;" . grugru)))

;;; add rectangle number lines
;;;###autoload
(defun my/insert-number-lines (start-at end-at step format)
  (interactive
   (list (read-number "Number to count from: " 1)
         (read-number "Number to count end: " 5)
         (read-number "step: " 1)
         (read-string "Format string: "
                      "%d ")))
  (save-excursion
    (dolist (i (number-sequence start-at end-at step))
      (insert (format format i))
      (newline-and-indent))))

;;; goto precent
;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun scroll-up-1/3 ()
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(defun scroll-down-1/3 ()
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun scroll-other-window-up-1/3 ()
  (interactive)
  (scroll-other-window (/ (window-body-height) 3)))

(defun scroll-other-window-down-1/3 ()
  (interactive)
  (scroll-other-window-down (/ (window-body-height) 3)))

;;; insert something
(defun +lizqwer/insert-file-path (filename)
  (interactive "*fInsert file path: \n")
  (insert (expand-file-name filename)))

(defun +lizqwer/insert-file-path-abbrev (filename)
  (interactive "*fInsert file path: \n")
  (insert (abbreviate-file-name (expand-file-name filename))))

(defun +lizqwer/insert-file-path-relative (filename)
  (interactive "*fInsert file path: \n")
  (insert (file-relative-name filename)))

(defun +lizqwer/insert-file-name (filename)
  (interactive "fInsert file name: \n")
  (insert (file-name-nondirectory filename)))

(pretty-hydra-define-e hydra-insert-file
  (:title "Insert file or path" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("path"
   (("f" +lizqwer/insert-file-path "full")
    ("r" +lizqwer/insert-file-path-relative "relative")
    ("a" +lizqwer/insert-file-path-abbrev "abbrev"))
   "name"
   (("n" +lizqwer/insert-file-name "name"))))

(one-key-create-menu
 "Insert-file-path"
 '((("f" . "insert full path") . +lizqwer/insert-file-path)
   (("r" . "insert relative path") . +lizqwer/insert-file-path-relative)
   (("a" . "insert abbrev path") . +lizqwer/insert-file-path-abbrev)))

(one-key-create-menu
 "Insert-file"
 '((("p" . "insert file path") . one-key-menu-insert-file-path)
   (("n" . "insert file name") . +lizqwer/insert-file-name)))

;;;###autoload
(defun toggle-sub-word-or-super-word ()
  (interactive)
  (if subword-mode
      (progn
        (superword-mode)
        (message "开启 super-word-mode"))
    (subword-mode)
    (message "开启 sub-word-mode")))

(provide 'init-edit)
;;; init-edit.el ends here.
