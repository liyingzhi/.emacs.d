
;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

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
   ("e" "Mark from point to endline" my/select-end-of-current-line-to-point)
   ("G" "Mark from point to endbuffer" my/select-end-of-buffer-to-point)
   ("c" "Yank name of current buffer" my/copy-current-buffer-name)])

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

(setq my-string-case-cycle-auto--Cnt 0)

(defun my/string-case-cycle-auto ()
  "string-case-cycle-auto"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq my-string-case-cycle-auto--Cnt 0)
    (setq my-string-case-cycle-auto--Cnt 1)
    (call-interactively #'downcase-dwim))
   ;; for python
   ((eq my-string-case-cycle-auto--Cnt 1)
    (setq my-string-case-cycle-auto--Cnt 2)
    (call-interactively #'capitalize-dwim))
   ;; for java
   ((eq my-string-case-cycle-auto--Cnt 2)
    (setq my-string-case-cycle-auto--Cnt 0)
    (call-interactively #'upcase-dwim))
   (t
    ;; default
    (setq my-string-case-cycle-auto--Cnt 0)
    (call-interactively #'downcase-dwim))))

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
  (setq my-string-case-cycle-auto--Cnt 0)
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

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)

(advice-add #'meow-grab
            :before
            #'(lambda ()
                (call-interactively #'electric-pair-mode)))

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

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
      lazy-count-prefix-format "%s/%s ")

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
    (call-interactively #'isearch-forward-symbol-at-point))
  (keymap-sets isearch-mode-map
               '(("<escape>" . isearch-exit)
                 ("C-d" . my/isearch-forward-symbol-at-point)
                 ("C-l" . my-isearch-consult-line-from-isearch)
                 ("C-o" . my-occur-from-isearch))))

(add-hooks '(emacs-lisp-mode lisp-mode)
           #'aggressive-indent-mode )

;;; indent yank
(require 'indent-yank)
(add-hook 'prog-mode-hook
          #'indent-yank-mode)

;;; Outline indent
(require 'init-outline-indent)



(provide 'init-edit)
