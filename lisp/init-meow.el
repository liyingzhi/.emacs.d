;;; init-meow.el ---                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'meow)
;; (setq meow-expand-hint-remove-delay 5.0)
(setq meow-visit-sanitize-completion nil)
(setq meow-esc-delay 0.001)
(setq meow-keypad-meta-prefix nil)
(setq meow-keypad-leader-dispatch "C-c")
(setq meow-mode-state-list
      '((fundamental-mode . normal)
        (text-mode . normal)
        (prog-mode . normal)
        (conf-mode . normal)
        (elfeed-show-mode . normal)
        (helpful-mode . normal)
        (cargo-process-mode . normal)
        (compilation-mode . normal)
        (message-mode . normal)
        (messages-buffer-mode . normal)
        (blink-search-mode . insert)
        (color-rg-mode . insert)
        (rg-mode . insert)
        (Info-mode . motion)
        (help-mode . normal)
        (devdocs-mode . normal)
        (vterm-mode . normal)
        (fanyi-mode . normal)
        (eww-mode . normal)
        (difftastic-mode . motion)
        (biblio-selection-mode . motion)))

(when user/load-eaf
  (add-to-list 'meow-mode-state-list '(eaf-mode . motion)))
;; overwrite default j,k key function in motion state
;; (meow-motion-overwrite-define-key '("j" . dired-next-line))
;; (meow-motion-overwrite-define-key '("n" . "H-j"))
;; (meow-motion-overwrite-define-key '("k" . dired-previous-line))
;; (meow-motion-overwrite-define-key '("p" . "H-k"))

(setq meow-use-clipboard t)
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

(meow-thing-register 'url 'url 'url)
(meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))

(require 'mark-comment)
(meow-thing-register 'comment #'mark-comment-inner-of-comment #'mark-comment-inner-of-comment)

(defvar wrap-keymap
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (dolist (k '("(" "[" "{" "<"))
      (define-key map k #'insert-pair))
    map)
  "Keymap for wrap.")

(meow-normal-define-key (cons "\\" wrap-keymap))

(setq meow-char-thing-table '((?\( . round)
                              (?\) . round)
                              (?\g .  string)
                              (?\[ . square)
                              (?\] . square)
                              (?<  . angle)
                              (?>  . angle)
                              (?{  . curly)
                              (?}  . curly)
                              (?s  . symbol)
                              (?d  . defun)
                              (?w  . window)
                              (?l  . line)
                              (?b  . buffer)
                              (?p  . paragraph)
                              (?u . url)
                              (?c . comment)))

(defun lazy-meow-leader-define-key (&rest keybinds)
  (let* ((meow-leader-keybinds))
    (dolist (ele  keybinds)
      (let ((func (cdar ele))
            (key (caar ele))
            (filename (cadr ele)))
        (autoload func filename nil t)
        (meow-define-keys 'leader (cons key func))))))

(defun lazy-meow-insert-define-key (&rest keybinds)
  (let* ((meow-insert-keybinds))
    (dolist (ele  keybinds)
      (let ((func (cdar ele))
            (key (caar ele))
            (filename (cadr ele)))
        (autoload func filename nil t)
        (meow-define-keys 'insert (cons key func))))))

(defun help-helpful-lsp-sly ()
  "Help function with lsp and sly info."
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-documentation)
    (if (or (equal major-mode 'emacs-lisp-mode)
            (equal major-mode 'lisp-interaction-mode))
        (helpful-at-point)
      (pcase user/lsp-client
        ('eglot
         (eldoc-box-help-at-point))
        ('lsp-bridge
         (if (bound-and-true-p lsp-bridge-mode)
             (lsp-bridge-popup-documentation)
           (message "dont't know how to help")))))))

(defun meow-next-enhance ()
  (interactive)
  (if (and (fboundp #'scroll-up-one-line) (not user/move-style-motion))
      (call-interactively #'scroll-up-one-line)
    (call-interactively #'meow-next)))

(defun meow-prev-enhance ()
  (interactive)
  (if (and (fboundp #'scroll-up-one-line) (not user/move-style-motion))
      (call-interactively #'scroll-down-one-line)
    (call-interactively #'meow-prev)))

(defun my/meow-quit ()
  "Meow quit."
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

(pcase user/lsp-client
  ('eglot
   (keymap-sets goto-map
     '(("r" . xref-find-references)
       ("d" . xref-find-definitions)
       ("D" . xref-find-definitions-other-window)
       ("u" . eglot-find-implementation)))
   (defun go-back-and-recenter-top-bottom ()
     (interactive)
     (xref-go-back)
     (recenter-top-bottom)))
  ('lsp-bridge
   (keymap-sets goto-map
     '(("r" . lsp-bridge-find-references)
       ("d" . find-definition-with-lsp-bridge)
       ("D" . find-definition-with-lsp-bridge-other-window)
       ("u" . lsp-bridge-find-impl)
       ("U" . lsp-bridge-find-impl-other-window)))
   (defun go-back-and-recenter-top-bottom ()
     (interactive)
     (return-find-def)
     (recenter-top-bottom))))

(global-set-keys
 '(("C-o" . go-back-and-recenter-top-bottom)
   (("M-RET" "s-<return>" "S-<return>" "C-<return>") . my/new-next-item-function-byScene)))

;; meow while translate i into TAB
(keymap-unset goto-map "TAB")
(keymap-sets goto-map
  '(("f" . find-file-at-point)
    ("P" . goto-percent)
    ("l" . consult-goto-line)
    ("L" . avy-goto-line)
    ("o" . consult-outline)
    ("m" . consult-mark)
    ("k" . consult-global-mark)
    ("i" . consult-imenu)
    ("I" . consult-imenu-multi)
    ("b" . consult-bookmark)
    ("e" . my/copy-select-utils-dispatch)
    ("E" . one-key-menu-thing-edit)
    ("G" . one-key-menu-mark-macro)
    ("T" . one-key-menu-tool-kit)
    ("c" . my/string-case-cycle-auto)
    ("C" . my/string-convert-dispatch)
    ("O" . casual-editkit-main-tmenu)))

(defvar-keymap find-map
  :doc "Keymap for find commands."
  :prefix t
  "c" #'find-custom-file
  "i" #'find-init-file
  "l" #'find-library
  "v" #'find-variable)

(defun meow-setup ()
  "Meow keymap setup."
  (meow-leader-define-key
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-leader-define-key
   '("w" . hydra-window/body)
   '("u" . one-key-menu-useful)
   '("j" . one-key-menu-code)
   '("s" . one-key-menu-search)
   '("f" . one-key-menu-file)
   '("b" . one-key-menu-buffer)
   '("o" . one-key-menu-org)
   ;; '("v" . one-key-menu-sort-tab)
   '("v" . git-dispatch)
   ;; '("l" . one-key-menu-workspace)
   '("z" . hydra-language/body)
   '("d" . hydra-jump-dir/body)
   '("i" . one-key-menu-insert)
   '("a" . one-key-menu-agenda))

  (lazy-meow-leader-define-key
   '(("p" . project-dispatch) "init-project"))

  (when user/load-eaf
    (meow-leader-define-key
     '("e" . one-key-menu-eaf)))

  (meow-leader-define-key
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("3" . split-window-horizontally)
   '("0" . delete-window))

  (meow-define-keys 'insert
    '("C-c i" . one-key-menu-insert))

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
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   ;; '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next-enhance)
   '("J" . meow-next-expand)
   '("k" . meow-prev-enhance)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-visit)
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
   '("<escape>" . ignore))

  (meow-normal-define-key
   '("g" . "M-g")
   '("S" . "M-s")
   (cons "F" find-map))

  (meow-normal-define-key
   '("Q" . kill-buffer-and-window)
   '("?" . help-helpful-lsp-sly)
   '("/" . consult-ripgrep)))

(meow-vterm-enable)
(meow-setup)
(meow-global-mode 1)

(global-set-keys
 '(("C-y" . meow-clipboard-yank)))

(require 'meow-tree-sitter)

(dolist (bind '((?a . "class")
                (?f . "function")
                (?t . "test")
                (?y . "entry")
                (?, . "parameter")))
  (setq meow-char-thing-table (assoc-delete-all (car bind)
                                                meow-char-thing-table))
  (meow-tree-sitter-register-thing (car bind) (cdr bind)))

(require 'repeat-fu)
(setq repeat-fu-preset 'meow)
(add-hook 'meow-mode-hook
          #'(lambda ()
              (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
                (repeat-fu-mode)
                (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
                (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute))))

(provide 'init-meow)
;;; init-meow.el ends here
