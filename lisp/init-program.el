;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; outli
(setq outli-allow-indented-headlines t)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (eq major-mode 'beancount-mode)
              (outli-mode))))

;;; outline
(with-eval-after-load 'outline
  (meow-normal-define-key
   (cons "@" outline-mode-prefix-map)))

;;; Tree-Sitter
(require 'treesit)
(customize-set-variable 'treesit-font-lock-level 4)

(treesit-font-lock-recompute-features
 '(command string variable function operator bracket keyword))

(add-list-to-list 'major-mode-remap-alist
                  '((sh-mode . bash-ts-mode)
                    (rust-mode . rust-ts-mode)
                    (python-mode . python-ts-mode)
                    (c++-mode . c++-ts-mode)
                    (c-mode . c-ts-mode)
                    (go-mode . go-ts-mode)
                    (csharp-mode . csharp-ts-mode)
                    (conf-toml-mode . toml-ts-mode)
                    (js-json-mode . json-ts-mode)))

;;; project
(add-hook 'after-init-hook 'global-projection-hook-mode)

(defun run-project-compile ()
  "Run project compile."
  (interactive)
  (autoload 'project-root-path "init-project" nil t)
  (if-let* ((project-path (project-root-path)))
      (call-interactively #'projection-commands-build-project)
    (call-interactively #'compile)))

(keymap-sets prog-mode-map
  '(("C-c r" . run-project-compile)))



;;; Xref
(setq xref-show-xrefs-function 'consult-xref)
(setq xref-show-definitions-function 'consult-xref)

;; Use faster search tool
(when (executable-find "rg")
  (setq xref-search-program 'ripgrep))

;;; check error
(pcase user/lsp-client
  ('eglot
   (defun set-diagnostic-check (symbol value)
     "Set SYMBOL to VALUE and update check."
     (when (bound-and-true-p global-flycheck-mode)
       (global-flycheck-mode nil))
     (remove-hook 'prog-mode-hook
                  #'flymake-mode)
     (set-default-toplevel-value symbol value)
     (pcase value
       ('flymake
        (wait-packages! '(flymake-popon))
        ;; Check elisp with `load-path'
        (defun my-elisp-flymake-byte-compile (fn &rest args)
          "Wrapper for `elisp-flymake-byte-compile'."
          (let ((elisp-flymake-byte-compile-load-path
                 (append elisp-flymake-byte-compile-load-path load-path)))
            (apply fn args)))
        (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile)

        (setq flymake-no-changes-timeout nil
              flymake-fringe-indicator-position 'right-fringe
              flymake-margin-indicator-position 'right-margin)

        (setq flymake-indicator-type 'margins
              flymake-margin-indicators-string
              `((error "!" compilation-error) ;; Alternatives: », E, W, i, !, ?)
                (warning "?" compilation-warning)
                (note "i" compilation-info)))

        (add-hook 'prog-mode-hook
                  #'flymake-mode)

        ;; flymake popon
        (setq flymake-popon-width 80)

        (custom-set-faces
         '(flymake-popon ((t :inherit default :height 0.85)))
         `(flymake-popon-posframe-border ((t :foreground ,(face-background 'posframe-border nil t)))))

        (add-hook #'flymake-mode-hook
                  #'flymake-popon-mode))
       ('flycheck
        (wait-packages! '(flycheck consult-flycheck flycheck-eglot flycheck-package))
        ;; flycheck
        (require 'flycheck)
        (setq flycheck-emacs-lisp-load-path 'inherit)
        (global-flycheck-mode 1)

        (require 'flycheck-eglot)
        (setq-default flycheck-eglot-exclusive nil)
        (global-flycheck-eglot-mode 1))))

   (defcustom user/diagnostic 'flymake
     "Diagnostic."
     :group 'user
     :type '(choice (const :tag "flymake" flymake)
                    (const :tag "flycheck" flycheck))
     :set #'set-diagnostic-check)

   ;; flyover
   (when user/flyoverp
     (require 'flyover)
     (pcase user/diagnostic
       ('flymake
        (add-hook 'flymake-mode-hook #'flyover-mode))
       ('flycheck
        (add-hook 'flycheck-mode-hook #'flyover-mode)))
     (setq flyover-use-theme-colors t
           flyover-checkers `(,user/diagnostic)
           flyover-show-at-eol t
           flyover-virtual-line-icon (concat (nerd-icons-faicon "nf-fa-arrow_right_long")
                                             " ")
           flyover-virtual-line-type nil))))

;;; debug
(require 'init-dap)

;;; eldoc
(with-eval-after-load 'eldoc
  (defun my/eldoc-box-or-other-window-scroll-down ()
    "If eldoc-box child frame exist and visible, scroll down within the child frame.
  Otherwise scroll down other window."
    (interactive)
    (if (and (bound-and-true-p eldoc-box--frame) (frame-visible-p eldoc-box--frame))
        (eldoc-box-scroll-down 3)
      (scroll-other-window-down-1/3)))

  (defun my/eldoc-box-or-other-window-scroll-up ()
    "If eldoc-box child frame exist and visible, scroll up within the child frame.
  Otherwise scroll up other window."
    (interactive)
    (if (and (bound-and-true-p eldoc-box--frame) (frame-visible-p eldoc-box--frame))
        (eldoc-box-scroll-up 3)
      (scroll-other-window-up-1/3)))

  (when (childframe-workable-p)
    (require 'eldoc-box)
    (setq eldoc-box-lighter nil
          eldoc-box-only-multi-line t
          eldoc-box-clear-with-C-g t)
    (setq eldoc-echo-area-use-multiline-p nil)

    (custom-set-faces
     '(eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
     '(eldoc-box-body ((t (:inherit tooltip)))))

    ;; (add-hook 'eglot-managed-mode-hook
    ;;           #'eldoc-box-hover-at-point-mode)

    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)

    (when (equal user/lsp-client 'eglot)
      (global-set-keys
       '(("C-h ?" . eldoc-box-help-at-point))))

    (with-eval-after-load 'eglot
      (keymap-sets eglot-mode-map
        '((("M-N" "s-N") . my/eldoc-box-or-other-window-scroll-up)
          (("M-P" "s-P") . my/eldoc-box-or-other-window-scroll-down))))))

;;; complile
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error nil)
(setq compilation-max-output-line-length nil)

(defun get-first-compilation-error ()
  (when (compilation-buffer-p (current-buffer))
    (compilation--ensure-parse (point-min))
    (save-excursion
      (goto-char (point-min))
      (condition-case err
          (progn
            (compilation-next-error 1)
            (> (point)
               (point-min)))
        (error
         nil)))))

(defun ar/compile-autoclose-or-jump-first-error (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (with-current-buffer buffer
    (when (eq major-mode 'compilation-mode)
      (if (or (string-match "^.*warning.*" string)
              (get-first-compilation-error)
              (string-match ".*exited abnormally.*" string))
          (progn
            (message "Compilation %s" string)
            (goto-char (point-min))
            (call-interactively #'compilation-next-error)
            (when (or (not (get-buffer-window buffer 'visible))
                      (not (frame-focus-state)))
              (knockknock-notify :title "Build Failed"
                                 :message "Compilation failed"
                                 :icon "cod-error"
                                 :duration 5)))
        (message "Build finished :)")
        (run-with-timer 1 nil
                        (lambda ()
                          (when-let* ((multi-window (> (count-windows) 1))
                                      (live (buffer-live-p buffer))
                                      (window (get-buffer-window buffer t)))
                            (delete-window window))))
        (when (or (not (get-buffer-window buffer 'visible))
                  (not (frame-focus-state)))
          (knockknock-notify :title "Build Complete"
                             :message "Compilation successful!"
                             :icon "cod-check"
                             :duration 5))))))

(setq compilation-finish-functions
      (list #'ar/compile-autoclose-or-jump-first-error))

(defun not-split-window (orig-fn &rest args)
  "Let ORIG-FN not split window.
ARGS is ORIG-FN args."
  (let ((split-height-threshold nil)
        (split-width-threshold nil))
    (apply orig-fn args)))

(advice-add #'next-error-no-select :around #'not-split-window)
(advice-add #'previous-error-no-select :around #'not-split-window)
(advice-add #'compile-goto-error :around #'not-split-window)

;;; eat
(require 'init-eat)

;;; vterm
(defun project-run-command-with-vterm ()
  "Run COMMAND in vterm."
  (interactive)
  (let ((command (compilation-read-command compile-command)))
    (require 'multi-vterm)
    (multi-vterm-run command)))

;;; lisp
(add-hook 'before-save-hook
          #'(lambda ()
              (when (or (equal major-mode 'emacs-lisp-mode)
                        (equal major-mode 'lisp-mode)
                        (equal major-mode 'scheme-mode))
                (call-interactively #'check-parens))))
;;; latex

;; (require 'tex-site) is autoloaded by AUCTex tex-site.el
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-output-dir "Tmp")
;; (setq-default TeX-engine 'xetex)

(setq TeX-newline-function #'reindent-then-newline-and-indent)

(defun my/set-file-local-variable-value-with-file-path (local-variable-name)
  "Set LOCAL-VARIABLE-NAME as a file-local variable with a user-selected file value."
  (interactive "sLocal variable name: ")
  (if (boundp local-variable-name)
      (let* ((prompt (format "TeX %s file: " local-variable-name))
             (local-variable-value (read-file-name prompt nil nil t)))
        (add-file-local-variable local-variable-name
                                 (file-relative-name local-variable-value
                                                     (file-name-directory (buffer-file-name)))))
    (message "Symbol %s is not bound" local-variable-name)))

(defun my/set-file-local-variable-value-with-candidates (local-variable-name candidates-list)
  "Set LOCAL-VARIABLE-NAME as a file-local variable with a value selected from CANDIDATES-LIST."
  (interactive)
  (if (boundp local-variable-name)
      (let* ((prompt (format "TeX %s file: " local-variable-name))
             (local-variable-value (completing-read prompt candidates-list)))
        (add-file-local-variable local-variable-name (intern local-variable-value)))
    (message "Symbol %s is not bound" local-variable-name)))

(pretty-transient-define-prefix transient-latex-line-template ()
  "Transient latex line menu."
  [["RefTex"
    ("t" "citet" reftex-citet)
    ("p" "citep" reftex-citep)
    ("c" "citation" reftex-citation)
    ("r" "insert reference" consult-reftex-insert-reference)
    ("g" "goto-label" consult-reftex-goto-label)
    ("T" "toc" reftex-toc-recenter)]
   ["Misc"
    ("b" "environment" LaTeX-environment)
    ("v" "add-file-local-variable" add-file-local-variable)
    ("m" "set-local-Tex-master" (lambda ()
                                  (interactive)
                                  (my/set-file-local-variable-value-with-file-path 'TeX-master)
                                  (save-buffer)
                                  (revert-buffer nil t)))
    ("d" "set-local-TeX-output-dir" (lambda ()
                                      (interactive)
                                      (my/set-file-local-variable-value-with-file-path 'TeX-output-dir)
                                      (save-buffer)
                                      (revert-buffer nil t)))
    ("e" "set-local-Tex-engine" (lambda ()
                                  (interactive)
                                  (my/set-file-local-variable-value-with-candidates 'TeX-engine TeX-engine-alist-builtin)
                                  (save-buffer)
                                  (revert-buffer nil t)))
    (">" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-list-to-list 'TeX-view-program-selection
                              '((output-pdf "Evince")
                                (output-pdf "Sioyek")
                                (output-pdf "xdg-open")))))

(with-eval-after-load 'latex
  (keymap-sets LaTeX-mode-map
    '(("M-g r" . consult-reftex-goto-label)
      ("C-c C-l" . consult-reftex-insert-reference)
      (">" . transient-latex-line-template))))

(defun +cdlatex-complete ()
  "TAB complete."
  (interactive)
  (or (yas-expand)
      (cdlatex-tab)))

(with-eval-after-load 'cdlatex
  (keymap-sets cdlatex-mode-map
    '(("TAB" . +cdlatex-complete))))

(add-hook 'TeX-mode-hook
          (lambda ()
            (prettify-symbols-mode)
            (cdlatex-mode)
            (setq-local corfu-auto nil)))

;;; RefTex
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(setq reftex-cite-format 'natbib)

;;; language
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

(require 'init-elisp)

;; bash
(add-hook 'sh-mode-hook #'(lambda () (treesit-parser-create 'bash)))

;; json
(add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
(setq json-ts-mode-indent-offset 4)

;; markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setopt markdown-fontify-code-blocks-natively t)

;;; Local Variables

;; Local Variables:
;; eval: (when user/hidden-outline (outline-hide-sublevels 2))
;; End:

(provide 'init-program)
;;; init-program.el ends heres.
