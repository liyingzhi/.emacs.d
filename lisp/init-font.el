;;; init-font.el --- font                            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("MonoLisa Lucius" "Jetbrains Mono" "Source Code Pro" "PragmataPro Mono Liga"
                           "Aporetic Sans Mono" "Aporetic Sans" "Cascadia Code" "Fira Code"
                           "SF Mono" "Hack" "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        ;; :slant 'italic
                                        ;; :weight 'medium
                                        :height (cond (sys/macp user/font-mac-size)
                                                      (sys/win32p user/font-win-size)
                                                      (t user/font-linux-size))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai" "LXGW Neo Xihei" "WenQuanYi Zen Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-fontset-font t 'han (font-spec :family font))))))

;;; setup default font
(setup-fonts)
(add-hook 'window-setup-hook #'setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

(defun set-buffer-font (font face-name)
  "Set the current buffer's font to FONT using FACE-NAME.
If FONT is nil, use the default face entirely.
If FONT is a string, use it as the font family
while preserving other default attributes."
  (unless (facep face-name)
    (make-face face-name))
  (if (null font)
      ;;Inheritance default face
      (copy-face 'default face-name)
    ;;Replace fonts only, keeping other attributes unchanged.
    (progn
      (copy-face 'default face-name)
      (set-face-attribute face-name nil :font font)))
  (setq-local buffer-face-mode-face face-name)
  (buffer-face-mode))

(defun set-font-for-modes (font-alist)
  "Set fonts for different modes based on FONT-ALIST."
  (dolist (entry font-alist)
    (let ((mode (car entry))
          (font (cdr entry)))
      (add-hook (intern (format "%s-hook" mode))
                (lambda ()
                  (let ((face-name (intern (format "%s-font-face" mode))))
                    (set-buffer-font font face-name)))))))

;;; setup buffer-specified font
;; (defconst *fallback-fonts* '("Jigmo" "Jigmo2" "Jigmo3"))
;; (defconst *font-size* 15)
;; (defconst *default-font* (format "MonoLisa Lucius %d" *font-size*))
;; (defconst *org-font* (format "Aporetic Serif Mono %d" *font-size*))
;; (defconst *term-default-font* (format "Aporetic Serif Mono %d" *font-size*))
;; (defconst *prog-font* (format "Aporetic Serif Mono %d" *font-size*))
;; (defconst *zh-default-font* "LXGW WenKai")
;; (defconst *nerd-icons-font* "Symbols Nerd Font Mono")
;; (defconst *emoji-fonts* '("Apple Color Emoji"
;;                           "Noto Color Emoji"
;;                           "Noto Emoji"
;;                           "Segoe UI Emoji"))
;; (defconst *symbol-font* '("Apple Symbols"
;;                           "Segoe UI Symbol"
;;                           "Symbola"
;;                           "Symbol"))

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (set-font-for-modes
;;              `((vterm-mode . ,*term-default-font*)
;;                (nxml-mode  . ,*prog-font*)
;;                (org-mode   . ,*org-font*)
;;                (latex-mode . ,*prog-font*)
;;                (prog-mode  . ,*prog-font*)))))

(add-hook 'after-init-hook
          (lambda ()
            (set-font-for-modes
             `((vterm-mode . ,user/*term-default-font*)))))

(defun +suggest-other-faces (func &rest args)
  "Temporarily disable `global-hl-line-mode' while executing FUNC with ARGS."
  (let ((was-hl-line-mode-enabled global-hl-line-mode))
    (when was-hl-line-mode-enabled
      (global-hl-line-mode -1))
    (unwind-protect
        (apply func args)
      (when was-hl-line-mode-enabled
        (global-hl-line-mode 1)))))

(advice-add 'face-at-point :around #'+suggest-other-faces)


;;; 连体字体
(with-eval-after-load 'ligature
  (ligature-set-ligatures 't '("www"))
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 'prog-mode
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

(add-hook 'after-init-hook
          #'(lambda ()
              (when user/ligature
                (global-ligature-mode))))

;;; 替换符号
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'(lambda ()
               (require 'pretty-mode)
               (when user/pretty-mode
                 (pretty-mode 1))))

(provide 'init-font)
;;; init-font.el ends here
